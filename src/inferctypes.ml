module M   = Misc
module P   = Pretty
module C   = Cil
module Cs  = Constants
module E   = Errormsg
module ST  = Ssa_transform
module RA  = Refanno
module SM  = Misc.StringMap
module S   = Sloc
module SLM = Sloc.SlocMap
module SS  = Sloc.SlocSet
module CG  = Callgraph
module VM  = CilMisc.VarMap

open Ctypes
open M.Ops

(******************************************************************************)
(****************************** Index Constraints *****************************)
(******************************************************************************)

type indexvar = int

let d_indexvar () (iv: indexvar): P.doc =
  P.text <| "?i" ^ string_of_int iv

let (fresh_indexvar, reset_fresh_indexvars) = M.mk_int_factory ()

let with_fresh_indexvar (f: indexvar -> 'a): 'a =
  fresh_indexvar () |> f

type indexexp =
  | IEConst of index
  | IEVar of indexvar
  | IEPlus of indexexp * (* RHS scale: *) int * indexexp
  | IEMinus of indexexp * (* RHS scale: *) int * indexexp
  | IEMult of indexexp * indexexp
  | IEDiv of indexexp * indexexp

let rec d_indexexp (): indexexp -> P.doc = function
  | IEConst i                 -> d_index () i
  | IEVar iv                  -> d_indexvar () iv
  | IEPlus (iv1, scale, iv2)  -> P.dprintf "%a + %d * %a" d_indexexp iv1 scale d_indexexp iv2
  | IEMinus (iv1, scale, iv2) -> P.dprintf "%a - %d * %a" d_indexexp iv1 scale d_indexexp iv2
  | IEMult (iv1, iv2)         -> P.dprintf "%a * %a" d_indexexp iv1 d_indexexp iv2
  | IEDiv (iv1, iv2)          -> P.dprintf "%a / %a" d_indexexp iv1 d_indexexp iv2

type indexcstr =
  | ICVarLess of indexexp * indexvar
  | ICConstLess of indexexp * index

module IndexSol =
  Misc.MapWithDefault(struct
                        type t      = indexvar
                        let compare = compare

                        type v      = index
                        let default = IBot
                      end)

type indexsol = index IndexSol.t

let rec indexexp_apply (is: indexsol): indexexp -> index = function
  | IEConst i             -> i
  | IEVar iv              -> IndexSol.find iv is
  | IEPlus (ie1, x, ie2)  -> index_plus (indexexp_apply is ie1) (index_scale x <| indexexp_apply is ie2)
  | IEMinus (ie1, x, ie2) -> index_minus (indexexp_apply is ie1) (index_scale x <| indexexp_apply is ie2)
  | IEMult (ie1, ie2)     -> index_mult (indexexp_apply is ie1) (indexexp_apply is ie2)
  | IEDiv (ie1, ie2)      -> index_div (indexexp_apply is ie1) (indexexp_apply is ie2)

let refine_index (ie: indexexp) (iv: indexvar) (is: indexsol): indexsol =
  IndexSol.add iv (index_lub (indexexp_apply is ie) (IndexSol.find iv is)) is

let indexcstr_sat (ic: indexcstr) (is: indexsol): bool =
  match ic with
    | ICVarLess (ie, iv)  -> is_subindex (indexexp_apply is ie) (IndexSol.find iv is)
    | ICConstLess (ie, i) -> is_subindex (indexexp_apply is ie) i

(******************************************************************************)
(****************************** Type Constraints ******************************)
(******************************************************************************)

type ctypevar = indexexp prectype

let d_ctypevar: unit -> ctypevar -> P.doc =
  d_prectype d_indexexp

let fresh_ctvint (n: int): ctypevar =
  CTInt (n, IEVar (fresh_indexvar ()))

let fresh_ctvref (): ctypevar =
  CTRef (Sloc.fresh Sloc.Abstract, IEVar (fresh_indexvar ()))

let ctypevar_apply (is: indexsol): ctypevar -> ctype = function
  | CTInt (n, ie) -> CTInt (n, indexexp_apply is ie)
  | CTRef (s, ie) -> CTRef (s, indexexp_apply is ie)

type ctypecstr =
  | CTCSubtype of ctypevar * ctypevar
  | CTCDSubtype of ctypevar * ctypevar * (* bound: *) ctype * (* check: *) C.varinfo * FI.refctype

type dcheck = C.varinfo * FI.refctype

let d_dcheck () ((vi, rt): dcheck): P.doc =
  P.dprintf "%s :: %a" vi.C.vname FI.d_refctype rt

(* pmr: better error report *)
let refine_ctype (ctv1: ctypevar) (ctv2: ctypevar) (is: indexsol): indexsol =
  match (ctv1, ctv2) with
    | (CTInt (n1, ie1), CTInt (n2, IEVar (iv2))) when n1 = n2       -> refine_index ie1 iv2 is
    | (CTRef (s1, ie1), CTRef (s2, IEVar (iv2))) when Sloc.eq s1 s2 -> refine_index ie1 iv2 is
    | _                                                     -> raise (NoLUB (ctypevar_apply is ctv1, ctypevar_apply is ctv2))

let refine_dctype (ctv1: ctypevar) (ctv2: ctypevar) (cbound: ctype) (v: C.varinfo) (t: FI.refctype) (is: indexsol): indexsol =
  let (ct1, ct2) = (ctypevar_apply is ctv1, ctypevar_apply is ctv2) in
    if is_subctype ct1 cbound then
      refine_ctype ctv1 ctv2 is
    else
      match ctv2, cbound with
        | CTInt (n1, iv), CTInt (n2, i) when n1 = n2    -> IndexSol.add iv i is
        | CTRef (s1, iv), CTRef (s2, i) when S.eq s1 s2 -> IndexSol.add iv i is
        | CTRef (s1, _), CTRef (s2, _)                  -> raise (NoLUB (ctypevar_apply is ctv1, ctypevar_apply is ctv2))
        | _                                             -> assert false

let equalize_ctypes (ctv1: ctypevar) (ctv2: ctypevar) (is: indexsol): indexsol =
  refine_ctype ctv2 ctv1 (refine_ctype ctv1 ctv2 is)

let ctypecstr_sat (ctc: ctypecstr) (is: indexsol): bool =
  match ctc with
    | CTCSubtype (ctv1, ctv2)                -> is_subctype (ctypevar_apply is ctv1) (ctypevar_apply is ctv2)
    | CTCDSubtype (ctv1, ctv2, cbound, _, _) ->
        let (ct1, ct2) = (ctypevar_apply is ctv1, ctypevar_apply is ctv2) in
             (not (is_subctype ct1 cbound) && is_subctype ct2 cbound && is_subctype cbound ct2)
          || is_subctype ct1 ct2

(******************************************************************************)
(****************************** Store Constraints *****************************)
(******************************************************************************)

type heapvar = int

let (fresh_heapvar, reset_fresh_heapvars) = M.mk_int_factory ()

let d_heapvar () (hv: heapvar): P.doc =
  P.text <| "?h" ^ string_of_int hv

type heapmap = heapvar VM.t

type storecstr =
  | SCInc of S.t * indexvar * ctypevar (* (l, i, ct): (i, ct) in store(l) *)
  | SCMem of S.t                       (* l in dom(store) *)
  | SCUniq of S.t list                 (* for all l1, l2 in list, not S.eq l1 l2 *)

type storesol = indexexp prestore

let storesol_add (l: Sloc.t) (pl: ploc) (ctv: ctypevar) (ss: storesol): storesol =
  SLM.add l (LDesc.add pl ctv (prestore_find l ss)) ss

let storesol_apply (is: indexsol) (ss: storesol): store =
  SLM.map (LDesc.map <| ctypevar_apply is) ss

let refine_store (l: Sloc.t) (iv: indexvar) (ctv: ctypevar) (is: indexsol) (ss: storesol): indexsol * storesol =
  match IndexSol.find iv is with
    | IBot   -> (is, ss)
    | IInt n ->
        let pl = PLAt n in
          begin match LDesc.find pl (prestore_find l ss) with
            | []          -> (is, storesol_add l pl ctv ss)
            | [(_, ctv2)] -> (equalize_ctypes ctv ctv2 is, ss)
            | _           -> assert false
        end
    | ISeq (n, m) ->
        let (ld, is) = LDesc.shrink_period m equalize_ctypes is (prestore_find l ss) in
        let pl       = PLSeq n in
        let pcts     = LDesc.find pl ld in
        let is       = List.fold_left (fun is (_, ctv2) -> equalize_ctypes ctv ctv2 is) is pcts in
        let p        = ld |> LDesc.get_period |> Misc.get_option 0 in
          if List.exists (fun (pl2, _) -> ploc_contains pl2 pl p) pcts then
            (* If this sequence is included in an existing one, there's nothing left to do *)
            (is, ss)
          else
            (* Otherwise, remove "later", overlapping elements and add this sequence.
               (Note if there's no including sequence, all the elements we found previously
                come after this one.) *)
            let ld = List.fold_left (fun ld (pl2, _) -> LDesc.remove pl2 ld) ld pcts in
            let ld = LDesc.add pl ctv ld in
              (is, SLM.add l ld ss)
    | ITop ->
        let (ld, is) = LDesc.shrink_period (prectype_width ctv) equalize_ctypes is (prestore_find l ss) in
        let (ld, is) = LDesc.foldn (fun _ (ld, is) pl ctv2 -> (LDesc.remove pl ld, equalize_ctypes ctv ctv2 is)) (ld, is) ld in
          (is, SLM.add l (LDesc.add PLEverywhere ctv ld) ss)

let storeinc_sat ((l, iv, ctv): S.t * indexvar * ctypevar) (is: indexsol) (ss: storesol): bool =
  let ld = prestore_find l ss in
  let ct = ctypevar_apply is ctv in
    match IndexSol.find iv is with
      | IBot -> true
      | ITop ->
          begin match LDesc.find PLEverywhere ld with
            | [(PLEverywhere, ctv2)] -> prectype_eq ct (ctypevar_apply is ctv2)
            | _                      -> false
          end
      | IInt n ->
          begin match LDesc.find (PLAt n) ld with
            | [(_, ctv2)] -> prectype_eq ct (ctypevar_apply is ctv2)
            | _           -> false
          end
      | ISeq (n, m) ->
          match LDesc.get_period ld with
            | None   -> false
            | Some p ->
                (* This is stricter than the formal algorithm requires - we actually
                   enforce that a sequence is represented only by a sequence element,
                   rather than by several individual elements followed by a sequence *)
                let pl = PLSeq n in
                  match LDesc.find pl ld with
                    | [(pl2, ctv2)] -> m mod p = 0 && ploc_contains pl2 pl p && prectype_eq ct (ctypevar_apply is ctv2)
                    | _             -> false

let storecstr_sat (sc: storecstr) (is: indexsol) (ss: storesol): bool =
  match sc with
    | SCInc (l, iv, ctv) -> storeinc_sat (l, iv, ctv) is ss
    | SCMem l            -> SLM.mem l ss
    | SCUniq ls          -> not (M.exists_pair S.eq ls)

(******************************************************************************)
(****************************** Misc Junk to Move *****************************)
(******************************************************************************)

module IM = M.IntMap

type ctvenv = ctypevar VM.t

type ctvemap = ctypevar ExpMap.t

(* should change to a vmap or something *)
(* is this thing even useful? *)
type 'a funmap = ('a precfun * Ssa_transform.ssaCfgInfo) SM.t

type funenv = (indexexp precfun * heapvar) VM.t

type env = funenv * heapvar * ctvenv

(* consider replacing with the vars instead of exps; we really only care about
   function locals *)
type annotenv = (ctvemap * RA.block_annotation array) VM.t

(* consider replacing with the vars instead of exps; we really only care about
   function locals *)
type annotenv = (ctvemap * RA.block_annotation array) VM.t

(* somewhat messed-up; we don't need sto_in, sto_out... *)
type cfunvar = indexexp precfun

let d_cfunvar: unit -> cfunvar -> P.doc =
  d_precfun d_indexexp

(* pmr: needs to go elsewhere fo sho *)
type subst = (S.t * S.t) list

let d_subst () (sub: subst): P.doc =
  P.dprintf "[@[%a@]]" (P.d_list ", " (fun () (s1, s2) -> P.dprintf "%a -> %a" S.d_sloc s1 S.d_sloc s2)) sub

(* pmr: probably should compact substs when concatenating *)
(* pmr: this order is backwards from what I want *)
let subst_sloc (sub: subst) (s: S.t) =
  List.fold_right (fun (s1, s2) s -> if S.eq s1 s then s2 else s) sub s

type simplecstrdesc = [
| `CInLoc of indexexp * ctypevar * S.t
| `CSubtype of ctypevar * subst * ctypevar
| `CWFSubst of subst
]

type cstrdesc = [
| simplecstrdesc
| `CInHeap of S.t * heapvar
| `CSubheap of heapvar * subst * heapvar
]

let is_cstrdesc_simple: cstrdesc -> bool = function
  | `CInHeap _ | `CSubheap _ -> false
  | _                        -> true

let d_cstrdesc (): cstrdesc -> P.doc = function
  | `CSubtype (ctv1, sub, ctv2) -> P.dprintf "@[@[%a@] <: @[%a %a@]@]" d_ctypevar ctv1 d_subst sub d_ctypevar ctv2
  | `CInHeap (s, hv)            -> P.dprintf "%a ∈ %a" S.d_sloc s d_heapvar hv
  | `CInLoc (ie, ctv, s)        -> P.dprintf "(%a, %a) ∈ %a" d_indexexp ie d_ctypevar ctv S.d_sloc s
  | `CSubheap (hv1, sub, hv2)   -> P.dprintf "%a <: %a %a" d_heapvar hv1 d_subst sub d_heapvar hv2
  | `CWFSubst (sub)             -> P.dprintf "WF(%a)" d_subst sub

(* pmr: need a subst module... *)
(* pmr: compose substitutions more nicely (compression?) *)
(* pmr: ^ will be necessary for handling CWFSubst correctly! *)
let cstrdesc_subst (sub: subst): cstrdesc -> cstrdesc = function
  | `CSubtype (ctv1, sub2, ctv2) -> `CSubtype (prectype_subs sub ctv1, sub2 @ sub, ctv2)
  | `CInHeap (s, hv)             -> `CInHeap (subst_sloc sub s, hv)
  | `CInLoc (ie, ctv, s)         -> `CInLoc (ie, prectype_subs sub ctv, subst_sloc sub s)
  | `CSubheap (hv1, sub2, hv2)   -> `CSubheap (hv1, sub2 @ sub, hv2)
  | `CWFSubst sub2               -> `CWFSubst (sub2 @ sub)

type cstr = {cid: int; cdesc: cstrdesc; cloc: C.location}

let (fresh_cstrid, reset_fresh_cstrids) = M.mk_int_factory ()

let mk_cstr (loc: C.location) (cdesc: cstrdesc): cstr =
  {cid = fresh_cstrid (); cloc = loc; cdesc = cdesc}

let cstr_subst (sub: subst) (c: cstr): cstr =
  mk_cstr c.cloc (cstrdesc_subst sub c.cdesc)

let d_cstr () ({cid = cid; cdesc = cdesc; cloc = loc}: cstr): P.doc =
  P.dprintf "%a: %a" C.d_loc loc d_cstrdesc cdesc

let is_cstr_simple (c: cstr): bool =
  is_cstrdesc_simple c.cdesc

type cstremap = ctvemap * cstr list

type shape =
  {vtyps : (Cil.varinfo * Ctypes.ctype) list;
   etypm : Ctypes.ctemap;
   store : Ctypes.store;
   anna  : RA.block_annotation array;
   theta : RA.ctab }

(******************************************************************************)
(*************************** Systems of Constraints ***************************)
(******************************************************************************)

(*

let mk_ivarless (loc: C.location) (ie: indexexp) (iv: indexvar) =
  {cdesc = CSIndex (ICVarLess (ie, iv)); cloc = loc}

let mk_iconstless (loc: C.location) (ie: indexexp) (i: index) =
  {cdesc = CSIndex (ICConstLess (ie, i)); cloc = loc}

let mk_subty (loc: C.location) (ctv1: ctypevar) (ctv2: ctypevar) =
  {cdesc = CSCType (CTCSubtype (ctv1, ctv2)); cloc = loc}

let mk_dsubty (loc: C.location) (ctv1: ctypevar) (ctv2: ctypevar) (cbound: ctype) (v: C.varinfo) (t: FI.refctype) =
  {cdesc = CSCType (CTCDSubtype (ctv1, ctv2, cbound, v, t)); cloc = loc}

let mk_const_subty (loc: C.location) (ctv: ctypevar): ctype -> cstr list = function
  | CTInt (n, i) ->
      with_fresh_indexvar <| fun iv ->
        [mk_subty loc ctv (CTInt (n, iv)); mk_iconstless loc (IEVar iv) i]
  | CTRef (s, i) ->
      with_fresh_indexvar <| fun iv ->
        [mk_subty loc ctv (CTRef (s, iv)); mk_iconstless loc (IEVar iv) i]

let mk_storeinc (loc: C.location) (l: Sloc.t) (iv: indexvar) (ctv: ctypevar) =
  {cdesc = CSStore (SCInc (l, iv, ctv)); cloc = loc}

let mk_storemem (loc: C.location) (l: Sloc.t) =
  {cdesc = CSStore (SCMem l); cloc = loc}

let mk_indexvar_eq_index (loc: C.location) (iv: indexvar) (i: index): cstr list =
  [mk_ivarless loc (IEConst i) iv; mk_iconstless loc (IEVar iv) i]

let ctypevar_of_ctype (loc: C.location): ctype -> ctypevar * cstr list = function
  | CTInt (n, i) ->
      with_fresh_indexvar <| fun iv ->
        let ctv = CTInt (n, iv) in
          (ctv, mk_indexvar_eq_index loc iv i)
  | CTRef (s, i) ->
      with_fresh_indexvar <| fun iv ->
        let ctv = CTRef (s, iv) in
          (ctv, mk_indexvar_eq_index loc iv i)

let mk_const_storeinc (loc: C.location) (l: Sloc.t) (i: index) (ct: ctype): cstr list =
  with_fresh_indexvar <| fun iv ->
    let (ctv, cs) = ctypevar_of_ctype loc ct in
      cs @ [mk_ivarless loc (IEConst i) iv; mk_storeinc loc l iv ctv]

let mk_uniqlocs (loc: C.location) (ls: S.t list) =
  {cdesc = CSStore (SCUniq ls); cloc = loc}

type cstrsol = indexsol * storesol

let cstrdesc_sat ((is, ss): cstrsol): cstrdesc -> bool = function
  | CSIndex ic  -> indexcstr_sat ic is
  | CSCType ctc -> ctypecstr_sat ctc is
  | CSStore sc  -> storecstr_sat sc is ss

let cstr_sat (csol: cstrsol) (c: cstr): bool =
  cstrdesc_sat csol c.cdesc

let refine (loc: C.location) ((is, ss): cstrsol): cstrdesc -> indexsol * storesol = function
  | CSIndex (ICVarLess (ie, iv))      -> (refine_index ie iv is, ss)
  | CSIndex (ICConstLess (ie, i))     -> E.s <| C.errorLoc loc "Index constraint violation: %a <= %a@!@!" d_index (indexexp_apply is ie) d_index i
  | CSCType (CTCSubtype (ctv1, ctv2)) -> (refine_ctype ctv1 ctv2 is, ss)
  | CSCType (CTCDSubtype (ctv1, ctv2, cbound, v, t)) -> (refine_dctype ctv1 ctv2 cbound v t is, ss)
  | CSStore (SCUniq _)                -> failwith "Cannot refine store uniqueness constraint!\n\n"
  | CSStore (SCMem l)                 -> if SLM.mem l ss then (is, ss) else (is, SLM.add l LDesc.empty ss)
  | CSStore (SCInc (l, iv, ctv))      ->
      try
        refine_store l iv ctv is ss
      with TypeDoesntFit ->
        let i  = IndexSol.find iv is in
        let ct = ctypevar_apply is ctv in
        let ld = LDesc.map (ctypevar_apply is) (prestore_find l ss) in
          E.s <| C.errorLoc loc "Can't fit %a |-> %a in location %a: @!@!%a@!@!"
              d_index i d_ctype ct Sloc.d_sloc l (LDesc.d_ldesc d_ctype) ld

let rec solve_rec (cs: cstr list) ((is, ss) as csol: cstrsol): cstrsol =
  match (try Some (List.find (fun c -> not (cstr_sat csol c)) cs) with Not_found -> None) with
    | None   -> csol
    | Some c ->
        let (cs, is, ss) =
          try
            let (is, ss) = refine c.cloc (is, ss) c.cdesc in
              (cs, is, ss)
          with
            | NoLUB (CTRef (s1, _), CTRef (s2, _)) ->
                let ss = ss |> SLM.remove s1 |> SLM.remove s2 in
                let _  = S.unify s1 s2 in
                  (cs, is, ss)
            | NoLUB (ctv1, ctv2) -> E.s <| Cil.errorLoc c.cloc "Incompatible types: %a, %a@!@!" d_ctype ctv1 d_ctype ctv2
            | _                  -> E.s <| Cil.errorLoc c.cloc "Unknown error"
        in solve_rec cs (is, ss)

let solve (cs: cstr list): cstrsol =
  (* Defer checking uniqueness constraints until the very end *)
  let (cs, uniqcs) = List.partition (function {cdesc = CSStore (SCUniq _)} -> false | _ -> true) cs in
  let csol         = solve_rec cs (IndexSol.empty, SLM.empty) in
   match (try Some (List.find (fun c -> not (cstr_sat csol c)) uniqcs) with Not_found -> None) with
     | None                                           -> csol
     | Some {cdesc = CSStore (SCUniq ls); cloc = loc} -> M.find_pair S.eq ls |> fun (l, _) -> E.s <| C.errorLoc loc "Parameter location %a aliased to another parameter location@!@!" S.d_sloc l
     | _                                              -> failwith "Severe constraint weirdness"
*)
(******************************************************************************)
(***************************** CIL Types to CTypes ****************************)
(******************************************************************************)

let int_width    = C.bytesSizeOfInt C.IInt
let short_width  = C.bytesSizeOfInt C.IShort
let char_width   = C.bytesSizeOfInt C.IChar

let rec typ_width (t: C.typ): int =
  match C.unrollType t with
    | C.TInt (ik, _)                    -> C.bytesSizeOfInt ik
    | C.TPtr _                          -> typ_width !C.upointType
    | C.TComp (ci, _) when ci.C.cstruct -> List.fold_left (fun w fi -> w + typ_width fi.C.ftype) 0 ci.C.cfields
    | t                                 -> E.s <| E.bug "Unimplemented typ_width: %a@!@!" C.d_type t

let fresh_ctypevar (t: C.typ): ctypevar =
  match C.unrollType t with
    | C.TInt (ik, _)        -> fresh_ctvint (C.bytesSizeOfInt ik)
    | C.TVoid _             -> fresh_ctvint (0)
    | C.TPtr _ | C.TArray _ -> fresh_ctvref ()
    | _                     -> E.s <| E.bug "Unimplemented fresh_ctypevar: %a@!@!" C.d_type t

(******************************************************************************)
(******************************* Shape Solutions ******************************)
(******************************************************************************)
(*

let d_vartypes () vars =
  P.docList ~sep:(P.dprintf "@!") (fun (v, ct) -> P.dprintf "%s: %a" v.C.vname Ctypes.d_ctype ct) () vars

let print_shape (fname: string) (cf: cfun) ({vtyps = locals; store = st}: shape): unit =
  let _ = P.printf "%s@!" fname in
  let _ = P.printf "============@!@!" in
  let _ = P.printf "Signature:@!" in
  let _ = P.printf "----------@!@!" in
  let _ = P.printf "%a@!@!" d_cfun cf in
  let _ = P.printf "Locals:@!" in
  let _ = P.printf "-------@!@!" in
  let _ = P.printf "%a@!@!" d_vartypes locals in
  let _ = P.printf "Store:@!" in
  let _ = P.printf "------@!@!" in
  let _ = P.printf "%a@!@!" d_store st in
  let _ = P.printf "Deferred Checks:@!" in
  let _ = P.printf "------@!@!" in
  let _ = P.printf "%a@!@!" (P.d_list "\n" d_dcheck) ds in
    ()
*)
(******************************************************************************)
(**************************** Constraint Generation ***************************)
(******************************************************************************)
(*

and constrain_constptr (em: cstremap) (loc: C.location): C.constant -> ctypevar * cstremap * cstr list = function
  | C.CStr _ ->
      let s = Sloc.fresh Sloc.Abstract in
        with_fresh_indexvar <| fun ivr -> with_fresh_indexvar <| fun ivl -> with_fresh_indexvar begin fun ivc ->
          (CTRef (s, ivr), em, [mk_ivarless loc (IEConst (IInt 0)) ivr;
                                mk_ivarless loc (IEConst (ISeq (0, 1))) ivl;
                                mk_ivarless loc (IEConst ITop) ivc;
                                mk_storeinc loc s ivl (CTInt (char_width, ivc))])
      end
  | C.CInt64 (v, ik, so) when v = Int64.zero -> (fresh_ctvref (), em, [])
  | c                                        -> E.s <| C.errorLoc loc "Cannot cast non-zero, non-string constant %a to pointer@!@!" C.d_const c

and constrain_addrof (ve: ctvenv) (em: cstremap) (loc: C.location): C.lval -> ctypevar * cstremap * cstr list = function
  | (C.Var f, C.NoOffset) when C.isFunctionType <| C.unrollType f.C.vtype ->
      (fresh_ctvref (), em, [])
  | lv -> E.s <| E.error "Don't know how to take address of %a@!@!" C.d_lval lv

let instantiate_args (loc: C.location) (argcts: (string * ctype) list): ctypevar list * cstr list =
  let (argctvs, argcts) = argcts |> List.map (M.compose (ctypevar_of_ctype loc) snd) |> List.split in
    (argctvs, List.flatten argcts)

let instantiate_ret (loc: C.location): ctype option -> ctypevar option * cstr list = function
  | Some rct ->
      let (ctv, cs) = ctypevar_of_ctype loc rct in
        (Some ctv, cs)
  | None -> (None, [])

let instantiate_store (loc: C.location) (st: store): cstr list =
     prestore_fold (fun css l i ct -> mk_const_storeinc loc l i ct :: css) [] st
  |> List.concat
  |> SLM.fold (fun l _ css -> mk_storemem loc l :: css) st

let lookup_function (loc: C.location) (env: ctypeenv) (f: string): cfun * (Sloc.t * Sloc.t) list =
  try
    M.StringMap.find f env |> cfun_instantiate
  with Not_found ->
    E.s <| C.errorLoc loc "Couldn't find spec for function %s@!@!" f

let instantiate_function (loc: C.location) (env: ctypeenv) (f: string): ctypevar * ctypevar list * cstr list * RA.annotation list =
  let ({args = argcts; ret = rct; sto_out = sout}, subs) = lookup_function loc env f in
  let (argctvs, argcs) = instantiate_args loc argcts in
  let (rctv, rctcs)    = ctypevar_of_ctype loc rct in
  let sublocs          = List.map snd subs in
  let storecs          = mk_uniqlocs loc sublocs :: instantiate_store loc sout in
    (rctv, argctvs, List.concat [storecs; rctcs; argcs], List.map (fun (s1, s2) -> RA.New (s1, s2)) subs)


exception Unified

let match_slocs (ct1: ctype) (ct2: ctype): unit =
  match (ct1, ct2) with
    | (CTRef (s1, _), CTRef (s2, _)) when not (S.eq s1 s2) -> S.unify s1 s2; raise Unified
    | _                                                    -> ()

let check_expected_type (loc: C.location) (etyp: ctype) (atyp: ctype): bool =
  match_slocs atyp etyp;
  if is_subctype atyp etyp then
    true
  else begin
    C.errorLoc loc "Expected type %a, but got type %a\n\n" d_ctype etyp d_ctype atyp |> ignore;
    false
  end

let check_out_store_complete (loc: C.location) (sto_out_formal: store) (sto_out_actual: store): bool =
  prestore_fold begin fun ok l i ct ->
    if SLM.mem l sto_out_formal && prestore_find_index l i sto_out_formal = [] then begin
      C.errorLoc loc "Actual store has binding %a |-> %a: %a, missing from spec for %a\n\n" S.d_sloc l d_index i d_ctype ct S.d_sloc l |> ignore;
      false
    end else
      ok
  end true sto_out_actual

let check_out_store (loc: C.location) (sto_out_formal: store) (sto_out_actual: store): bool =
  check_out_store_complete loc sto_out_formal sto_out_actual &&
    prestore_fold begin fun ok l i ft ->
      try
        match prestore_find_index l i sto_out_actual with
          | []   -> ok
          | [at] -> check_expected_type loc ft at && ok (* order is important here for unification! *)
          | _    -> failwith "Returned too many at index from prestore_find_index"
      with
        | Unified   -> raise Unified
        | Not_found -> ok
        | _         -> false
    end true sto_out_formal

let rec solve_and_check_rec (loc: C.location) (cf: cfun) (cs: cstr list): cstrsol =
  let (is, ss)  = solve cs in
  let out_store = storesol_apply is ss in
    try
      if check_out_store loc cf.sto_out out_store then
        (is, ss)
      else
        E.s <| C.errorLoc loc "Failed checking store typing:\nStore:\n%a\n\ndoesn't match expected type:\n\n%a\n\n" d_store out_store d_cfun cf
    with Unified -> solve_and_check_rec loc cf cs

let solve_and_check (loc: C.location) (cf: cfun) (cs: cstr list): cstrsol =
  mk_uniqlocs loc (precfun_slocset cf |> S.SlocSet.elements) :: cs |> solve_and_check_rec loc cf

let get_cstr_dcheck (is: indexsol): cstr -> dcheck option = function
  | {cdesc = CSCType (CTCDSubtype (ctv1, _, cbound, vi, rt))} when not (is_subctype (ctypevar_apply is ctv1) cbound) -> Some (vi, rt)
  | _                                                                                                                -> None

let infer_shape (env: ctypeenv) ({args = argcts; ret = rt; sto_in = sin} as cf: cfun) ({ST.fdec = fd; ST.phis = phis; ST.cfg = cfg}: ST.ssaCfgInfo): shape * dcheck list =
  let loc                      = fd.C.svar.C.vdecl in
  let (formals, formalcs)      = instantiate_args loc argcts in
  let bodyformals              = fresh_vars fd.C.sformals in
  let bodyformalcs             = Misc.map2 (fun f (_, bf) -> mk_subty loc f bf) formals bodyformals in
  let locals                   = fresh_vars fd.C.slocals in
  let vars                     = locals @ bodyformals |> List.fold_left (fun ve (v, ctv) -> IM.add v.C.vid ctv ve) IM.empty in
  let phics                    = mk_phis_cs vars phis in
  let storecs                  = instantiate_store loc sin in
  let ((ctvm, bodycs), annots) = constrain_cfg env vars rt cfg in
  let cs                       = List.concat [formalcs; bodyformalcs; phics; storecs; bodycs] in
  let (is, ss)                 = solve_and_check loc cf cs in
  let ds                       = M.map_partial (get_cstr_dcheck is) cs in
  let apply_sol                = ctypevar_apply is in
  let etypm                    = ExpMap.map apply_sol ctvm in
  let anna, theta              = RA.annotate_cfg cfg etypm annots in
  let shp                      =
    {vtyps = List.map (fun (v, ctv) -> (v, apply_sol ctv)) locals;
     etypm = etypm;
     store = storesol_apply is ss;
     anna  = anna;
     theta = theta }
  in
    if !Cs.verbose_level >= Cs.ol_ctypes || !Cs.ctypes_only then print_shape fd.C.svar.C.vname cf shp;
    shp
*)

let mk_subty (loc: C.location) (ctv1: ctypevar) (sub: subst) (ctv2: ctypevar): cstr =
  mk_cstr loc (`CSubtype (ctv1, sub, ctv2))

let mk_heapinc (loc: C.location) (s: S.t) (hv: heapvar): cstr =
  mk_cstr loc (`CInHeap (s, hv))

let mk_locinc (loc: C.location) (ie: indexexp) (ctv: ctypevar) (s: S.t): cstr =
  mk_cstr loc (`CInLoc (ie, ctv, s))

let mk_subheap (loc: C.location) (hv1: heapvar) (sub: subst) (hv2: heapvar): cstr =
  mk_cstr loc (`CSubheap (hv1, sub, hv2))

let mk_wfsubst (loc: C.location) (sub: subst): cstr =
  mk_cstr loc (`CWFSubst sub)

let ctypevar_of_const: C.constant -> ctypevar = function
  | C.CInt64 (v, ik, _) -> CTInt (C.bytesSizeOfInt ik, IEConst (index_of_int (Int64.to_int v)))
  | C.CChr c            -> CTInt (int_width, IEConst (IInt (Char.code c)))
  | c                   -> E.s <| E.bug "Unimplemented constrain_const: %a@!@!" C.d_const c

let rec constrain_exp_aux (env: env) (ctem: ctvemap) (loc: C.location): C.exp -> ctypevar * cstr list * S.t list * ctvemap = function
  | C.Const c                     -> let ctv = ctypevar_of_const c in (ctv, [], [], ctem)
  | C.Lval lv | C.StartOf lv      -> constrain_lval env ctem loc lv
  | C.UnOp (uop, e, t)            -> constrain_unop uop env ctem loc t e
  | C.BinOp (bop, e1, e2, t)      -> constrain_binop bop env ctem loc t e1 e2
  | C.CastE (C.TPtr _, C.Const c) -> constrain_constptr ctem loc c
  | C.CastE (ct, e)               -> constrain_cast env ctem loc ct e
  | C.SizeOf t                    -> constrain_sizeof ctem loc t
  | e                             -> E.s <| E.error "Unimplemented constrain_exp_aux: %a@!@!" C.d_exp e

and constrain_lval_aux ((_, hv, ve) as env: env) (ctem: ctvemap) (loc: C.location): C.lval -> ctypevar * cstr list * S.t list * ctvemap = function
  | (C.Var v, C.NoOffset)       -> (VM.find v ve, [], [], ctem)
  | (C.Mem e, C.NoOffset) as lv ->
      let (ctv, cs, ss, ctem) = constrain_exp env ctem loc e in
        begin match ctv with
          | CTRef (s, ie) ->
              let ctvlv = fresh_ctypevar <| C.typeOfLval lv in
              let cs    = mk_locinc loc ie ctvlv s :: cs in
                (ctvlv, cs, M.maybe_cons (prectype_sloc ctvlv) (s :: ss), ctem)
          | _ -> E.s <| E.bug "fresh_ctvref gave back non-ref type in constrain_lval@!@!"
        end
  | lv -> E.s <| E.bug "constrain_lval got lval with offset: %a@!@!" C.d_lval lv

and constrain_lval (env: env) (ctem: ctvemap) (loc: C.location) (lv: C.lval): ctypevar * cstr list * S.t list * ctvemap =
  let (ctv, cs, ss, ctem) = constrain_lval_aux env ctem loc lv in
    (ctv, cs, ss, ExpMap.add (C.Lval lv) ctv ctem)

and constrain_unop (op: C.unop) (env: env) (ctem: ctvemap) (loc: C.location) (t: C.typ) (e: C.exp): ctypevar * cstr list * S.t list * ctvemap =
  let (ctv, cs, ss, ctem) = constrain_exp env ctem loc e in
    match ctv with
      | CTInt _ -> (apply_unop t op, cs, ss, ctem)
      | _       -> E.s <| E.unimp "Haven't considered how to apply unops to references@!"

and apply_unop (rt: C.typ): C.unop -> ctypevar = function
  | C.LNot -> CTInt (typ_width rt, IEConst (ISeq (0, 1)))
  | C.BNot -> CTInt (typ_width rt, IEConst ITop)
  | C.Neg  -> CTInt (typ_width rt, IEConst ITop)

and constrain_binop (op: C.binop) (env: env) (ctem: ctvemap) (loc: C.location) (t: C.typ) (e1: C.exp) (e2: C.exp): ctypevar * cstr list * S.t list * ctvemap =
  let (ctv1, cs1, ss1, ctem) = constrain_exp env ctem loc e1 in
  let (ctv2, cs2, ss2, ctem) = constrain_exp env ctem loc e2 in
  let ctv                    = apply_binop op t ctv1 ctv2 in
    (ctv, List.concat [cs1; cs2], List.concat [ss1; ss2], ctem)

and apply_binop: C.binop -> C.typ -> ctypevar -> ctypevar -> ctypevar = function
  | C.PlusA                                 -> apply_arithmetic (fun ie1 ie2 -> IEPlus (ie1, 1, ie2))
  | C.MinusA                                -> apply_arithmetic (fun ie1 ie2 -> IEMinus (ie1, 1, ie2))
  | C.Mult                                  -> apply_arithmetic (fun ie1 ie2 -> IEMult (ie1, ie2))
  | C.Div                                   -> apply_arithmetic (fun ie1 ie2 -> IEDiv (ie1, ie2))
  | C.PlusPI | C.IndexPI                    -> apply_ptrarithmetic (fun ie1 x ie2 -> IEPlus (ie1, x, ie2))
  | C.MinusPI                               -> apply_ptrarithmetic (fun ie1 x ie2 -> IEMinus (ie1, x, ie2))
  | C.MinusPP                               -> apply_ptrminus
  | C.Lt | C.Gt | C.Le | C.Ge | C.Eq | C.Ne -> apply_rel
  | C.Mod                                   -> apply_unknown
  | C.BAnd | C.BOr | C.BXor                 -> apply_unknown
  | C.Shiftlt | C.Shiftrt                   -> apply_unknown
  | bop                                     -> E.s <| E.bug "Unimplemented apply_binop: %a@!@!" C.d_binop bop

and apply_arithmetic (f: indexexp -> indexexp -> indexexp) (rt: C.typ) (ctv1: ctypevar) (ctv2: ctypevar): ctypevar =
  match (ctv1, ctv2) with
    | (CTInt (n1, ie1), CTInt (n2, ie2)) -> CTInt (typ_width rt, f ie1 ie2)
    | _                                  -> E.s <| E.bug "Type mismatch in apply_arithmetic@!@!"

and apply_ptrarithmetic (f: indexexp -> int -> indexexp -> indexexp) (pt: C.typ) (ctv1: ctypevar) (ctv2: ctypevar): ctypevar =
  match (C.unrollType pt, ctv1, ctv2) with
    | (C.TPtr (t, _), CTRef (s, ie1), CTInt (n, ie2)) when n = int_width -> CTRef (s, f ie1 (typ_width t) ie2)
    | _                                                                  -> E.s <| E.bug "Type mismatch in constrain_ptrarithmetic@!@!"

and apply_ptrminus (pt: C.typ) (_: ctypevar) (_: ctypevar): ctypevar =
  CTInt (typ_width !C.upointType, IEConst ITop)

and apply_rel (_: C.typ) (_: ctypevar) (_: ctypevar): ctypevar =
  CTInt (int_width, IEConst (ISeq (0, 1)))

and apply_unknown (rt: C.typ) (_: ctypevar) (_: ctypevar): ctypevar =
  CTInt (typ_width rt, IEConst ITop)

and constrain_exp (env: env) (ctem: ctvemap) (loc: C.location) (e: C.exp): ctypevar * cstr list * S.t list * ctvemap =
  let (ctv, cs, ss, ctem) = constrain_exp_aux env ctem loc e in
    (ctv, cs, ss, ExpMap.add e ctv ctem)

and constrain_constptr (ctem: ctvemap) (loc: C.location): C.constant -> ctypevar * cstr list * S.t list * ctvemap = function
  | C.CStr _                                 -> E.s <| E.unimp "Haven't implemented string constants yet"
  | C.CInt64 (v, ik, so) when v = Int64.zero -> let s = S.fresh S.Abstract in (CTRef (s, IEConst IBot), [], [s], ctem)
  | c                                        -> E.s <| C.errorLoc loc "Cannot cast non-zero, non-string constant %a to pointer@!@!" C.d_const c

and constrain_cast (env: env) (ctem: ctvemap) (loc: C.location) (ct: C.typ) (e: C.exp): ctypevar * cstr list * S.t list * ctvemap =
  match (C.unrollType ct, C.unrollType <| C.typeOf e) with
    | (C.TInt (ik, _), C.TPtr _) -> (CTInt (C.bytesSizeOfInt ik, IEConst ITop), [], [], ctem)
    | (C.TInt (ik, _), C.TInt _) ->
        begin match constrain_exp_aux env ctem loc e with
          | (CTInt (n, ie), cs, ss, ctem) ->
              let iec =
                if n <= C.bytesSizeOfInt ik then
                  (* pmr: what about the sign bit?  this may not always be safe *)
                  ie
                else if not !Constants.safe then begin
                  C.warnLoc loc "Unsoundly assuming cast is lossless@!@!" |> ignore;
                  ie
                end else
                  IEConst ITop
              in (CTInt (C.bytesSizeOfInt ik, iec), cs, ss, ctem)
          | _ -> E.s <| C.errorLoc loc "Got bogus type in contraining int-int cast@!@!"
        end
    | _ -> constrain_exp_aux env ctem loc e

and constrain_sizeof (ctem: ctvemap) (loc: C.location) (t: C.typ): ctypevar * cstr list * S.t list * ctvemap =
  (CTInt (int_width, IEConst (IInt (C.bitsSizeOf t / 8))), [], [], ctem)

let constrain_return (env: env) (ctem: ctvemap) (rtv: ctypevar) (loc: C.location): C.exp option -> cstr list * S.t list * ctvemap * RA.block_annotation = function
    | None -> if is_void rtv then ([], [], ctem, []) else (C.errorLoc loc "Returning void value for non-void function\n\n" |> ignore; assert false)
    | Some e ->
        let (ctv, cs, ss, ctem) = constrain_exp env ctem loc e in
          (mk_subty loc ctv [] rtv :: cs, ss, ctem, [])

let constrain_arg (env: env) (loc: C.location) (e: C.exp) ((ctvs, css, sss, ctem): ctypevar list * cstr list list * S.t list list * ctvemap): ctypevar list * cstr list list * S.t list list * ctvemap =
  let (ctv, cs, ss, ctem) = constrain_exp env ctem loc e in
    (ctv :: ctvs, cs :: css, ss :: sss, ctem)

let constrain_args (env: env) (ctem: ctvemap) (loc: C.location) (es: C.exp list): ctypevar list * cstr list list * S.t list list * ctvemap =
  let (ctvs, css, sss, ctem) = List.fold_right (constrain_arg env loc) es ([], [], [], ctem) in
    (ctvs, css, sss, ctem)

let constrain_app ((fs, hv, _) as env: env) (ctem: ctvemap) (loc: C.location) (f: C.varinfo) (lvo: C.lval option) (args: C.exp list): cstr list list * S.t list list * ctvemap * RA.annotation list =
  let ctvs, css, sss, ctem = constrain_args env ctem loc args in
  let (ftv, fhv)           = VM.find f fs in
  let instslocs            = List.map (fun _ -> S.fresh S.Abstract) ftv.qlocs in
  let sub                  = List.combine ftv.qlocs instslocs in
  let sss                  = instslocs :: sss in
  let ctvfs                = List.map snd ftv.args in
  (* pmr: can probably eliminate some constraints by substituting with actual parameter locations directly when possible *)
  let css                  = [mk_subheap loc hv sub fhv; mk_wfsubst loc sub] ::
                               List.map2 (fun ctva ctvf -> mk_subty loc ctva sub ctvf) ctvs ctvfs ::
                               css in
    match lvo with
      | None    -> (css, sss, ctem, [])
      | Some lv ->
          let ctvlv, cs2, ss2, ctem = constrain_lval env ctem loc lv in
            ((mk_subty loc ftv.ret sub ctvlv :: cs2) :: css, ss2 :: sss, ctem, [])

let printf_funs = ["printf"; "fprintf"]

let constrain_instr_aux (env: env) (ctem: ctvemap) ((ctem, css, sss, bas): ctvemap * cstr list list * S.t list list * RA.block_annotation): C.instr -> ctvemap * cstr list list * S.t list list * RA.block_annotation = function
  | C.Set (lv, e, loc) ->
      let ctv1, cs1, ss1, ctem = constrain_lval env ctem loc lv in
      let ctv2, cs2, ss2, ctem = constrain_exp env ctem loc e in
        (ctem, (mk_subty loc ctv2 [] ctv1 :: cs1) :: cs2 :: css, ss1 :: ss2 :: sss, [] :: bas)
  | C.Call (None, C.Lval (C.Var {C.vname = f}, C.NoOffset), args, loc) when List.mem f printf_funs ->
      if not !Constants.safe then C.warnLoc loc "Unsoundly ignoring printf-style call to %s@!@!" f |> ignore else E.s <| C.errorLoc loc "Can't handle printf";
      let _, css, sss, ctem = constrain_args env ctem loc args in
        (ctem, css, sss, [] :: bas)
  | C.Call (lvo, C.Lval (C.Var f, C.NoOffset), args, loc) ->
      let css, sss, ctem, ba = constrain_app env ctem loc f lvo args in
        (ctem, css, sss, ba :: bas)
  | i -> E.s <| E.bug "Unimplemented constrain_instr: %a@!@!" C.dn_instr i

let constrain_instr (env: env) (ctem: ctvemap) (is: C.instr list): cstr list * S.t list * ctvemap * RA.block_annotation =
  let (ctem, css, sss, bas) = List.fold_left (constrain_instr_aux env ctem) (ctem, [], [], []) is in
    (List.concat css, List.concat sss, ctem, List.rev ([]::bas))

let constrain_stmt (env: env) (ctem: ctvemap) (rtv: ctypevar) (s: C.stmt): cstr list * S.t list * ctvemap * RA.block_annotation =
  match s.C.skind with
    | C.Instr is             -> constrain_instr env ctem is
    | C.If (e, _, _, loc)    -> let (_, cs, ss, ctem) = constrain_exp env ctem loc e in (cs, ss, ctem, []) (* we'll visit the subblocks later *)
    | C.Break _              -> ([], [], ctem, [])
    | C.Continue _           -> ([], [], ctem, [])
    | C.Goto _               -> ([], [], ctem, [])
    | C.Block _              -> ([], [], ctem, [])                              (* we'll visit this later as we iterate through blocks *)
    | C.Loop (_, _, _, _)    -> ([], [], ctem, [])                              (* ditto *)
    | C.Return (rexp, loc)   -> constrain_return env ctem rtv loc rexp
    | _                      -> E.s <| E.bug "Unimplemented constrain_stmt: %a@!@!" C.dn_stmt s

let maybe_fresh (v: C.varinfo): (C.varinfo * ctypevar) option =
  let t = C.unrollType v.C.vtype in
  match t with
  | C.TInt _
  | C.TPtr _
  | C.TArray _ -> Some (v, fresh_ctypevar t)
  | _          -> let _ = if !Constants.safe then E.error "not freshing local %s" v.C.vname in
                    C.warnLoc v.C.vdecl "Not freshing local %s of tricky type %a@!@!" v.C.vname C.d_type t
                    |> ignore; None

let fresh_vars (vs: C.varinfo list): (C.varinfo * ctypevar) list =
  Misc.map_partial maybe_fresh vs

let constrain_phi_defs (ve: ctvenv) ((vphi, vdefs): C.varinfo * (int * C.varinfo) list): cstr list =
  List.map (fun (_, vdef) -> mk_subty vphi.C.vdecl (VM.find vdef ve) [] (VM.find vphi ve)) vdefs

let constrain_phis (ve: ctvenv) (phis: (C.varinfo * (int * C.varinfo) list) list array): cstr list =
  Array.to_list phis |> List.flatten |> List.map (constrain_phi_defs ve) |> List.concat

let constrain_fun (fs: funenv) (hv: heapvar) (ftv: cfunvar) ({ST.fdec = fd; ST.phis = phis; ST.cfg = cfg}: ST.ssaCfgInfo): ctvemap * RA.block_annotation array * S.t list * cstr list =
  let blocks           = cfg.Ssa.blocks in
  let bas              = Array.make (Array.length blocks) [] in
  let bodyformals      = fresh_vars fd.C.sformals in
  let locals           = fresh_vars fd.C.slocals in
  let vars             = locals @ bodyformals in
  let ve               = List.fold_left (fun ve (v, ctv) -> VM.add v ctv ve) VM.empty vars in
  let loc              = fd.C.svar.C.vdecl in
  let formalcs         = List.map (fun (v, ctv) -> mk_subty loc (List.assoc v.C.vname ftv.args) [] ctv) bodyformals in
  let phics            = constrain_phis ve phis in
  let (ctem, sss, css) =
    M.array_fold_lefti begin fun i (ctem, sss, css) b ->
      let (cs, ss, ctem, ba) = constrain_stmt (fs, hv, ve) ctem ftv.ret b.Ssa.bstmt in
        Array.set bas i ba;
        (ctem, ss :: sss, cs :: css)
    end (ExpMap.empty, [], []) blocks
  in
  let sss = (ftv.ret :: List.map snd ftv.args @ List.map snd vars |> List.map prectype_sloc |> Misc.maybe_list) :: sss in
  let ss  = sss |> List.concat |> M.sort_and_compact in
  let cs  = List.map (fun s -> mk_heapinc loc s hv) ss :: formalcs :: phics :: css |> List.concat in
    P.printf "Constraints for %s:\n\n" fd.C.svar.C.vname;
    P.printf "%a\nheapvar   %a\n" d_cfunvar ftv d_heapvar hv;
    P.printf "vars      %a\n" (P.d_list ", " S.d_sloc) ss;
    List.iter (fun c -> P.printf "%a\n" d_cstr c |> ignore) cs;
    P.printf "\n";
    (ctem, bas, ss, cs)

let fresh_fun_typ (fd: C.fundec): cfunvar =
  let rty, ftyso, _, _ = C.splitFunctionType fd.C.svar.C.vtype in
  let fctys            = match ftyso with None -> [] | Some ftys -> List.map (fun (fn, fty, _) -> (fn, fresh_ctypevar fty)) ftys in
    mk_cfun [] fctys (fresh_ctypevar rty) SLM.empty SLM.empty

let constrain_scc ((fs, ae, css): funenv * annotenv * (heapvar * cstr list) list) (scc: (C.varinfo * ST.ssaCfgInfo) list): funenv * annotenv * (heapvar * cstr list) list =
  let fvs, scis              = List.split scc in
  let ftvs                   = List.map (fun sci -> fresh_fun_typ sci.ST.fdec) scis in
  let hv                     = fresh_heapvar () in
  let fs                     = List.fold_left2 (fun fs fv ftv -> VM.add fv (ftv, hv) fs) fs fvs ftvs in
  let ctems, bass, sss, css2 = List.map2 (constrain_fun fs hv) ftvs scis |> M.split4 in
  let ss                     = List.concat sss in
  let fs                     = List.fold_left (fun fs fv -> VM.add fv ({(VM.find fv fs |> fst) with qlocs = ss}, hv) fs) fs fvs in
  let ae                     = List.combine ctems bass |> List.fold_left2 (fun ae fv fa -> VM.add fv fa ae) ae fvs in
    (fs, ae, (hv, List.concat css2) :: css)

(******************************************************************************)
(************************** Constraint Simplification *************************)
(******************************************************************************)
(* The following assumes that all locations are quantified, i.e., that
   no location in a callee's SCC also appears in a caller's SCC. *)

type cstrmap = cstr IM.t

module IMP = P.MakeMapPrinter(IM)

let d_cstrmap =
  IMP.d_map ~dmaplet:(fun d1 d2 -> P.dprintf "%t\t%t" (fun () -> d1) (fun () -> d2)) "\n" (fun () cid -> P.num cid) d_cstr

type heapdom = SS.t IM.t

let d_heapdom () (hd: heapdom): P.doc =
  IM.fold (fun hv ss d -> P.dprintf "%a: %a@!%t" d_heapvar hv (P.d_list ", " S.d_sloc) (SS.elements ss) (fun () -> d)) hd P.nil

(* pmr: fix Set so this isn't necessary *)
let slocset_map (f: S.t -> S.t) (ss: SS.t): SS.t =
  SS.fold (fun s ss2 -> SS.add (f s) ss2) ss SS.empty

let dom_trans (hd: heapdom) (c: cstr): heapdom =
  match c.cdesc with
    | `CInHeap (s, hv) ->
        let ss = IM.find hv hd in
          IM.add hv (SS.add s ss) hd
    | `CSubheap (hv1, sub, hv2) ->
        let ss1 = IM.find hv1 hd in
        let ss2 = IM.find hv2 hd in
          IM.add hv1 (SS.union (slocset_map (subst_sloc sub) ss2) ss1) hd
    | _ -> hd

let mk_heapdom (css: (heapvar * cstr list) list): heapdom =
  List.fold_left (fun hd (hv, cs) -> List.fold_left dom_trans (IM.add hv SS.empty hd) cs) IM.empty css

module SCM = M.MapWithDefault(struct
                                type t = S.t
                                type v = cstr list
                                let compare = S.compare
                                let default = []
                              end)

type scm = cstr list SCM.t

let close_inc (hd: heapdom) (scm: scm) (c: cstr): scm =
  match c.cdesc with
    | `CInLoc (_, _, s)       -> SCM.add s (c :: SCM.find s scm) scm
    | `CSubheap (_, sub, hv2) ->
        let ss = IM.find hv2 hd in
          SS.fold (fun s scm ->
                     let subs = subst_sloc sub s in
                       SCM.add subs (List.map (cstr_subst sub) (SCM.find s scm) @ SCM.find subs scm) scm) ss scm
    | _ -> scm

let is_not_inc (s: S.t) (c: cstr): bool =
  match c.cdesc with
    | `CInLoc (_, _, s2) -> not (S.eq s s2)
    | _                  -> true

let close_incs (hd: heapdom) (css: (heapvar * cstr list) list): cstr list list =
  let scm = List.fold_left (fun scm (_, cs) -> List.fold_left (close_inc hd) scm cs) SCM.empty css in
    SCM.fold (fun s cs css -> List.filter (is_not_inc s) cs :: css) scm (List.map snd css)

let simplify_cs (css: (heapvar * cstr list) list): heapdom * cstrmap =
  let css = List.rev css in
  let hd  = mk_heapdom css in
  let cs  = css |> close_incs hd |> List.concat |> List.filter is_cstr_simple |> List.fold_left (fun cm c -> IM.add c.cid c cm) IM.empty in
    (hd, cs)

(* API *)
let infer_shapes (env: ctypeenv) (cg: Callgraph.t) (scim: ST.ssaCfgInfo SM.t): shape SM.t * ctypeenv =
  let sccs        = List.rev_map (fun scc -> List.map (fun fv -> (fv, SM.find fv.C.vname scim)) scc) cg in
  let fs, ae, css = List.fold_left constrain_scc (VM.empty, VM.empty, []) sccs in
  let hd, cm      = simplify_cs css in
  let _           = P.printf "Heap domains:@!@!%a@!@!" d_heapdom hd in
  let _           = P.printf "Simplified closed constraints:@!@!%a@!@!" d_cstrmap cm in
    assert false
