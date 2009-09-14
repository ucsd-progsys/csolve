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
module CG  = Callgraph

open Ctypes
open M.Ops

(******************************************************************************)
(****************************** Index Constraints *****************************)
(******************************************************************************)

type indexvar = int

let d_indexvar () (iv: indexvar): P.doc =
  P.text <| "i" ^ string_of_int iv

let (fresh_indexvar, reset_fresh_indexvars) = M.mk_int_factory ()

let with_fresh_indexvar (f: indexvar -> 'a): 'a =
  fresh_indexvar () |> f

type indexexp =
  | IEConst of index
  | IEVar of indexvar
  | IEPlus of indexvar * (* RHS scale: *) int * indexvar
  | IEMinus of indexvar * (* RHS scale: *) int * indexvar
  | IEMult of indexvar * indexvar
  | IEDiv of indexvar * indexvar

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

let indexexp_apply (is: indexsol): indexexp -> index = function
  | IEConst i             -> i
  | IEVar iv              -> IndexSol.find iv is
  | IEPlus (iv1, x, iv2)  -> index_plus (IndexSol.find iv1 is) (index_scale x <| IndexSol.find iv2 is)
  | IEMinus (iv1, x, iv2) -> index_minus (IndexSol.find iv1 is) (index_scale x <| IndexSol.find iv2 is)
  | IEMult (iv1, iv2)     -> index_mult (IndexSol.find iv1 is) (IndexSol.find iv2 is)
  | IEDiv (iv1, iv2)      -> index_div (IndexSol.find iv1 is) (IndexSol.find iv2 is)

let refine_index (ie: indexexp) (iv: indexvar) (is: indexsol): indexsol =
  IndexSol.add iv (index_lub (indexexp_apply is ie) (IndexSol.find iv is)) is

let indexcstr_sat (ic: indexcstr) (is: indexsol): bool =
  match ic with
    | ICVarLess (ie, iv)  -> is_subindex (indexexp_apply is ie) (IndexSol.find iv is)
    | ICConstLess (ie, i) -> is_subindex (indexexp_apply is ie) i

(******************************************************************************)
(****************************** Type Constraints ******************************)
(******************************************************************************)

type ctypevar = indexvar prectype

let fresh_ctvint (n: int): ctypevar =
  CTInt (n, fresh_indexvar ())

let fresh_ctvref (): ctypevar =
  CTRef (Sloc.fresh Sloc.Abstract, fresh_indexvar ())

let ctypevar_apply (is: indexsol): ctypevar -> ctype = function
  | CTInt (n, iv) -> CTInt (n, IndexSol.find iv is)
  | CTRef (s, iv) -> CTRef (s, IndexSol.find iv is)

type ctypecstr =
  | CTCSubtype of ctypevar * ctypevar
  | CTCDSubtype of ctypevar * ctypevar * (* bound: *) ctype * (* check: *) C.varinfo * FI.refctype

type dcheck = C.varinfo * FI.refctype

let d_dcheck () ((vi, rt): dcheck): P.doc =
  P.dprintf "%s :: %a" vi.C.vname FI.d_refctype rt

let refine_ctype (ctv1: ctypevar) (ctv2: ctypevar) (is: indexsol): indexsol =
  match (ctv1, ctv2) with
    | (CTInt (n1, iv1), CTInt (n2, iv2)) when n1 = n2       -> refine_index (IEVar iv1) iv2 is
    | (CTRef (s1, iv1), CTRef (s2, iv2)) when Sloc.eq s1 s2 -> refine_index (IEVar iv1) iv2 is
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

type storecstr =
  | SCInc of S.t * indexvar * ctypevar (* (l, i, ct): (i, ct) in store(l) *)
  | SCMem of S.t                       (* l in dom(store) *)
  | SCUniq of S.t list                 (* for all l1, l2 in list, not S.eq l1 l2 *)

type storesol = indexvar prestore

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
(*************************** Systems of Constraints ***************************)
(******************************************************************************)

type cstrdesc =
  | CSIndex of indexcstr
  | CSCType of ctypecstr
  | CSStore of storecstr

type cstr = {cdesc: cstrdesc; cloc: C.location}

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
    | C.TPtr _ | C.TArray _ -> fresh_ctvref ()
    | _                     -> E.s <| E.bug "Unimplemented fresh_ctypevar: %a@!@!" C.d_type t

(******************************************************************************)
(******************************* Shape Solutions ******************************)
(******************************************************************************)

type ctvemap = ctypevar ExpMap.t

let d_vartypes () vars =
  P.docList ~sep:(P.dprintf "@!") (fun (v, ct) -> P.dprintf "%s: %a" v.C.vname Ctypes.d_ctype ct) () vars

module IM = M.IntMap

type ctvenv = ctypevar IM.t

type cstremap = ctvemap * cstr list

type shape =
  {vtyps : (Cil.varinfo * Ctypes.ctype) list;
   etypm : Ctypes.ctemap;
   store : Ctypes.store;
   anna  : RA.block_annotation array;
   theta : RA.ctab }

let print_shape (fname: string) (cf: cfun) ({vtyps = locals; store = st}: shape) (ds: dcheck list): unit =
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

(******************************************************************************)
(**************************** Constraint Generation ***************************)
(******************************************************************************)

let constrain_const (loc: C.location): C.constant -> ctypevar * cstr = function
  | C.CInt64 (v, ik, _) -> with_fresh_indexvar <| fun iv -> (CTInt (C.bytesSizeOfInt ik, iv), mk_ivarless loc (IEConst (index_of_int (Int64.to_int v))) iv)
  | C.CChr c            -> with_fresh_indexvar <| fun iv -> (CTInt (int_width, iv), mk_ivarless loc (IEConst (IInt (Char.code c))) iv)
  | c                   -> E.s <| E.bug "Unimplemented constrain_const: %a@!@!" C.d_const c

let constrain_sizeof (loc: C.location) (t: C.typ): ctypevar * cstr =
  with_fresh_indexvar <| fun iv -> (CTInt (int_width, iv), mk_ivarless loc (IEConst (IInt (C.bitsSizeOf t / 8))) iv)

let rec constrain_exp_aux (ve: ctvenv) (em: cstremap) (loc: C.location): C.exp -> ctypevar * cstremap * cstr list = function
  | C.Const c                     -> let (ctv, c) = constrain_const loc c in (ctv, em, [c])
  | C.Lval lv | C.StartOf lv      -> let (ctv, em) = constrain_lval ve em loc lv in (ctv, em, [])
  | C.UnOp (uop, e, t)            -> constrain_unop uop ve em loc t e
  | C.BinOp (bop, e1, e2, t)      -> constrain_binop bop ve em loc t e1 e2
  | C.CastE (C.TPtr _, C.Const c) -> constrain_constptr em loc c
  | C.CastE (ct, e)               -> constrain_cast ve em loc ct e
  | C.AddrOf lv                   -> constrain_addrof ve em loc lv
  | C.SizeOf t                    -> let (ctv, c) = constrain_sizeof loc t in (ctv, em, [c])
  | e                             -> E.s <| E.error "Unimplemented constrain_exp_aux: %a@!@!" C.d_exp e

and constrain_unop (op: C.unop) (ve: ctvenv) (em: cstremap) (loc: C.location) (t: C.typ) (e: C.exp): ctypevar * cstremap * cstr list =
  let (ctv, em) = constrain_exp ve em loc e in
    match ctv with
      | CTInt _ -> apply_unop op em loc t ctv
      | _       -> E.s <| E.unimp "Haven't considered how to apply unops to references@!"

and apply_unop (op: C.unop) (em: cstremap) (loc: C.location) (rt: C.typ) (ctv: ctypevar): ctypevar * cstremap * cstr list =
  match op with
    | C.LNot -> with_fresh_indexvar (fun iv -> (CTInt (typ_width rt, iv), em, [mk_ivarless loc (IEConst (ISeq (0, 1))) iv]))
    | C.BNot -> with_fresh_indexvar (fun iv -> (CTInt (typ_width rt, iv), em, [mk_ivarless loc (IEConst ITop) iv]))
    | C.Neg  -> with_fresh_indexvar (fun iv -> (CTInt (typ_width rt, iv), em, [mk_ivarless loc (IEConst ITop) iv]))

and constrain_binop (op: C.binop) (ve: ctvenv) (em: cstremap) (loc: C.location) (t: C.typ) (e1: C.exp) (e2: C.exp): ctypevar * cstremap * cstr list =
  let (ctv1, em1) = constrain_exp ve em loc e1 in
  let (ctv2, em2) = constrain_exp ve em1 loc e2 in
    apply_binop op em2 loc t ctv1 e2 ctv2

and apply_binop: C.binop -> cstremap -> C.location -> C.typ -> ctypevar -> C.exp -> ctypevar -> ctypevar * cstremap * cstr list = function
  | C.PlusA                                 -> constrain_arithmetic (fun iv1 iv2 -> IEPlus (iv1, 1, iv2))
  | C.MinusA                                -> constrain_arithmetic (fun iv1 iv2 -> IEMinus (iv1, 1, iv2))
  | C.Mult                                  -> constrain_arithmetic (fun iv1 iv2 -> IEMult (iv1, iv2))
  | C.Div                                   -> constrain_arithmetic (fun iv1 iv2 -> IEDiv (iv1, iv2))
  | C.Mod                                   -> constrain_unknown
  | C.PlusPI | C.IndexPI                    -> constrain_ptrarithmetic (fun iv1 x iv2 -> IEPlus (iv1, x, iv2))
  | C.MinusPI                               -> constrain_ptrarithmetic (fun iv1 x iv2 -> IEMinus (iv1, x, iv2))
  | C.MinusPP                               -> constrain_ptrminus
  | C.Lt | C.Gt | C.Le | C.Ge | C.Eq | C.Ne -> constrain_rel
  | C.BAnd | C.BOr | C.BXor                 -> constrain_unknown
  | C.Shiftlt | C.Shiftrt                   -> constrain_unknown
  | bop                                     -> E.s <| E.bug "Unimplemented apply_binop: %a@!@!" C.d_binop bop

and constrain_arithmetic (f: indexvar -> indexvar -> indexexp) (em: cstremap) (loc: C.location) (rt: C.typ) (ctv1: ctypevar) (_: C.exp) (ctv2: ctypevar): ctypevar * cstremap * cstr list =
    match ctv1, ctv2 with
      | (CTInt (n1, iv1), CTInt (n2, iv2)) -> with_fresh_indexvar <| fun iv -> (CTInt (typ_width rt, iv), em, [mk_ivarless loc (f iv1 iv2) iv])
      | _ -> E.s <| E.bug "Type mismatch in constrain_arithmetic@!@!"

and constrain_ptrarithmetic (f: indexvar -> int -> indexvar -> indexexp) (em: cstremap) (loc: C.location) (pt: C.typ) (ctv1: ctypevar) (e2: C.exp) (ctv2: ctypevar): ctypevar * cstremap * cstr list =
    match (C.unrollType pt, ctv1, ctv2) with
      | (C.TPtr (t, _), CTRef (s, ie1), CTInt (n, ie2)) when n = int_width ->
          begin match e2 with
            | C.Const _ -> with_fresh_indexvar <| fun iv -> (CTRef (s, iv), em, [mk_ivarless loc (f ie1 (typ_width t) ie2) iv])
            | C.Lval (C.Var vi, C.NoOffset) ->
                let iv  = fresh_indexvar () in
                let iv3 = fresh_indexvar () in
                let vv  = Ast.Symbol.value_variable Ast.Sort.Int in
                let rt  = CTInt (n, (ITop, FC.make_reft vv Ast.Sort.Int [FC.Conc (Ast.pAtom (Ast.eVar vv, Ast.Ge, Ast.eCon (Ast.Constant.Int 0)))])) in
                  (CTRef (s, iv),
                   em,
                   [mk_ivarless loc (f ie1 (typ_width t) iv3) iv;
                    mk_dsubty loc ctv2 (CTInt (n, iv3)) (CTInt (n, ISeq (0, 1))) vi rt])
            | _ -> assert false
          end
      | _ -> E.s <| E.bug "Type mismatch in constrain_ptrarithmetic@!@!"

and constrain_ptrminus (em: cstremap) (loc: C.location) (pt: C.typ) (_: ctypevar) (_: C.exp) (_: ctypevar): ctypevar * cstremap * cstr list =
  with_fresh_indexvar (fun iv -> (CTInt (typ_width !C.upointType, iv), em, [mk_ivarless loc (IEConst ITop) iv]))

and constrain_unknown (em: cstremap) (loc: C.location) (rt: C.typ) (_: ctypevar) (_: C.exp) (_: ctypevar): ctypevar * cstremap * cstr list =
  with_fresh_indexvar (fun iv -> (CTInt (typ_width rt, iv), em, [mk_ivarless loc (IEConst ITop) iv]))

and constrain_rel (em: cstremap) (loc: C.location) (rt: C.typ) (_: ctypevar) (_: C.exp) (_: ctypevar): ctypevar * cstremap * cstr list =
  with_fresh_indexvar (fun iv -> (CTInt (typ_width rt, iv), em, [mk_ivarless loc (IEConst (ISeq (0, 1))) iv]))

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

and constrain_cast (ve: ctvenv) (em: cstremap) (loc: C.location) (ct: C.typ) (e: C.exp): ctypevar * cstremap * cstr list =
  match (C.unrollType ct, C.unrollType <| C.typeOf e) with
    | (C.TInt (ik, _), C.TPtr _) ->
        with_fresh_indexvar (fun iv -> (CTInt (C.bytesSizeOfInt ik, iv), em, [mk_ivarless loc (IEConst ITop) iv]))
    | (C.TInt (ik, _), C.TInt _) ->
        begin match constrain_exp_aux ve em loc e with
          | (CTInt (n, ive), em, cs) ->
              let ivc =
                if n <= C.bytesSizeOfInt ik then
                  IEVar ive
                else if not !Constants.safe then begin
                  C.warnLoc loc "Unsoundly assuming cast is lossless@!@!" |> ignore;
                  IEVar ive
                end else
                  IEConst ITop
              in with_fresh_indexvar <| fun iv -> (CTInt (C.bytesSizeOfInt ik, iv), em, mk_ivarless loc ivc iv :: cs)
          | _ -> E.s <| C.errorLoc loc "Got bogus type in contraining int-int cast@!@!"
        end
    | _ -> constrain_exp_aux ve em loc e

and constrain_lval_aux (ve: ctvenv) (em: cstremap) (loc: C.location): C.lval -> ctypevar * cstremap = function
  | (C.Var v, C.NoOffset)       -> (IM.find v.C.vid ve, em)
  | (C.Mem e, C.NoOffset) as lv ->
      let (ctv, (ctvm, cs)) = constrain_exp ve em loc e in
      let ctv2              = fresh_ctvref () in
        begin match ctv2 with
          | CTRef (s, iv) ->
              let ctvlv = fresh_ctypevar <| C.typeOfLval lv in
              let cs    = mk_storeinc loc s iv ctvlv :: mk_subty loc ctv ctv2 :: cs in
                (ctvlv, (ctvm, cs))
          | _ -> E.s <| E.bug "fresh_ctvref gave back non-ref type in constrain_lval@!@!"
        end
  | lv -> E.s <| E.error "constrain_lval got lval with offset: %a@!@!" C.d_lval lv

and constrain_lval (ve: ctvenv) (em: cstremap) (loc: C.location) (lv: C.lval): ctypevar * cstremap =
  let (ctv, (ctvm, cs)) = constrain_lval_aux ve em loc lv in
    (ctv, (ExpMap.add (C.Lval lv) ctv ctvm, cs))

and constrain_addrof (ve: ctvenv) (em: cstremap) (loc: C.location): C.lval -> ctypevar * cstremap * cstr list = function
  | (C.Var f, C.NoOffset) when C.isFunctionType <| C.unrollType f.C.vtype ->
      (fresh_ctvref (), em, [])
  | lv -> E.s <| E.error "Don't know how to take address of %a@!@!" C.d_lval lv

and constrain_exp (ve: ctvenv) (em: cstremap) (loc: C.location) (e: C.exp): ctypevar * cstremap =
  let (ctv, (ctvm, cs), cs') = constrain_exp_aux ve em loc e in
    (ctv, (ExpMap.add e ctv ctvm, cs @ cs'))

let constrain_args (ve: ctvenv) (em: cstremap) (loc: C.location) (es: C.exp list): ctypevar list * cstremap =
  List.fold_right (fun e (ctvs, em) -> let (ctv, em) = constrain_exp ve em loc e in (ctv :: ctvs, em)) es ([], em)

type funmap = (cfun * Ssa_transform.ssaCfgInfo) SM.t

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

let constrain_app (env: ctypeenv) (ve: ctvenv) (em: cstremap) (loc: C.location) (f: string) (lvo: C.lval option) (args: C.exp list): cstremap * RA.annotation list =
  let (ctvs, (ctvm, argcs))   = constrain_args ve em loc args in
  let (rtv, atvs, ics, annot) = instantiate_function loc env f in
  let (ctvm, cs)              = try (ctvm, List.concat [Misc.map2 (mk_subty loc) ctvs atvs; ics; argcs]) 
                                with ex -> E.s <| C.errorLoc loc "constrain_app: bad spec for %s" f in
    match lvo with
      | None    -> ((ctvm, cs), annot)
      | Some lv ->
          let (lvctv, (ctvm, cs)) = constrain_lval ve (ctvm, cs) loc lv in
            ((ctvm, mk_subty loc rtv lvctv :: cs), annot)

let printf_funs = ["printf"; "fprintf"; "chatting"]

let constrain_instr_aux (env: ctypeenv) (ve: ctvenv) ((em, bas): cstremap * RA.block_annotation): C.instr -> cstremap * RA.block_annotation = function
  | C.Set (lv, e, loc) ->
      let (ctv1, em1)        = constrain_lval ve em loc lv in
      let (ctv2, (ctvm, cs)) = constrain_exp ve em1 loc e in
      ((ctvm, mk_subty loc ctv2 ctv1 :: cs), [] :: bas)
  | C.Call (None, C.Lval (C.Var {C.vname = f}, C.NoOffset), args, loc) when List.mem f printf_funs ->
      if not !Constants.safe then C.warnLoc loc "Unsoundly ignoring printf-style call@!@!" |> ignore else E.s <| C.errorLoc loc "Can't handle printf";
      (constrain_args ve em loc args |> snd, [] :: bas)
  | C.Call (lvo, C.Lval (C.Var {C.vname = f}, C.NoOffset), args, loc) ->
      let (em, ba) = constrain_app env ve em loc f lvo args in
        (em, ba :: bas)
  | i -> E.s <| E.bug "Unimplemented constrain_instr: %a@!@!" C.dn_instr i

let constrain_instr (env: ctypeenv) (ve: ctvenv) (em: cstremap) (is: C.instr list): cstremap * RA.block_annotation =
  let (em, bas) = List.fold_left (constrain_instr_aux env ve) (em, []) is in
  (em, List.rev ([]::bas))


let constrain_return (env: ctypeenv) (ve: ctvenv) ((ctvm, cs): cstremap) (rt: ctype) (loc: C.location): C.exp option -> cstremap * RA.block_annotation = function
  | None   -> if rt = void_ctype then ((ctvm, cs), []) else (C.errorLoc loc "Returning void value for non-void function\n\n" |> ignore; assert false)
  | Some e ->
      let (ctv, (ctvm, cs)) = constrain_exp ve (ctvm, cs) loc e in
        ((ctvm, mk_const_subty loc ctv rt @ cs), [])

let constrain_stmt (env: ctypeenv) (ve: ctvenv) (em: cstremap) (rt: ctype) (s: C.stmt): cstremap * RA.block_annotation =
  match s.C.skind with
    | C.Instr is             -> constrain_instr env ve em is
    | C.Break _              -> (em, [])
    | C.Continue _           -> (em, [])
    | C.Goto _               -> (em, [])
    | C.Block _              -> (em, [])                              (* we'll visit this later as we iterate through blocks *)
    | C.If (e, _, _, loc)    -> (snd (constrain_exp ve em loc e), []) (* we'll visit the subblocks on a later pass *)
    | C.Loop (_, _, _, _)    -> (em, [])                              (* ditto *)
    | C.Return (rexp, loc)   -> constrain_return env ve em rt loc rexp
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

let mk_phi_defs_cs (ve: ctvenv) ((vphi, vdefs): C.varinfo * (int * C.varinfo) list): cstr list =
  List.map (fun (_, vdef) -> mk_subty vphi.C.vdecl (IM.find vdef.C.vid ve) (IM.find vphi.C.vid ve)) vdefs

let mk_phis_cs (ve: ctvenv) (phis: (C.varinfo * (int * C.varinfo) list) list array): cstr list =
  Array.to_list phis |> List.flatten |> List.map (mk_phi_defs_cs ve) |> List.concat

let constrain_cfg (env: ctypeenv) (vars: ctypevar IM.t) (rt: ctype) (cfg: Ssa.cfgInfo): cstremap * RA.block_annotation array =
  let blocks = cfg.Ssa.blocks in
  let bas    = Array.make (Array.length blocks) [] in
  let em     =
    M.array_fold_lefti begin fun i em b ->
        let (em, ba) = constrain_stmt env vars em rt b.Ssa.bstmt in
          Array.set bas i ba;
          em
      end (ExpMap.empty, []) blocks
  in (em, bas)

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
    if !Cs.verbose_level >= Cs.ol_ctypes || !Cs.ctypes_only then print_shape fd.C.svar.C.vname cf shp ds;
    (shp, ds)

(* API *)
let infer_shapes (env: ctypeenv) (scis: funmap): (shape * dcheck list) SM.t * ctypeenv =
  (scis |> SM.map (infer_shape env |> M.uncurry), env)
