module M   = Misc
module P   = Pretty
module C   = Cil
module Cs  = Constants
module E   = Errormsg
module ST  = Ssa_transform
module RA  = Refanno
module SM  = Misc.StringMap
module S   = Sloc
module SLM = S.SlocMap
module SS  = S.SlocSet
module VM  = CilMisc.VarMap
module CM  = CilMisc

open Ctypes
open M.Ops

(******************************************************************************)
(****************************** Store Constraints *****************************)
(******************************************************************************)

type heapvar = int

let (fresh_heapvar, reset_fresh_heapvars) = M.mk_int_factory ()

let d_heapvar () (hv: heapvar): P.doc =
  P.text <| "?h" ^ string_of_int hv

type heapmap = heapvar VM.t

(******************************************************************************)
(****************************** Misc Junk to Move *****************************)
(******************************************************************************)

module IM = M.IntMap

type ctvenv = ctype VM.t

type ctvemap = ctype ExpMap.t

type funenv = (cfun * ctvenv) VM.t

type env = funenv * heapvar * ctvenv

(* consider replacing with the vars instead of exps; we really only care about
   function locals *)
type annotenv = (ctvemap * RA.block_annotation array) VM.t

(******************************************************************************)
(********************************* Constraints ********************************)
(******************************************************************************)

type simplecstrdesc = [
| `CInLoc of index * ctype * S.t
| `CSubtype of ctype * S.subst * ctype
| `CWFSubst of S.subst
]

let simplecstrdesc_slocs: simplecstrdesc -> S.t list = function
  | `CInLoc (_, ct, s)        -> M.maybe_cons (prectype_sloc ct) [s]
  | `CSubtype (ct1, sub, ct2) -> M.maybe_cons (prectype_sloc ct1) (M.maybe_cons (prectype_sloc ct2) (S.subst_slocs sub))
  | `CWFSubst (sub)           -> S.subst_slocs sub

type cstrdesc = [
| simplecstrdesc
| `CInHeap of S.t * heapvar
| `CSubheap of heapvar * S.subst * heapvar
]

let is_cstrdesc_simple: cstrdesc -> bool = function
  | `CInHeap _ | `CSubheap _ -> false
  | _                        -> true

let d_cstrdesc (): [< cstrdesc] -> P.doc = function
  | `CSubtype (ctv1, sub, ctv2) -> P.dprintf "@[@[%a@] <: @[%a %a@]@]" d_ctype ctv1 S.d_subst sub d_ctype ctv2
  | `CInHeap (s, hv)            -> P.dprintf "%a ∈ %a" S.d_sloc s d_heapvar hv
  | `CInLoc (i, ctv, s)         -> P.dprintf "(%a, %a) ∈ %a" d_index i d_ctype ctv S.d_sloc s
  | `CSubheap (hv1, sub, hv2)   -> P.dprintf "%a <: %a %a" d_heapvar hv1 S.d_subst sub d_heapvar hv2
  | `CWFSubst (sub)             -> P.dprintf "WF(%a)" S.d_subst sub

let apply_subst_to_subst (appsub: S.subst) (sub: S.subst): S.subst =
  List.map (fun (sfrom, sto) -> (S.subst_apply appsub sfrom, S.subst_apply appsub sto)) sub

let simplecstrdesc_subst (sub: S.subst): simplecstrdesc -> simplecstrdesc = function
  | `CSubtype (ctv1, sub2, ctv2) -> `CSubtype (prectype_subs sub ctv1, apply_subst_to_subst sub sub2, prectype_subs sub ctv2)
  | `CInLoc (ie, ctv, s)         -> `CInLoc (ie, prectype_subs sub ctv, S.subst_apply sub s)
  | `CWFSubst sub2               -> `CWFSubst (apply_subst_to_subst sub sub2)

let cstrdesc_subst (sub: S.subst): cstrdesc -> cstrdesc = function
  | #simplecstrdesc as scd     -> (simplecstrdesc_subst sub scd :> cstrdesc)
  | `CSubheap (hv1, sub2, hv2) -> `CSubheap (hv1, apply_subst_to_subst sub sub2, hv2)
  | `CInHeap (s, hv)           -> `CInHeap (S.subst_apply sub s, hv)

type 'a precstr = {cid: int; cdesc: 'a; cloc: C.location}

type cstr = cstrdesc precstr

type simplecstr = simplecstrdesc precstr

let (fresh_cstrid, reset_fresh_cstrids) = M.mk_int_factory ()

let mk_cstr (loc: C.location) (cdesc: cstrdesc): cstr =
  {cid = fresh_cstrid (); cloc = loc; cdesc = cdesc}

let cstr_subst (sub: S.subst) (c: cstr): cstr =
  mk_cstr c.cloc (cstrdesc_subst sub c.cdesc)

let d_cstr () ({cid = cid; cdesc = cdesc; cloc = loc}: cstr): P.doc =
  P.dprintf "%a: %a" C.d_loc loc d_cstrdesc cdesc

let d_simplecstr () ({cid = cid; cdesc = cdesc; cloc = loc}: simplecstr): P.doc =
  P.dprintf "%a: %a" C.d_loc loc d_cstrdesc cdesc

let filter_simple_cstrs_aux (scs: simplecstr list) (c: cstr): simplecstr list =
  match c.cdesc with
    | #simplecstrdesc as sds -> {c with cdesc = sds} :: scs
    | _                      -> scs

let filter_simple_cstrs (cs: cstr list): simplecstr list =
  List.fold_left filter_simple_cstrs_aux [] cs

type cstremap = ctvemap * cstr list

(******************************************************************************)
(***************************** Constraint Solving *****************************)
(******************************************************************************)

let inloc_sat (st: store) (i: index) (s: S.t) (ct1: ctype): bool =
  try
    match prestore_find_index s i st with
      | [ct2] -> prectype_eq ct1 ct2
      | _     -> false
  with Not_found -> false

let all_slocs_eq: S.t list -> bool = function
  | []      -> true
  | s :: ss -> List.fold_left (fun all_eq s' -> all_eq && S.eq s s') true ss

let images_of_subst (sub: S.subst): S.t list list =
  sub |> M.groupby fst |> List.map (List.map snd)

let subst_well_defined (sub: S.subst): bool =
  sub |> images_of_subst |> List.for_all all_slocs_eq

let subst_transpose (sub: S.subst): S.subst =
  List.map (fun (x, y) -> (y, x)) sub

let simplecstr_sat (st: store) (sc: simplecstr): bool =
  match sc.cdesc with
    | `CInLoc (i, ct, s)        -> inloc_sat st i s ct
    | `CSubtype (ct1, sub, ct2) -> is_subctype ct1 (prectype_subs sub ct2)
    | `CWFSubst (sub)           -> subst_well_defined sub && sub |> subst_transpose |> subst_well_defined

(* pmr: better error reporting *)
let unify_ctypes (ct1: ctype) (ct2: ctype) (sub: S.subst): S.subst =
  match ct1, ct2 with
    | CTRef (s1, _), CTRef (s2, _) when S.eq s1 s2 -> sub
    | CTRef (s1, _), CTRef (s2, _)                 -> S.subst_extend s1 s2 sub
    | CTInt (n1, _), CTInt (n2, _) when n1 = n2    -> sub
    | _                                            -> assert false

let store_add (l: Sloc.t) (pl: ploc) (ctv: ctype) (sto: store): store =
  SLM.add l (LDesc.add pl ctv (prestore_find l sto)) sto

(* pmr: This doesn't seem to check for partial overlap! *)
let refine_inloc (l: Sloc.t) (i: index) (ct: ctype) (sto: store): S.subst * store =
  match i with
    | IBot   -> ([], sto)
    | IInt n ->
        let pl = PLAt n in
          begin match LDesc.find pl (prestore_find l sto) with
            | []         -> ([], store_add l pl ct sto)
            | [(_, ct2)] -> (unify_ctypes ct ct2 [], sto)
            | _          -> assert false
        end
    | ISeq (n, m) ->
        let ld, sub = LDesc.shrink_period m unify_ctypes [] (prestore_find l sto) in
        let pl      = PLSeq n in
        let cts     = LDesc.find pl ld in
        let sub     = List.fold_left (fun sub (_, ct2) -> unify_ctypes ct ct2 sub) sub cts in
        let p       = ld |> LDesc.get_period |> Misc.get_option 0 in
          if List.exists (fun (pl2, _) -> ploc_contains pl2 pl p) cts then
            (* If this sequence is included in an existing one, there's nothing left to do *)
            (sub, sto)
          else
            (* Otherwise, remove "later", overlapping elements and add this sequence.
               (Note if there's no including sequence, all the elements we found previously
                come after this one.) *)
            let ld = List.fold_left (fun ld (pl2, _) -> LDesc.remove pl2 ld) ld cts in
            let ld = LDesc.add pl ct ld in
              (sub, SLM.add l ld sto)
    | ITop ->
        let ld, sub = LDesc.shrink_period (prectype_width ct) unify_ctypes [] (prestore_find l sto) in
        let ld, sub = LDesc.foldn (fun _ (ld, sub) pl ct2 -> (LDesc.remove pl ld, unify_ctypes ct ct2 sub)) (ld, sub) ld in
          (sub, SLM.add l (LDesc.add PLEverywhere ct ld) sto)

(* pmr: better error report; check subtyping really holds *)
let refine_subtype (ct1: ctype) (sub: S.subst) (ct2: ctype): S.subst =
  match ct1, ct2 with
    | CTRef (s1, _), CTRef (s2, _) -> [(s1, S.subst_apply sub s2)]
    | _                            -> assert false

let unify_slocs: S.t list -> S.subst = function
  | []      -> []
  | s :: ss -> List.fold_left (fun sub s' -> S.subst_extend s' s sub) [] ss

let make_subst_well_defined (sub: S.subst): S.subst =
  sub |> images_of_subst |> List.fold_left (fun outsub rng -> S.subst_compose (unify_slocs rng) outsub) []

let refine_wfsubst (sub: S.subst): S.subst =
  S.subst_compose (sub |> make_subst_well_defined) (sub |> subst_transpose |> make_subst_well_defined)

let refine_aux (sc: simplecstr) (sto: store): S.subst * store =
  match sc.cdesc with
    | `CInLoc (i, ct, s)        -> refine_inloc s i ct sto
    | `CSubtype (ct1, sub, ct2) -> (refine_subtype ct1 sub ct2, sto)
    | `CWFSubst (sub)           -> (refine_wfsubst sub, sto)

type slocdep = int list SLM.t (* sloc -> cstr deps *)

type cstrmap = simplecstr IM.t (* cstr ident -> cstr map *)

(* pmr: should make sure that subst is well-defined at this point *)
let adjust_slocdep (sub: S.subst) (sd: slocdep): slocdep =
  List.fold_left begin fun sd (sfrom, sto) ->
    (* pmr: this find pattern is common; should be using a MapWithDefault *)
    let olddeps = try SLM.find sfrom sd with Not_found -> [] in
    let newdeps = try SLM.find sto sd with Not_found -> [] in
      sd |> SLM.remove sfrom |> SLM.add sto (newdeps @ olddeps)
  end sd sub

let refine (sd: slocdep) (cm: cstrmap) (sub: S.subst) (sc: simplecstr) (sto: store): slocdep * cstrmap * S.subst * store * int list =
  let refsub, sto   = refine_aux sc sto in
  let invalid_slocs = S.subst_slocs refsub in
  let sto           = invalid_slocs |> List.fold_left (fun sto s -> SLM.remove s sto) sto |> prestore_subs refsub in
  let succs         = invalid_slocs |> M.flap (fun ivs -> try SLM.find ivs sd with Not_found -> (P.printf "Couldn't find dep for %a\n\n" S.d_sloc ivs; assert false)) in
  let sd            = adjust_slocdep refsub sd in
  let cm            = IM.map (fun sc -> {sc with cdesc = simplecstrdesc_subst refsub sc.cdesc}) cm in
    (sd, cm, S.subst_compose refsub sub, sto, succs)

(* pmr: pull out worklist code? *)
module WkList = Heaps.Functional(struct
                                   type t = int
                                   let compare = compare
                                 end)

let wklist_push (wkl: WkList.t) (itcids: int list): WkList.t =
  List.fold_left (M.flip WkList.add) wkl itcids

let rec iter_solve (wkl: WkList.t) (sd: slocdep) (cm: cstrmap) (sub: S.subst) (sto: store): slocdep * cstrmap * S.subst * store =
  match try Some (WkList.maximum wkl) with Heaps.EmptyHeap -> None with
    | None     -> (sd, cm, sub, sto)
    | Some cid ->
        let sc  = M.IntMap.find cid cm in
        let wkl = WkList.remove wkl in
          if simplecstr_sat sto sc then
            iter_solve wkl sd cm sub sto
          else
            let sd, cm, sub, sto, succs = refine sd cm sub sc sto in
            let wkl                     = wklist_push wkl succs in
              iter_solve wkl sd cm sub sto

let solve (sd: slocdep) (cm: cstrmap) (sub: S.subst) (sto: store): slocdep * cstrmap * S.subst * store =
  let wkl = IM.fold (fun cid _ cids -> cid :: cids) cm [] |> wklist_push WkList.empty in
    iter_solve wkl sd cm sub sto

type shape =
  {vtyps : (Cil.varinfo * Ctypes.ctype) list;
   etypm : Ctypes.ctemap;
   store : Ctypes.store;
   anna  : RA.block_annotation array;
   theta : RA.ctab }

(******************************************************************************)
(***************************** CIL Types to CTypes ****************************)
(******************************************************************************)

let int_width    = C.bytesSizeOfInt C.IInt
let short_width  = C.bytesSizeOfInt C.IShort
let char_width   = C.bytesSizeOfInt C.IChar

let fresh_heaptype (t: C.typ): ctype =
  match C.unrollType t with
    | C.TInt (ik, _)        -> CTInt (C.bytesSizeOfInt ik, ITop)
    | C.TVoid _             -> CTInt (0, IBot)
    | C.TPtr _ | C.TArray _ -> CTRef (S.fresh S.Abstract, IInt 0)
    | _                     -> E.s <| E.bug "Unimplemented fresh_ctype: %a@!@!" C.d_type t

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

and constrain_constptr (em: cstremap) (loc: C.location): C.constant -> ctype * cstremap * cstr list = function
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

and constrain_addrof (ve: ctvenv) (em: cstremap) (loc: C.location): C.lval -> ctype * cstremap * cstr list = function
  | (C.Var f, C.NoOffset) when C.isFunctionType <| C.unrollType f.C.vtype ->
      (fresh_ctvref (), em, [])
  | lv -> E.s <| E.error "Don't know how to take address of %a@!@!" C.d_lval lv

let instantiate_args (loc: C.location) (argcts: (string * ctype) list): ctype list * cstr list =
  let (argctvs, argcts) = argcts |> List.map (M.compose (ctype_of_ctype loc) snd) |> List.split in
    (argctvs, List.flatten argcts)

let instantiate_ret (loc: C.location): ctype option -> ctype option * cstr list = function
  | Some rct ->
      let (ctv, cs) = ctype_of_ctype loc rct in
        (Some ctv, cs)
  | None -> (None, [])

let instantiate_store (loc: C.location) (st: store): cstr list =
     prestore_fold (fun css l i ct -> mk_const_storeinc loc l i ct :: css) [] st
  |> List.concat
  |> SLM.fold (fun l _ css -> mk_storemem loc l :: css) st

exception Unified

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
  let (is, ss)                 = List.concat [formalcs; bodyformalcs; phics; storecs; bodycs] |> solve_and_check loc cf in
  let apply_sol                = ctype_apply is in
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

let mk_subty (loc: C.location) (ctv1: ctype) (sub: S.subst) (ctv2: ctype): cstr =
  mk_cstr loc (`CSubtype (ctv1, sub, ctv2))

let mk_heapinc (loc: C.location) (s: S.t) (hv: heapvar): cstr =
  mk_cstr loc (`CInHeap (s, hv))

let mk_locinc (loc: C.location) (i: index) (ctv: ctype) (s: S.t): cstr =
  mk_cstr loc (`CInLoc (i, ctv, s))

let mk_subheap (loc: C.location) (hv1: heapvar) (sub: S.subst) (hv2: heapvar): cstr =
  mk_cstr loc (`CSubheap (hv1, sub, hv2))

let mk_wfsubst (loc: C.location) (sub: S.subst): cstr =
  mk_cstr loc (`CWFSubst sub)

let ctype_of_const: C.constant -> ctype = function
  | C.CInt64 (v, ik, _) -> CTInt (C.bytesSizeOfInt ik, index_of_int (Int64.to_int v))
  | C.CChr c            -> CTInt (int_width, IInt (Char.code c))
  | c                   -> E.s <| E.bug "Unimplemented constrain_const: %a@!@!" C.d_const c

let rec constrain_exp_aux (env: env) (ctem: ctvemap) (loc: C.location): C.exp -> ctype * cstr list * S.t list * ctvemap = function
  | C.Const c                     -> let ctv = ctype_of_const c in (ctv, [], [], ctem)
  | C.Lval lv | C.StartOf lv      -> constrain_lval env ctem loc lv
  | C.UnOp (uop, e, t)            -> constrain_unop uop env ctem loc t e
  | C.BinOp (bop, e1, e2, t)      -> constrain_binop bop env ctem loc t e1 e2
  | C.CastE (C.TPtr _, C.Const c) -> constrain_constptr ctem loc c
  | C.CastE (ct, e)               -> constrain_cast env ctem loc ct e
  | C.SizeOf t                    -> constrain_sizeof ctem loc t
  | e                             -> E.s <| E.error "Unimplemented constrain_exp_aux: %a@!@!" C.d_exp e

and constrain_lval_aux ((_, hv, ve) as env: env) (ctem: ctvemap) (loc: C.location): C.lval -> ctype * cstr list * S.t list * ctvemap = function
  | (C.Var v, C.NoOffset)       -> (VM.find v ve, [], [], ctem)
  | (C.Mem e, C.NoOffset) as lv ->
      let (ctv, cs, ss, ctem) = constrain_exp env ctem loc e in
        begin match ctv with
          | CTRef (s, ie) ->
              let ctvlv = fresh_heaptype <| C.typeOfLval lv in
              let cs    = mk_locinc loc ie ctvlv s :: cs in
                (ctvlv, cs, M.maybe_cons (prectype_sloc ctvlv) (s :: ss), ctem)
          | _ -> E.s <| E.bug "constraining ref lval gave back non-ref type in constrain_lval@!@!"
        end
  | lv -> E.s <| E.bug "constrain_lval got lval with offset: %a@!@!" C.d_lval lv

and constrain_lval (env: env) (ctem: ctvemap) (loc: C.location) (lv: C.lval): ctype * cstr list * S.t list * ctvemap =
  let (ctv, cs, ss, ctem) = constrain_lval_aux env ctem loc lv in
    (ctv, cs, ss, ExpMap.add (C.Lval lv) ctv ctem)

and constrain_unop (op: C.unop) (env: env) (ctem: ctvemap) (loc: C.location) (t: C.typ) (e: C.exp): ctype * cstr list * S.t list * ctvemap =
  let (ctv, cs, ss, ctem) = constrain_exp env ctem loc e in
    match ctv with
      | CTInt _ -> (apply_unop t op, cs, ss, ctem)
      | _       -> E.s <| E.unimp "Haven't considered how to apply unops to references@!"

and apply_unop (rt: C.typ): C.unop -> ctype = function
  | C.LNot -> CTInt (CM.typ_width rt, ISeq (0, 1))
  | C.BNot -> CTInt (CM.typ_width rt, ITop)
  | C.Neg  -> CTInt (CM.typ_width rt, ITop)

and constrain_binop (op: C.binop) (env: env) (ctem: ctvemap) (loc: C.location) (t: C.typ) (e1: C.exp) (e2: C.exp): ctype * cstr list * S.t list * ctvemap =
  let (ctv1, cs1, ss1, ctem) = constrain_exp env ctem loc e1 in
  let (ctv2, cs2, ss2, ctem) = constrain_exp env ctem loc e2 in
  let ctv                    = apply_binop op t ctv1 ctv2 in
    (ctv, List.concat [cs1; cs2], List.concat [ss1; ss2], ctem)

and apply_binop: C.binop -> C.typ -> ctype -> ctype -> ctype = function
  | C.PlusA                                 -> apply_arithmetic index_plus
  | C.MinusA                                -> apply_arithmetic index_minus
  | C.Mult                                  -> apply_arithmetic index_mult
  | C.Div                                   -> apply_arithmetic index_div
  | C.PlusPI | C.IndexPI                    -> apply_ptrarithmetic (fun i1 x i2 -> index_plus i1 (index_scale x i2))
  | C.MinusPI                               -> apply_ptrarithmetic (fun i1 x i2 -> index_minus i1 (index_scale x i2))
  | C.MinusPP                               -> apply_ptrminus
  | C.Lt | C.Gt | C.Le | C.Ge | C.Eq | C.Ne -> apply_rel
  | C.Mod                                   -> apply_unknown
  | C.BAnd | C.BOr | C.BXor                 -> apply_unknown
  | C.Shiftlt | C.Shiftrt                   -> apply_unknown
  | bop                                     -> E.s <| E.bug "Unimplemented apply_binop: %a@!@!" C.d_binop bop

and apply_arithmetic (f: index -> index -> index) (rt: C.typ) (ctv1: ctype) (ctv2: ctype): ctype =
  match (ctv1, ctv2) with
    | (CTInt (n1, i1), CTInt (n2, i2)) -> CTInt (CM.typ_width rt, f i1 i2)
    | _                                -> E.s <| E.bug "Type mismatch in apply_arithmetic@!@!"

and apply_ptrarithmetic (f: index -> int -> index -> index) (pt: C.typ) (ctv1: ctype) (ctv2: ctype): ctype =
  match (C.unrollType pt, ctv1, ctv2) with
    | (C.TPtr (t, _), CTRef (s, i1), CTInt (n, i2)) when n = int_width -> CTRef (s, f i1 (CM.typ_width t) i2)
    | _                                                                -> E.s <| E.bug "Type mismatch in constrain_ptrarithmetic@!@!"

and apply_ptrminus (pt: C.typ) (_: ctype) (_: ctype): ctype =
  CTInt (CM.typ_width !C.upointType, ITop)

and apply_rel (_: C.typ) (_: ctype) (_: ctype): ctype =
  CTInt (int_width, ISeq (0, 1))

and apply_unknown (rt: C.typ) (_: ctype) (_: ctype): ctype =
  CTInt (CM.typ_width rt, ITop)

and constrain_exp (env: env) (ctem: ctvemap) (loc: C.location) (e: C.exp): ctype * cstr list * S.t list * ctvemap =
  let (ctv, cs, ss, ctem) = constrain_exp_aux env ctem loc e in
    (ctv, cs, ss, ExpMap.add e ctv ctem)

and constrain_constptr (ctem: ctvemap) (loc: C.location): C.constant -> ctype * cstr list * S.t list * ctvemap = function
  | C.CStr _                                 -> E.s <| E.unimp "Haven't implemented string constants yet"
  | C.CInt64 (v, ik, so) when v = Int64.zero -> let s = S.fresh S.Abstract in (CTRef (s, IBot), [], [s], ctem)
  | c                                        -> E.s <| C.errorLoc loc "Cannot cast non-zero, non-string constant %a to pointer@!@!" C.d_const c

and constrain_cast (env: env) (ctem: ctvemap) (loc: C.location) (ct: C.typ) (e: C.exp): ctype * cstr list * S.t list * ctvemap =
  let ect = constrain_exp_aux env ctem loc e in
    match (C.unrollType ct, C.unrollType <| C.typeOf e) with
      | (C.TInt (ik, _), C.TPtr _) -> (CTInt (C.bytesSizeOfInt ik, ITop), [], [], ctem)
      | (C.TInt (ik, _), C.TInt _) ->
          begin match ect with
            | (CTInt (n, ie), cs, ss, ctem) ->
                let iec =
                  if n <= C.bytesSizeOfInt ik then
                    (* pmr: what about the sign bit?  this may not always be safe *)
                    ie
                  else if not !Constants.safe then begin
                    C.warnLoc loc "Unsoundly assuming cast is lossless@!@!" |> ignore;
                    ie
                  end else
                    ITop
                in (CTInt (C.bytesSizeOfInt ik, iec), cs, ss, ctem)
            | _ -> E.s <| C.errorLoc loc "Got bogus type in contraining int-int cast@!@!"
          end
      | _ -> ect

and constrain_sizeof (ctem: ctvemap) (loc: C.location) (t: C.typ): ctype * cstr list * S.t list * ctvemap =
  (CTInt (int_width, IInt (C.bitsSizeOf t / 8)), [], [], ctem)

let constrain_return (env: env) (ctem: ctvemap) (rtv: ctype) (loc: C.location): C.exp option -> cstr list * S.t list * ctvemap * RA.block_annotation = function
    | None -> if is_void rtv then ([], [], ctem, []) else (C.errorLoc loc "Returning void value for non-void function\n\n" |> ignore; assert false)
    | Some e ->
        let (ctv, cs, ss, ctem) = constrain_exp env ctem loc e in
          (mk_subty loc ctv S.empty_subst rtv :: cs, ss, ctem, [])

let constrain_arg (env: env) (loc: C.location) (e: C.exp) ((ctvs, css, sss, ctem): ctype list * cstr list list * S.t list list * ctvemap): ctype list * cstr list list * S.t list list * ctvemap =
  let (ctv, cs, ss, ctem) = constrain_exp env ctem loc e in
    (ctv :: ctvs, cs :: css, ss :: sss, ctem)

let constrain_args (env: env) (ctem: ctvemap) (loc: C.location) (es: C.exp list): ctype list * cstr list list * S.t list list * ctvemap =
  let (ctvs, css, sss, ctem) = List.fold_right (constrain_arg env loc) es ([], [], [], ctem) in
    (ctvs, css, sss, ctem)

(* pmr: close functions over arg locations after each round of solving;
        compress list of qlocs;
        etc.
let instantiate_function (cf: cfun): S.subst * cfun =
  let sto = cf.args |> List.map snd |> M.map_partial prectype_sloc |> prestore_close_under cf.sto_out in
  let sub = sto |> prestore_domain |> List.map (fun s -> (s, S.fresh S.Abstract)) in
    (sub, {cf with sto_in = SLM.empty; sto_out = sto})
*)

let constrain_app ((fs, hv, _) as env: env) (ctem: ctvemap) (loc: C.location) (f: C.varinfo) (lvo: C.lval option) (args: C.exp list): cstr list list * S.t list list * ctvemap * RA.annotation list =
  let ctvs, css, sss, ctem = constrain_args env ctem loc args in
  let cf, _                = VM.find f fs in
  let instslocs            = List.map (fun _ -> S.fresh S.Abstract) cf.qlocs in
  let sub                  = List.combine cf.qlocs instslocs in
  let sss                  = instslocs :: sss in
  let ctvfs                = List.map snd cf.args in
  let stoincs              = prestore_fold (fun ics s i ct -> mk_locinc loc i (prectype_subs sub ct) (S.subst_apply sub s) :: ics) [] cf.sto_out in
  (* pmr: can probably eliminate some constraints by substituting with actual parameter locations directly when possible *)
  let css                  = (mk_wfsubst loc sub :: stoincs) ::
                             List.map2 (fun ctva ctvf -> mk_subty loc ctva sub ctvf) ctvs ctvfs ::
                             css in
    match lvo with
      | None    -> (css, sss, ctem, [])
      | Some lv ->
          let ctvlv, cs2, ss2, ctem = constrain_lval env ctem loc lv in
            ((mk_subty loc cf.ret sub ctvlv :: cs2) :: css, ss2 :: sss, ctem, [])

let printf_funs = ["printf"; "fprintf"]

let constrain_instr_aux (env: env) (ctem: ctvemap) ((ctem, css, sss, bas): ctvemap * cstr list list * S.t list list * RA.block_annotation): C.instr -> ctvemap * cstr list list * S.t list list * RA.block_annotation = function
  | C.Set (lv, e, loc) ->
      let ctv1, cs1, ss1, ctem = constrain_lval env ctem loc lv in
      let ctv2, cs2, ss2, ctem = constrain_exp env ctem loc e in
        (ctem, (mk_subty loc ctv2 S.empty_subst ctv1 :: cs1) :: cs2 :: css, ss1 :: ss2 :: sss, [] :: bas)
  | C.Call (None, C.Lval (C.Var {C.vname = f}, C.NoOffset), args, loc) when List.mem f printf_funs ->
      if not !Constants.safe then C.warnLoc loc "Unsoundly ignoring printf-style call to %s@!@!" f |> ignore else E.s <| C.errorLoc loc "Can't handle printf";
      let _, css2, sss, ctem = constrain_args env ctem loc args in
        (ctem, css2 @ css, sss, [] :: bas)
  | C.Call (lvo, C.Lval (C.Var f, C.NoOffset), args, loc) ->
      let css2, sss, ctem, ba = constrain_app env ctem loc f lvo args in
        (ctem, css2 @ css, sss, ba :: bas)
  | i -> E.s <| E.bug "Unimplemented constrain_instr: %a@!@!" C.dn_instr i

let constrain_instr (env: env) (ctem: ctvemap) (is: C.instr list): cstr list * S.t list * ctvemap * RA.block_annotation =
  let (ctem, css, sss, bas) = List.fold_left (constrain_instr_aux env ctem) (ctem, [], [], []) is in
    (List.concat css, List.concat sss, ctem, List.rev ([]::bas))

let constrain_stmt (env: env) (ctem: ctvemap) (rtv: ctype) (s: C.stmt): cstr list * S.t list * ctvemap * RA.block_annotation =
  match s.C.skind with
    | C.Instr is             -> constrain_instr env ctem is
    | C.If (e, _, _, loc)    -> let _, cs, ss, ctem = constrain_exp env ctem loc e in (cs, ss, ctem, []) (* we'll visit the subblocks later *)
    | C.Break _              -> ([], [], ctem, [])
    | C.Continue _           -> ([], [], ctem, [])
    | C.Goto _               -> ([], [], ctem, [])
    | C.Block _              -> ([], [], ctem, [])                              (* we'll visit this later as we iterate through blocks *)
    | C.Loop (_, _, _, _)    -> ([], [], ctem, [])                              (* ditto *)
    | C.Return (rexp, loc)   -> constrain_return env ctem rtv loc rexp
    | _                      -> E.s <| E.bug "Unimplemented constrain_stmt: %a@!@!" C.dn_stmt s

let constrain_phi_defs (ve: ctvenv) ((vphi, vdefs): C.varinfo * (int * C.varinfo) list): cstr list =
  List.map (fun (_, vdef) -> mk_subty vphi.C.vdecl (VM.find vdef ve) S.empty_subst (VM.find vphi ve)) vdefs

let constrain_phis (ve: ctvenv) (phis: (C.varinfo * (int * C.varinfo) list) list array): cstr list =
  Array.to_list phis |> List.flatten |> List.map (constrain_phi_defs ve) |> List.concat

let constrain_fun (fs: funenv) (hv: heapvar) ({ST.fdec = fd; ST.phis = phis; ST.cfg = cfg}: ST.ssaCfgInfo): ctvemap * RA.block_annotation array * S.t list * cstr list =
  let blocks         = cfg.Ssa.blocks in
  let bas            = Array.make (Array.length blocks) [] in
  let loc            = fd.C.svar.C.vdecl in
  let cf, ve         = VM.find fd.C.svar fs in
  let formalcs       = List.map2 (fun (_, fct) bv -> mk_subty loc fct S.empty_subst (VM.find bv ve)) cf.args fd.C.sformals in
  let phics          = constrain_phis ve phis in
  let ctem, sss, css =
    M.array_fold_lefti begin fun i (ctem, sss, css) b ->
      let (cs, ss, ctem, ba) = constrain_stmt (fs, hv, ve) ctem cf.ret b.Ssa.bstmt in
        Array.set bas i ba;
        (ctem, ss :: sss, cs :: css)
    end (ExpMap.empty, [], []) blocks
  in
  let vartys = VM.fold (fun _ ct cts -> ct :: cts) ve [] in
  let sss    = (cf.ret :: List.map snd cf.args @ vartys |> List.map prectype_sloc |> Misc.maybe_list) :: sss in
  let ss     = sss |> List.concat |> M.sort_and_compact in
  let cs     = List.map (fun s -> mk_heapinc loc s hv) ss :: formalcs :: phics :: css |> List.concat in
    P.printf "Constraints for %s:\n\n" fd.C.svar.C.vname;
    P.printf "%a\nheapvar   %a\n" d_cfun cf d_heapvar hv;
    P.printf "vars      %a\n" (P.d_list ", " S.d_sloc) ss;
    List.iter (fun c -> P.printf "%a\n" d_cstr c |> ignore) cs;
    P.printf "\n";
    (ctem, bas, ss, cs)

let constrain_scc (fs: funenv) (ae: annotenv) (scc: (C.varinfo * ST.ssaCfgInfo) list): funenv * annotenv * heapvar * cstr list =
  let fvs, scis             = List.split scc in
  let hv                    = fresh_heapvar () in
  let ctems, bass, sss, css = List.map (constrain_fun fs hv) scis |> M.split4 in
  let ss                    = List.concat sss in
    (* pmr: XXX: need to quantify over rather different locs *)
(*  let fs                  = List.fold_left (fun fs fv -> VM.add fv ({(VM.find fv fs |> fst) with qlocs = ss}, hv) fs) fs fvs in *)
  let ae                    = List.combine ctems bass |> List.fold_left2 (fun ae fv fa -> VM.add fv fa ae) ae fvs in
    (fs, ae, hv, List.concat css)

(******************************************************************************)
(************************** Constraint Simplification *************************)
(******************************************************************************)
(* The following assumes that all locations are quantified, i.e., that
   no location in a callee's SCC also appears in a caller's SCC. *)

module IMP = P.MakeMapPrinter(IM)

let d_cstrmap =
  IMP.d_map ~dmaplet:(fun d1 d2 -> P.dprintf "%t\t%t" (fun () -> d1) (fun () -> d2)) "\n" (fun () cid -> P.num cid) d_simplecstr

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
          IM.add hv1 (SS.union (slocset_map (S.subst_apply sub) ss2) ss1) hd
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
                     let subs = S.subst_apply sub s in
                       SCM.add subs (List.map (cstr_subst sub) (SCM.find s scm) @ SCM.find subs scm) scm) ss scm
    | _ -> scm

let is_not_inc (s: S.t) (c: cstr): bool =
  match c.cdesc with
    | `CInLoc (_, _, s2) -> not (S.eq s s2)
    | _                  -> true

let close_incs (hd: heapdom) (css: (heapvar * cstr list) list): cstr list list =
  let scm = List.fold_left (fun scm (_, cs) -> List.fold_left (close_inc hd) scm cs) SCM.empty css in
    SCM.fold (fun s cs css -> List.filter (is_not_inc s) cs :: css) scm (List.map snd css)

let simplify_cs (css: (heapvar * cstr list) list): heapdom * simplecstr list =
  let css = List.rev css in
  let hd  = mk_heapdom css in
  let cs  = css |> close_incs hd |> List.concat |> filter_simple_cstrs in
    (hd, cs)

(******************************************************************************)
(*************************** Constraint Dependencies **************************)
(******************************************************************************)

let mk_cstrmap (cs: simplecstr list): cstrmap =
  List.fold_left (fun cm c -> IM.add c.cid c cm) IM.empty cs

let add_slocdep (id: int) (sd: slocdep) (s: Sloc.t): slocdep =
  let depcs = try SLM.find s sd with Not_found -> [] in
    SLM.add s (id :: depcs) sd

let fresh_sloc_of (c: ctype): ctype =
  match c with
    | CTRef (_, i) -> CTRef (S.fresh S.Abstract, i)
    | CTInt _      -> c

let fresh_indextyping (it: Inferindices.indextyping): Inferindices.indextyping =
  VM.mapi (fun f (ifv, vm) -> if CM.definedHere f then (precfun_map fresh_sloc_of ifv, VM.map fresh_sloc_of vm) else (ifv, vm)) it

(******************************************************************************)
(***************************** Constraint Solving *****************************)
(******************************************************************************)

let solve_scc ((fs, ae, sd, cm, sub, sto): funenv * annotenv * slocdep * cstrmap * S.subst * store) (scc: (C.varinfo * ST.ssaCfgInfo) list): funenv * annotenv * slocdep * cstrmap * S.subst * store =
  let _                = if Cs.ck_olev Cs.ol_solve then P.printf "Solving scc [%a]...\n\n" (P.d_list "; " (fun () (fv, _) -> CM.d_var () fv)) scc |> ignore in
  let fs, ae, _, cs    = constrain_scc fs ae scc in
    (* pmr: obviously wrong *)
  let scs              = filter_simple_cstrs cs in
  let cm               = List.fold_left (fun cm sc -> IM.add sc.cid sc cm) cm scs in
  let sd               = List.fold_left (fun sd sc -> simplecstrdesc_slocs sc.cdesc |> List.fold_left (add_slocdep sc.cid) sd) sd scs in
  let sd, cm, sub, sto = solve sd cm sub sto in
  let _ = P.printf "SUBST: %a\n\n\n" S.d_subst sub in
  let _ = P.printf "STORE:\n\n%a\n\n" d_store sto in
    assert false

(* API *)
let infer_shapes (env: ctypeenv) (cg: Callgraph.t) (scim: ST.ssaCfgInfo SM.t): shape VM.t * ctypeenv =
  let fs                     = Inferindices.infer_indices env scim |> fresh_indextyping in
  let sccs                   = cg
                            |> List.filter (function [fv] -> SM.mem fv.C.vname scim | _ -> true)
                            |> List.rev_map (fun scc -> List.map (fun fv -> (fv, SM.find fv.C.vname scim)) scc) in
  let fs, ae, _, _, sub, sto = List.fold_left solve_scc (fs, VM.empty, SLM.empty, IM.empty, [], SLM.empty) sccs in
    assert false
    (*
pmr: after solving all SCCs, add store locations to fs according to the heap domains
pmr: when to compute heap doms?  *)

(* pmr: we need to solve after each scc

  let hd, cm      = simplify_cs css in
  let _           = P.printf "Heap domains:@!@!%a@!@!" d_heapdom hd in
  let _           = P.printf "Simplified closed constraints:@!@!%a@!@!" d_cstrmap cm in
    assert false
    *)
