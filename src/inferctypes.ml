module M   = Misc
module P   = Pretty
module C   = Cil
module Cs  = Constants
module E   = Errormsg
module ST  = Ssa_transform
module RA  = Refanno
module S   = Sloc
module SLM = S.SlocMap
module SS  = S.SlocSet
module CM  = CilMisc
module VM  = CM.VarMap
module SM  = M.StringMap
module FI  = FixInterface
module Ind = Inferindices
module SI  = ShapeInfra

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
(******************************** Environments ********************************)
(******************************************************************************)

module IM = M.IntMap

type ctvenv = ctype VM.t

type funenv = (cfun * ctvenv * heapvar) VM.t

let funenv_entry_of_cfun (cf: cfun): (cfun * ctvenv * heapvar) =
  (cf, VM.empty, fresh_heapvar ())

let ctenv_of_funenv (fe: funenv): cfun VM.t =
  VM.map fst3 fe

let funenv_of_ctenv (env: cfun VM.t): funenv =
  VM.fold (fun f cf fe -> VM.add f (funenv_entry_of_cfun cf) fe) env VM.empty

type env = funenv * heapvar * ctvenv

type ctvemap = ctype ExpMap.t

(******************************************************************************)
(********************************* Constraints ********************************)
(******************************************************************************)

type simplecstrdesc = [
| `CInLoc of index * ctype * S.t
| `CSubtype of ctype * ctype
| `CWFSubst of S.Subst.t
]

let simplecstrdesc_slocs: simplecstrdesc -> S.t list = function
  | `CInLoc (_, ct, s)   -> M.maybe_cons (prectype_sloc ct) [s]
  | `CSubtype (ct1, ct2) -> M.maybe_cons (prectype_sloc ct1) (M.maybe_cons (prectype_sloc ct2) [])
  | `CWFSubst (sub)      -> S.Subst.slocs sub

type cstrdesc = [
| simplecstrdesc
| `CInHeap of S.t * heapvar
| `CSubheap of heapvar * S.Subst.t * heapvar
]

let is_cstrdesc_simple: cstrdesc -> bool = function
  | `CInHeap _ | `CSubheap _ -> false
  | _                        -> true

let d_cstrdesc (): [< cstrdesc] -> P.doc = function
  | `CSubtype (ctv1, ctv2)    -> P.dprintf "@[@[%a@] <: @[%a@]@]" d_ctype ctv1 d_ctype ctv2
  | `CInHeap (s, hv)          -> P.dprintf "%a ∈ %a" S.d_sloc s d_heapvar hv
  | `CInLoc (i, ctv, s)       -> P.dprintf "(%a, %a) ∈ %a" d_index i d_ctype ctv S.d_sloc s
  | `CSubheap (hv1, sub, hv2) -> P.dprintf "%a <: %a %a" d_heapvar hv1 S.Subst.d_subst sub d_heapvar hv2
  | `CWFSubst (sub)           -> P.dprintf "WF(%a)" S.Subst.d_subst sub

let apply_subst_to_subst (appsub: S.Subst.t) (sub: S.Subst.t): S.Subst.t =
  List.map (fun (sfrom, sto) -> (S.Subst.apply appsub sfrom, S.Subst.apply appsub sto)) sub

let simplecstrdesc_subst (sub: S.Subst.t): simplecstrdesc -> simplecstrdesc = function
  | `CSubtype (ctv1, ctv2) -> `CSubtype (prectype_subs sub ctv1, prectype_subs sub ctv2)
  | `CInLoc (ie, ctv, s)   -> `CInLoc (ie, prectype_subs sub ctv, S.Subst.apply sub s)
  | `CWFSubst sub2         -> `CWFSubst (apply_subst_to_subst sub sub2)

let cstrdesc_subst (sub: S.Subst.t): cstrdesc -> cstrdesc = function
  | #simplecstrdesc as scd     -> (simplecstrdesc_subst sub scd :> cstrdesc)
  | `CSubheap (hv1, sub2, hv2) -> `CSubheap (hv1, apply_subst_to_subst sub sub2, hv2)
  | `CInHeap (s, hv)           -> `CInHeap (S.Subst.apply sub s, hv)

type 'a precstr = {cid: int; cdesc: 'a; cloc: C.location}

type cstr = cstrdesc precstr

type simplecstr = simplecstrdesc precstr

let (fresh_cstrid, reset_fresh_cstrids) = M.mk_int_factory ()

let mk_cstr (loc: C.location) (cdesc: 'a): 'a precstr =
  {cid = fresh_cstrid (); cloc = loc; cdesc = cdesc}

let simplecstr_subst (sub: S.Subst.t) (sc: simplecstr): simplecstr =
  mk_cstr sc.cloc (simplecstrdesc_subst sub sc.cdesc)

let cstr_subst (sub: S.Subst.t) (c: cstr): cstr =
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

(******************************************************************************)
(***************************** Constraint Solving *****************************)
(******************************************************************************)

exception Unify of ctype * ctype

let inloc_sat (st: store) (i: index) (s: S.t) (ct1: ctype): bool =
  match i with
    | IBot -> true
    | _    ->
        try
          match prestore_find_index s i st with
            | [ct2] -> prectype_eq ct1 ct2
            | []    -> false
            | _     -> halt <| C.bug "Prestore has multiple bindings for the same location"
        with Not_found -> false

let simplecstr_sat (st: store) (sc: simplecstr): bool =
  match sc.cdesc with
    | `CInLoc (i, ct, s)   -> inloc_sat st i s ct
    | `CSubtype (ct1, ct2) -> is_subctype ct1 ct2
    | `CWFSubst (sub)      -> S.Subst.well_defined sub && sub |> S.Subst.transpose |> S.Subst.well_defined

let unify_ctypes (ct1: ctype) (ct2: ctype) (sub: S.Subst.t): S.Subst.t =
  match ct1, ct2 with
    | CTRef (s1, _), CTRef (s2, _) when S.eq s1 s2 -> sub
    | CTRef (s1, _), CTRef (s2, _)                 -> S.Subst.extend s1 s2 sub
    | CTInt (n1, _), CTInt (n2, _) when n1 = n2    -> sub
    | _                                            -> raise (Unify (ct1, ct2))

let store_add (l: Sloc.t) (pl: ploc) (ctv: ctype) (sto: store): store =
  SLM.add l (LDesc.add pl ctv (prestore_find l sto)) sto

let refine_inloc (loc: C.location) (s: S.t) (i: index) (ct: ctype) (sto: store): S.Subst.t * store =
  try
    match i with
      | IBot   -> ([], sto)
      | IInt n ->
          let pl = PLAt n in
            begin match LDesc.find pl (prestore_find s sto) with
              | []         -> ([], store_add s pl ct sto)
              | [(_, ct2)] -> (unify_ctypes ct ct2 [], sto)
              | _          -> assert false
            end
      | ISeq (n, m) ->
          let ld, sub = LDesc.shrink_period m unify_ctypes [] (prestore_find s sto) in
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
                (sub, SLM.add s ld sto)
      | ITop ->
          let ld, sub = LDesc.shrink_period (prectype_width ct) unify_ctypes [] (prestore_find s sto) in
          let ld, sub = LDesc.foldn (fun _ (ld, sub) pl ct2 -> (LDesc.remove pl ld, unify_ctypes ct ct2 sub)) (ld, sub) ld in
            (sub, SLM.add s (LDesc.add PLEverywhere ct ld) sto)
  with
    | e ->
        C.errorLoc loc "Can't fit %a: %a in location %a |-> %a" d_index i d_ctype ct S.d_sloc s (LDesc.d_ldesc d_ctype) (prestore_find s sto) |> ignore;
        raise e

let unify_slocs: S.t list -> S.Subst.t = function
  | []      -> []
  | s :: ss -> List.fold_left (fun sub s' -> S.Subst.extend s' s sub) [] ss

let make_subst_well_defined (sub: S.Subst.t): S.Subst.t =
  sub |> S.Subst.images |> List.fold_left (fun outsub rng -> S.Subst.compose (unify_slocs rng) outsub) []

let refine_wfsubst (sub: S.Subst.t): S.Subst.t =
  S.Subst.compose (sub |> make_subst_well_defined) (sub |> S.Subst.transpose |> make_subst_well_defined)

let refine_aux (sc: simplecstr) (sto: store): S.Subst.t * store =
  try
    match sc.cdesc with
      | `CInLoc (i, ct, s)   -> refine_inloc sc.cloc s i ct sto
      | `CSubtype (ct1, ct2) -> (unify_ctypes ct1 ct2 S.Subst.empty, sto)
      | `CWFSubst (sub)      -> (refine_wfsubst sub, sto)
  with
    | Unify (ct1, ct2) -> halt <| C.errorLoc sc.cloc "Cannot unify %a with %a\n\n" d_ctype ct1 d_ctype ct2

type slocdep = int list SLM.t (* sloc -> cstr deps *)

type cstrmap = simplecstr IM.t (* cstr ident -> cstr map *)

let cstrmap_subs (sub: S.Subst.t) (cm: cstrmap): cstrmap =
  IM.map (simplecstr_subst sub) cm

(* pmr: should make sure that subst is well-defined at this point *)
let adjust_slocdep (sub: S.Subst.t) (sd: slocdep): slocdep =
  List.fold_left begin fun sd (sfrom, sto) ->
    (* pmr: this find pattern is common; should be using a MapWithDefault *)
    let olddeps = try SLM.find sfrom sd with Not_found -> [] in
    let newdeps = try SLM.find sto sd with Not_found -> [] in
      sd |> SLM.remove sfrom |> SLM.add sto (newdeps @ olddeps)
  end sd sub

let refine (sd: slocdep) (cm: cstrmap) (sub: S.Subst.t) (sc: simplecstr) (sto: store): slocdep * cstrmap * S.Subst.t * store * int list =
  let refsub, sto   = refine_aux sc sto in
  let invalid_slocs = S.Subst.slocs refsub in
  let sto           = invalid_slocs |> List.fold_left (fun sto s -> SLM.remove s sto) sto |> prestore_subs refsub in
  let succs         = invalid_slocs |> M.flap (fun ivs -> try SLM.find ivs sd with Not_found -> (P.printf "Couldn't find dep for %a\n\n" S.d_sloc ivs; assert false)) in
  let sd            = adjust_slocdep refsub sd in
  let cm            = cstrmap_subs refsub cm in
    (sd, cm, S.Subst.compose refsub sub, sto, succs)

(* pmr: pull out worklist code? *)
module WkList = Heaps.Functional(struct
                                   type t = int
                                   let compare = compare
                                 end)

let wklist_push (wkl: WkList.t) (itcids: int list): WkList.t =
  List.fold_left (M.flip WkList.add) wkl itcids

let rec iter_solve (wkl: WkList.t) (sd: slocdep) (cm: cstrmap) (sub: S.Subst.t) (sto: store): slocdep * cstrmap * S.Subst.t * store =
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

let solve (sd: slocdep) (cm: cstrmap) (sto: store): slocdep * cstrmap * S.Subst.t * store =
  let wkl = IM.fold (fun cid _ cids -> cid :: cids) cm [] |> wklist_push WkList.empty in
    iter_solve wkl sd cm S.Subst.empty sto

(******************************************************************************)
(***************************** CIL Types to CTypes ****************************)
(******************************************************************************)

let mk_subty (ctv1: ctype) (ctv2: ctype): cstr =
  mk_cstr !C.currentLoc (`CSubtype (ctv1, ctv2))

let mk_heapinc (s: S.t) (hv: heapvar): cstr =
  mk_cstr !C.currentLoc (`CInHeap (s, hv))

let mk_locinc (i: index) (ctv: ctype) (s: S.t): cstr =
  mk_cstr !C.currentLoc (`CInLoc (i, ctv, s))

let mk_subheap (hv1: heapvar) (sub: S.Subst.t) (hv2: heapvar): cstr =
  mk_cstr !C.currentLoc (`CSubheap (hv1, sub, hv2))

let mk_wfsubst (sub: S.Subst.t): cstr =
  mk_cstr !C.currentLoc (`CWFSubst sub)

let rec constrain_exp (env: env) (em: ctvemap) (e: C.exp): ctype * ctvemap * cstr list * S.t list =
  let ct, em, cs, ss = constrain_exp_aux env em e in
    (ct, ExpMap.add e ct em, cs, ss)

and constrain_exp_aux (env: env) (em: ctvemap): C.exp -> ctype * ctvemap * cstr list * S.t list = function
  | C.Const c                     -> let ctv = ctype_of_const c in (ctv, em, [], [])
  | C.Lval lv | C.StartOf lv      -> constrain_lval env em lv
  | C.UnOp (uop, e, t)            -> constrain_unop uop env em t e
  | C.BinOp (bop, e1, e2, t)      -> constrain_binop bop env em t e1 e2
  | C.CastE (C.TPtr _, C.Const c) -> let ctv, cs, ss = constrain_constptr c in (ctv, em, cs, ss)
  | C.CastE (ct, e)               -> constrain_cast env em ct e
  | C.SizeOf t                    -> constrain_sizeof em t
  | e                             -> E.s <| C.error "Unimplemented constrain_exp_aux: %a@!@!" C.d_exp e

and constrain_lval ((_, hv, ve) as env: env) (em: ctvemap): C.lval -> ctype * ctvemap * cstr list * S.t list = function
  | (C.Var v, C.NoOffset)       -> (VM.find v ve, em, [], [])
  | (C.Mem e, C.NoOffset) as lv ->
      let ctv, em, cs, ss = constrain_exp env em e in
        begin match ctv with
          | CTRef (s, ie) ->
              let ctvlv = lv |> C.typeOfLval |> SI.fresh_heaptype in
              let cs    = mk_locinc ie ctvlv s :: cs in
                (ctvlv, em, cs, M.maybe_cons (prectype_sloc ctvlv) (s :: ss))
          | _ -> E.s <| C.bug "constraining ref lval gave back non-ref type in constrain_lval@!@!"
        end
  | lv -> E.s <| C.bug "constrain_lval got lval with offset: %a@!@!" C.d_lval lv

and constrain_unop (op: C.unop) (env: env) (em: ctvemap) (t: C.typ) (e: C.exp): ctype * ctvemap * cstr list * S.t list =
  let ctv, em, cs, ss = constrain_exp env em e in
    match ctv with
      | CTInt _ -> (apply_unop t op, em, cs, ss)
      | _       -> E.s <| C.error "Unimplemented: Haven't considered how to apply unops to references@!"

and apply_unop (rt: C.typ): C.unop -> ctype = function
  | C.LNot -> CTInt (CM.typ_width rt, ISeq (0, 1))
  | C.BNot -> CTInt (CM.typ_width rt, ITop)
  | C.Neg  -> CTInt (CM.typ_width rt, ITop)

and constrain_binop (op: C.binop) (env: env) (em: ctvemap) (t: C.typ) (e1: C.exp) (e2: C.exp): ctype * ctvemap * cstr list * S.t list =
  let ctv1, em, cs1, ss1 = constrain_exp env em e1 in
  let ctv2, em, cs2, ss2 = constrain_exp env em e2 in
  let ctv                = apply_binop op t ctv1 ctv2 in
    (ctv, em, List.concat [cs1; cs2], List.concat [ss1; ss2])

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
  | bop                                     -> E.s <| C.bug "Unimplemented apply_binop: %a@!@!" C.d_binop bop

and apply_arithmetic (f: index -> index -> index) (rt: C.typ) (ctv1: ctype) (ctv2: ctype): ctype =
  match (ctv1, ctv2) with
    | (CTInt (n1, i1), CTInt (n2, i2)) -> CTInt (CM.typ_width rt, f i1 i2)
    | _                                -> E.s <| C.bug "Type mismatch in apply_arithmetic@!@!"

and apply_ptrarithmetic (f: index -> int -> index -> index) (pt: C.typ) (ctv1: ctype) (ctv2: ctype): ctype =
  match (C.unrollType pt, ctv1, ctv2) with
    | (C.TPtr (t, _), CTRef (s, i1), CTInt (n, i2)) when n = CM.int_width -> CTRef (s, f i1 (CM.typ_width t) i2)
    | _                                                                   -> E.s <| C.bug "Type mismatch in constrain_ptrarithmetic@!@!"

and apply_ptrminus (pt: C.typ) (_: ctype) (_: ctype): ctype =
  CTInt (CM.typ_width !C.upointType, ITop)

and apply_rel (_: C.typ) (_: ctype) (_: ctype): ctype =
  CTInt (CM.int_width, ISeq (0, 1))

and apply_unknown (rt: C.typ) (_: ctype) (_: ctype): ctype =
  CTInt (CM.typ_width rt, ITop)

and constrain_constptr: C.constant -> ctype * cstr list * S.t list = function
  | C.CStr _                                 -> E.s <| C.unimp "Haven't implemented string constants yet"
  | C.CInt64 (v, ik, so) when v = Int64.zero -> let s = S.fresh S.Abstract in (CTRef (s, IBot), [], [s])
  | c                                        -> E.s <| C.error "Cannot cast non-zero, non-string constant %a to pointer@!@!" C.d_const c

and constrain_cast (env: env) (em: ctvemap) (ct: C.typ) (e: C.exp): ctype * ctvemap * cstr list * S.t list =
  let ctv, em, cs, ss = constrain_exp env em e in
    match C.unrollType ct, C.unrollType <| C.typeOf e with
      | C.TInt (ik, _), C.TPtr _     -> (CTInt (C.bytesSizeOfInt ik, ITop), em, cs, ss)
      | C.TFloat (fk, _), C.TFloat _ -> (CTInt (CM.bytesSizeOfFloat fk, ITop), em, cs, ss)
      | C.TInt (ik, _), C.TInt _     ->
          begin match ctv with
            | CTInt (n, ie) ->
                let iec =
                  if n <= C.bytesSizeOfInt ik then
                    (* pmr: what about the sign bit?  this may not always be safe *)
                    if C.isSigned ik then ie else index_unsign ie
                  else if not !Constants.safe then begin
                    C.warn "Unsoundly assuming cast is lossless@!@!" |> ignore;
                    if C.isSigned ik then ie else index_unsign ie
                  end else
                    ITop
                in (CTInt (C.bytesSizeOfInt ik, iec), em, cs, ss)
            | _ -> E.s <| C.error "Got bogus type in contraining int-int cast@!@!"
          end
      | _ -> (ctv, em, cs, ss)

and constrain_sizeof (em: ctvemap) (t: C.typ): ctype * ctvemap * cstr list * S.t list =
  (CTInt (CM.int_width, IInt (CM.typ_width t)), em, [], [])

(* pmr: move loc earlier *)
let constrain_return (env: env) (em: ctvemap) (rtv: ctype): C.exp option -> ctvemap * RA.block_annotation * cstr list * S.t list = function
    | None   -> if is_void rtv then (em, [], [], []) else (C.error "Returning void value for non-void function\n\n" |> ignore; assert false)
    | Some e ->
        let ctv, em, cs, ss = constrain_exp env em e in
          (em, [], mk_subty ctv rtv :: cs, ss)

let constrain_arg (env: env) (e: C.exp) ((ctvs, em, css, sss): ctype list * ctvemap * cstr list list * S.t list list): ctype list * ctvemap * cstr list list * S.t list list =
  let ctv, em, cs, ss = constrain_exp env em e in
    (ctv :: ctvs, em, cs :: css, ss :: sss)

let constrain_args (env: env) (em: ctvemap) (es: C.exp list): ctype list * ctvemap * cstr list list * S.t list list =
  List.fold_right (constrain_arg env) es ([], em, [], [])

let constrain_app ((fs, hv, _) as env: env) (em: ctvemap) (f: C.varinfo) (lvo: C.lval option) (args: C.exp list): ctvemap * RA.annotation list * cstr list list * S.t list list =
  let ctvs, em, css, sss = constrain_args env em args in
  let cf, _, hvf     = VM.find f fs in
  let instslocs      = List.map (fun _ -> S.fresh S.Abstract) cf.qlocs in
  let annot          = List.map2 (fun sfrom sto -> RA.New (sfrom, sto)) cf.qlocs instslocs in
  let sub            = List.combine cf.qlocs instslocs in
  let sss            = instslocs :: sss in
  let ctvfs          = List.map (prectype_subs sub <.> snd) cf.args in
  let stoincs        = prestore_fold (fun ics s i ct -> mk_locinc i (prectype_subs sub ct) (S.Subst.apply sub s) :: ics) [] cf.sto_out in
  let css            = (mk_wfsubst sub :: mk_subheap hv sub hvf :: stoincs) ::
                        List.map2 (fun ctva ctvf -> mk_subty ctva ctvf) ctvs ctvfs ::
                        css in
    match lvo with
      | None    -> (em, annot, css, sss)
      | Some lv ->
          let ctvlv, em, cs2, ss2 = constrain_lval env em lv in
          let em              = ExpMap.add (C.Lval lv) ctvlv em in
            (em, annot, (mk_subty (prectype_subs sub cf.ret) ctvlv :: cs2) :: css, ss2 :: sss)

let constrain_instr_aux (env: env) ((em, bas, css, sss): ctvemap * RA.block_annotation * cstr list list * S.t list list) (i: C.instr): ctvemap * RA.block_annotation * cstr list list * S.t list list =
  let _ = C.currentLoc := C.get_instrLoc i in
    match i with
      | C.Set (lv, e, _) ->
          let ctv1, em, cs1, ss1 = constrain_lval env em lv in
          let ctv2, em, cs2, ss2 = constrain_exp env em e in
            (em, [] :: bas, (mk_subty ctv2 ctv1 :: cs1) :: cs2 :: css, ss1 :: ss2 :: sss)
      | C.Call (None, C.Lval (C.Var f, C.NoOffset), args, _) when CM.isVararg f.C.vtype ->
          if not !Constants.safe then C.warn "Unsoundly ignoring vararg call to %a@!@!" CM.d_var f |> ignore else E.s <| C.error "Can't handle varargs";
          let _, em, css2, sss = constrain_args env em args in
            (em, [] :: bas, css2 @ css, sss)
      | C.Call (lvo, C.Lval (C.Var f, C.NoOffset), args, _) ->
          let em, ba, css2, sss = constrain_app env em f lvo args in
            (em, ba :: bas, css2 @ css, sss)
      | i -> E.s <| C.bug "Unimplemented constrain_instr: %a@!@!" C.dn_instr i

let constrain_instr (env: env) (em: ctvemap) (is: C.instr list): ctvemap * RA.block_annotation * cstr list * S.t list =
  let em, bas, css, sss = List.fold_left (constrain_instr_aux env) (em, [], [], []) is in
    (em, List.rev ([] :: bas), List.concat css, List.concat sss)

let constrain_stmt (env: env) (em: ctvemap) (rtv: ctype) (s: C.stmt): ctvemap * RA.block_annotation * cstr list * S.t list =
  let _ = C.currentLoc := C.get_stmtLoc s.C.skind in
    match s.C.skind with
      | C.Instr is          -> constrain_instr env em is
      | C.If (e, _, _, _)   -> let _, em, cs, ss = constrain_exp env em e in (em, [], cs, ss) (* we'll visit the subblocks later *)
      | C.Break _           -> (em, [], [], [])
      | C.Continue _        -> (em, [], [], [])
      | C.Goto _            -> (em, [], [], [])
      | C.Block _           -> (em, [], [], [])                              (* we'll visit this later as we iterate through blocks *)
      | C.Loop (_, _, _, _) -> (em, [], [], [])                              (* ditto *)
      | C.Return (rexp, _)  -> constrain_return env em rtv rexp
      | _                   -> E.s <| C.bug "Unimplemented constrain_stmt: %a@!@!" C.dn_stmt s

let constrain_phi_defs (ve: ctvenv) ((vphi, vdefs): C.varinfo * (int * C.varinfo) list): cstr list =
  let _ = C.currentLoc := vphi.C.vdecl in
    List.map (fun (_, vdef) -> mk_subty (VM.find vdef ve) (VM.find vphi ve)) vdefs

let constrain_phis (ve: ctvenv) (phis: (C.varinfo * (int * C.varinfo) list) list array): cstr list =
  Array.to_list phis |> List.flatten |> List.map (constrain_phi_defs ve) |> List.concat

let constrain_fun (fs: funenv) (cf: cfun) (ve: ctvenv) (hv: heapvar) ({ST.fdec = fd; ST.phis = phis; ST.cfg = cfg}: ST.ssaCfgInfo): ctvemap * RA.block_annotation array * S.t list * cstr list =
  let _          = C.currentLoc := fd.C.svar.C.vdecl in
  let formalcs   = List.map2 (fun (_, fct) bv -> mk_subty fct (VM.find bv ve)) cf.args fd.C.sformals in
  let phics      = constrain_phis ve phis in
  let blocks     = cfg.Ssa.blocks in
  let bas        = Array.make (Array.length blocks) [] in
  let em, sss, css =
    M.array_fold_lefti begin fun i (em, sss, css) b ->
      let em, ba, cs, ss = constrain_stmt (fs, hv, ve) em cf.ret b.Ssa.bstmt in
        Array.set bas i ba;
        (em, ss :: sss, cs :: css)
    end (ExpMap.empty, [], []) blocks
  in
  let _      = C.currentLoc := fd.C.svar.C.vdecl in
  let vartys = VM.fold (fun _ ct cts -> ct :: cts) ve [] in
  let sss    = (cf.ret :: List.map snd cf.args @ vartys |> List.map prectype_sloc |> Misc.maybe_list) :: sss in
  let ss     = sss |> List.concat |> M.sort_and_compact in
  let cs     = List.map (fun s -> mk_heapinc s hv) ss :: formalcs :: phics :: css |> List.concat in
  let _      =
    if Cs.ck_olev Cs.ol_solve then begin
      P.printf "Constraints for %s:\n\n" fd.C.svar.C.vname;
      P.printf "%a\nheapvar   %a\n" d_cfun cf d_heapvar hv;
      P.printf "locs      %a\n" (P.d_list ", " S.d_sloc) ss;
      List.iter (fun c -> P.printf "%a\n" d_cstr c |> ignore) cs;
      P.printf "\n" |> ignore
    end else ()
  in (em, bas, ss, cs)

type scc = (C.varinfo * ST.ssaCfgInfo) list

let constrain_sci (fs: funenv) (sci: ST.ssaCfgInfo): ctvemap * RA.block_annotation array * S.t list * cstr list =
  let cf, ve, hv = VM.find sci.ST.fdec.C.svar fs in
    constrain_fun fs cf ve hv sci

let constrain_scc (fs: funenv) (scc: scc): funenv * S.t list * cstr list =
  let fvs, scis      = List.split scc in
  let _, _, sss, css = List.map (constrain_sci fs) scis |> M.split4 in
    (fs, List.concat sss, List.concat css)

(******************************************************************************)
(*************************** Constraint Dependencies **************************)
(******************************************************************************)

let mk_cstrmap (scs: simplecstr list): cstrmap =
  List.fold_left (fun cm sc -> IM.add sc.cid sc cm) IM.empty scs

let add_slocdep (id: int) (sd: slocdep) (s: Sloc.t): slocdep =
  let depcs = try SLM.find s sd with Not_found -> [] in
    SLM.add s (id :: depcs) sd

let fresh_sloc_of (c: ctype): ctype =
  match c with
    | CTRef (_, i) -> CTRef (S.fresh S.Abstract, i)
    | CTInt _      -> c

(******************************************************************************)
(***************************** Constraint Solving *****************************)
(******************************************************************************)

let funenv_apply_subs (sub: S.Subst.t) (fe: funenv): funenv =
  VM.map (fun (cf, ve, hv) -> (cfun_subs sub cf, VM.map (prectype_subs sub) ve, hv)) fe

let cfun_generalize (sto: store) (cf: cfun): cfun =
  let argrootlocs = List.map (prectype_sloc <.> snd) cf.args |> M.maybe_list in
  let sto_out     = prestore_close_under sto (M.maybe_cons (prectype_sloc cf.ret) argrootlocs) in
  let sto_in      = prestore_close_under sto_out argrootlocs in
    {cf with sto_in = sto_in; sto_out = sto_out; qlocs = prestore_domain sto_out}

let d_funenv () (fe: funenv): P.doc =
     fe
  |> M.flip (VM.fold (fun f t it -> if CM.definedHere f then VM.add f t it else it)) VM.empty
  |> CM.VarMapPrinter.d_map
      ~dmaplet:(fun d1 d2 -> P.dprintf "%t\n%t" (fun () -> d1) (fun () -> d2))
      "\n\n"
      CM.d_var
      (fun () (cf, vm, hv) -> P.dprintf "%a\nheap      %a\n\nLocals:\n%a\n\n" d_cfun cf d_heapvar hv (CM.VarMapPrinter.d_map "\n" CM.d_var d_ctype) vm) ()

let fresh_scc (it: Ind.indextyping) (fe: funenv) (scc: scc): funenv =
  let hv = fresh_heapvar () in
    List.fold_left begin fun fe (f, _) ->
      let (ifv, vm) = VM.find f it in
        VM.add f (precfun_map fresh_sloc_of ifv, VM.map fresh_sloc_of vm, hv) fe
    end fe scc

let d_scc () (scc: scc): P.doc =
  (P.d_list "; " (fun () (fv, _) -> CM.d_var () fv)) () scc

let update_deps (scs: simplecstr list) (cm: cstrmap) (sd: slocdep): cstrmap * slocdep =
  let cm = List.fold_left (fun cm sc -> IM.add sc.cid sc cm) cm scs in
  let sd = List.fold_left (fun sd sc -> simplecstrdesc_slocs sc.cdesc |> List.fold_left (add_slocdep sc.cid) sd) sd scs in
    (cm, sd)

let solve_scc (it: Ind.indextyping) ((fs, sd, cm, sto): funenv * slocdep * cstrmap * store) (scc: scc): funenv * slocdep * cstrmap * store =
  let _                = if Cs.ck_olev Cs.ol_solve then P.printf "Solving scc [%a]...\n\n" d_scc scc |> ignore in
  let fs               = fresh_scc it fs scc in
  let fs, ss, cs       = constrain_scc fs scc in
  let scs              = filter_simple_cstrs cs in
  let cm, sd           = update_deps scs cm sd in
  let sd, cm, sub, sto = solve sd cm sto in
  (* Can't be moved earlier because locations may be thrown out by solve when unifying *)
  let sto              = ss
                      |> List.map (S.Subst.apply sub)
                      |> List.fold_left (fun sto s -> if SLM.mem s sto then sto else SLM.add s LDesc.empty sto) sto in
  let fs               = fs
                      |> funenv_apply_subs sub
                      |> VM.mapi begin fun f ft ->
                           if CM.definedHere f then
                             M.app_fst3 (cfun_generalize sto) ft
                           else
                             ft
                         end in
  let _ =
    if Cs.ck_olev Cs.ol_solve then begin
      P.printf "SUBST: %a\n\n\n" S.Subst.d_subst sub;
      P.printf "STORE:\n\n%a\n\n" d_store sto;
      P.printf "ENV:\n\n%a\n\n" d_funenv fs |> ignore
    end else ()
  in (fs, sd, cm, sto)

let infer_spec (env: cfun VM.t) (cg: Callgraph.t) (scim: Ssa_transform.ssaCfgInfo CilMisc.VarMap.t): (string * cfun) list =
  let it          = Ind.infer_indices env scim in
  let cg          = List.filter (function [fv] -> CM.definedHere fv | _ -> true) cg in
  let sccs        = List.rev_map (fun scc -> List.map (fun fv -> (fv, VM.find fv scim)) scc) cg in
  let fs          = funenv_of_ctenv env in
  let fs, _, _, _ = List.fold_left (solve_scc it) (fs, SLM.empty, IM.empty, SLM.empty) sccs in
    VM.fold begin fun f (cf, _, _) spec ->
      if CM.definedHere f then
        (f.C.vname, cf) :: spec
      else
        spec
    end fs []

let add_builtin (cil: C.file) (fn: string) ((rf, _): FI.refcfun * bool) (env: cfun VM.t): cfun VM.t =
  let fv = C.findOrCreateFunc cil fn C.voidType in
    if CM.definedHere fv then env else VM.add fv (FI.cfun_of_refcfun rf) env

(* API *)
let specs_of_file spec cil =
  let cg   = Callgraph.sccs cil in
  let scis = cil |> ST.scis_of_file |> List.fold_left (fun scim sci -> VM.add sci.ST.fdec.C.svar sci scim) VM.empty in
  let env  = SM.fold (add_builtin cil) spec VM.empty in
    infer_spec env cg scis

(******************************************************************************)
(**************************** Local Shape Inference ***************************)
(******************************************************************************)

type shape =
  {vtyps : (Cil.varinfo * Ctypes.ctype) list;
   etypm : Ctypes.ctemap;
   store : Ctypes.store;
   anna  : Refanno.block_annotation array;
   theta : Refanno.ctab }

exception Unify of S.t * S.t

let match_slocs (ct1: ctype) (ct2: ctype): unit =
  match ct1, ct2 with
    | CTRef (s1, _), CTRef (s2, _) when not (S.eq s1 s2) -> raise (Unify (s1, s2))
    | _                                                  -> ()

let check_expected_type (etyp: ctype) (atyp: ctype): bool =
  match_slocs atyp etyp;
  if is_subctype atyp etyp then
    true
  else begin
    C.error "Expected type %a, but got type %a\n\n" d_ctype etyp d_ctype atyp |> ignore;
    false
  end

let check_out_store_complete (sto_out_formal: store) (sto_out_actual: store): bool =
  prestore_fold begin fun ok l i ct ->
    if SLM.mem l sto_out_formal && prestore_find_index l i sto_out_formal = [] then begin
      C.error "Actual store has binding %a |-> %a: %a, missing from spec for %a\n\n" S.d_sloc l d_index i d_ctype ct S.d_sloc l |> ignore;
      false
    end else
      ok
  end true sto_out_actual

let check_out_store (sto_out_formal: store) (sto_out_actual: store): bool =
  check_out_store_complete sto_out_formal sto_out_actual &&
    prestore_fold begin fun ok l i ft ->
      try
        match prestore_find_index l i sto_out_actual with
          | []   -> not (SLM.mem l sto_out_actual)
          | [at] -> check_expected_type ft at && ok (* order is important here for unification! *)
          | _    -> failwith "Returned too many at index from prestore_find_index"
      with
        | Unify (s1, s2) -> raise (Unify (s1, s2))
        | Not_found      -> ok
        | _              -> false
    end true sto_out_formal

let revert_spec_names (subaway: S.Subst.t) (cfspec: cfun): S.Subst.t =
     cfspec.sto_out
  |> prestore_domain
  |> List.fold_left (fun sub s -> S.Subst.extend (S.Subst.apply subaway s) s sub) []

type solstate = ctype VM.t * ctvemap * RA.block_annotation array

type soln = store * ctype VM.t * ctvemap * RA.block_annotation array

let subst_solstate (sub: S.Subst.t) ((vars, em, bas): solstate): solstate =
  (VM.map (prectype_subs sub) vars, ExpMap.map (prectype_subs sub) em, Array.map (RA.subs sub) bas)

let rec solve_and_check (cf: cfun) (vars: ctype VM.t) (em: ctvemap) (bas: RA.block_annotation array) (sd: slocdep) (cm: cstrmap): soln =
  let sd, cm, sub, sto = solve sd cm SLM.empty in
  let revsub           = revert_spec_names sub cf in
  let sto              = prestore_subs revsub sto in
  let sd               = adjust_slocdep revsub sd in
  let cm               = cstrmap_subs revsub cm in
  let sub              = S.Subst.compose revsub sub in
  let vars, em, bas    = subst_solstate sub (vars, em, bas) in
    try
      if check_out_store cf.sto_out sto then
        (sto, vars, em, bas)
      else
        E.s <| C.error "Failed checking store typing:\nStore:\n%a\n\ndoesn't match expected type:\n\n%a\n\n" d_store sto d_cfun cf
    with Unify (s1, s2) ->
      let sub           = [(s1, s2)] in
      let vars, em, bas = subst_solstate sub (vars, em, bas) in
      let sd            = adjust_slocdep sub sd in
      let cm            = cstrmap_subs sub cm in
        solve_and_check cf vars em bas sd cm

let d_vartypes () vars =
  P.docList ~sep:(P.dprintf "@!") (fun (v, ct) -> P.dprintf "%s: %a" v.C.vname Ctypes.d_ctype ct) () vars

let print_shape (fname: string) (cf: cfun) ({vtyps = locals; store = st}: shape) (ds: Ind.dcheck list): unit =
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
  let _ = P.printf "%a@!@!" (P.d_list "\n" Ind.d_dcheck) ds in
    ()

let infer_shape (fe: funenv) (scim: Ssa_transform.ssaCfgInfo CilMisc.VarMap.t) (cf: cfun) (sci: ST.ssaCfgInfo): shape * Ind.dcheck list =
  let ve, ds              = sci |> Ind.infer_fun_indices (ctenv_of_funenv fe) scim cf |> M.app_fst (VM.map fresh_sloc_of) in
  let em, bas, _, cs      = constrain_fun fe cf ve (fresh_heapvar ()) sci in
  let cs                  = prestore_fold (fun cs l i ct -> mk_locinc i ct l :: cs) cs cf.sto_in in
  let scs                 = filter_simple_cstrs cs in
  let cm, sd              = update_deps scs IM.empty SLM.empty in
  let _                   = C.currentLoc := sci.ST.fdec.C.svar.C.vdecl in
  let sto, vtyps, em, bas = solve_and_check cf ve em bas sd cm in
  let sto                 = prestore_fold (fun sto _ _ -> function CTRef (s, _) -> if SLM.mem s sto then sto else SLM.add s LDesc.empty sto | _ -> sto) sto sto in
  let annot, theta        = RA.annotate_cfg sci.ST.cfg em bas in
  let shp                 = {vtyps = CM.vm_to_list vtyps;
                             etypm = em;
                             store = sto;
                             anna  = annot;
                             theta = theta} in
    if !Cs.verbose_level >= Cs.ol_ctypes || !Cs.ctypes_only then print_shape sci.ST.fdec.C.svar.C.vname cf shp ds;
    (shp, ds)

type funmap = (cfun * Ssa_transform.ssaCfgInfo) SM.t

(* API *)
let infer_shapes (cil: C.file) (env: ctypeenv) (scis: (cfun * ST.ssaCfgInfo) SM.t): (shape * Ind.dcheck list) SM.t =
  let fe = VM.empty
        |> SM.fold (fun f cf fe -> VM.add (C.findOrCreateFunc cil f C.voidType) (funenv_entry_of_cfun cf) fe) env
        |> SM.fold (fun _ (cf, {ST.fdec = fd}) fe -> VM.add fd.C.svar (funenv_entry_of_cfun cf) fe) scis in
  let scim = SM.fold (fun _ (_, sci) scim -> VM.add sci.ST.fdec.C.svar sci scim) scis VM.empty in
    scis |> SM.map (infer_shape fe scim |> M.uncurry)
