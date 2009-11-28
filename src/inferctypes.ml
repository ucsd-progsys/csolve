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
(******************************** Environments ********************************)
(******************************************************************************)

module IM = M.IntMap

type ctvenv = ctype VM.t

type funenv = (cfun * ctvenv) VM.t

let funenv_entry_of_cfun (cf: cfun): (cfun * ctvenv) =
  (cf, VM.empty)

let ctenv_of_funenv (fe: funenv): cfun VM.t =
  VM.map fst fe

let funenv_of_ctenv (env: cfun VM.t): funenv =
  VM.fold (fun f cf fe -> VM.add f (funenv_entry_of_cfun cf) fe) env VM.empty

type env = funenv * ctvenv

type ctvemap = ctype ExpMap.t

(******************************************************************************)
(********************************* Constraints ********************************)
(******************************************************************************)

type cstrdesc =
  | CInLoc of index * ctype * S.t
  | CSubtype of ctype * ctype
  | CWFSubst of S.Subst.t

let cstrdesc_slocs: cstrdesc -> S.t list = function
  | CInLoc (_, ct, s)   -> M.maybe_cons (prectype_sloc ct) [s]
  | CSubtype (ct1, ct2) -> M.maybe_cons (prectype_sloc ct1) (M.maybe_cons (prectype_sloc ct2) [])
  | CWFSubst (sub)      -> S.Subst.slocs sub

let d_cstrdesc (): cstrdesc -> P.doc = function
  | CSubtype (ctv1, ctv2) -> P.dprintf "@[@[%a@] <: @[%a@]@]" d_ctype ctv1 d_ctype ctv2
  | CInLoc (i, ctv, s)    -> P.dprintf "(%a, %a) ∈ %a" d_index i d_ctype ctv S.d_sloc s
  | CWFSubst (sub)        -> P.dprintf "WF(%a)" S.Subst.d_subst sub

let apply_subst_to_subst (appsub: S.Subst.t) (sub: S.Subst.t): S.Subst.t =
  List.map (fun (sfrom, sto) -> (S.Subst.apply appsub sfrom, S.Subst.apply appsub sto)) sub

let cstrdesc_subst (sub: S.Subst.t): cstrdesc -> cstrdesc = function
  | CSubtype (ctv1, ctv2) -> CSubtype (prectype_subs sub ctv1, prectype_subs sub ctv2)
  | CInLoc (ie, ctv, s)   -> CInLoc (ie, prectype_subs sub ctv, S.Subst.apply sub s)
  | CWFSubst sub2         -> CWFSubst (apply_subst_to_subst sub sub2)

type 'a precstr = {cid: int; cdesc: 'a; cloc: C.location}

type cstr = cstrdesc precstr

let (fresh_cstrid, reset_fresh_cstrids) = M.mk_int_factory ()

let mk_cstr (loc: C.location) (cdesc: 'a): 'a precstr =
  {cid = fresh_cstrid (); cloc = loc; cdesc = cdesc}

let cstr_subst (sub: S.Subst.t) (c: cstr): cstr =
  mk_cstr c.cloc (cstrdesc_subst sub c.cdesc)

let d_cstr () ({cid = cid; cdesc = cdesc; cloc = loc}: cstr): P.doc =
  P.dprintf "%a: %a" C.d_loc loc d_cstrdesc cdesc

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

let cstr_sat (st: store) (sc: cstr): bool =
  match sc.cdesc with
    | CInLoc (i, ct, s)   -> inloc_sat st i s ct
    | CSubtype (ct1, ct2) -> is_subctype ct1 ct2
    | CWFSubst (sub)      ->
        try
          let s1, s2 = sub |> S.Subst.dom |> M.find_pair S.eq in
            halt <| C.error "Locations %a and %a get unified in call, not unified in spec"
        with Not_found ->
          true

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
      | ISeq (n, m, p) ->
          let ld, sub = LDesc.shrink_period m unify_ctypes [] (prestore_find s sto) in
          let pl      = PLSeq (n, p) in
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
  sub |> S.Subst.transpose |> make_subst_well_defined

let refine_aux (sc: cstr) (sto: store): S.Subst.t * store =
  try
    match sc.cdesc with
      | CInLoc (i, ct, s)   -> refine_inloc sc.cloc s i ct sto
      | CSubtype (ct1, ct2) -> (unify_ctypes ct1 ct2 S.Subst.empty, sto)
      | CWFSubst (sub)      -> (refine_wfsubst sub, sto)
  with
    | Unify (ct1, ct2) -> halt <| C.errorLoc sc.cloc "Cannot unify %a with %a\n\n" d_ctype ct1 d_ctype ct2

type slocdep = int list SLM.t (* sloc -> cstr deps *)

type cstrmap = cstr IM.t (* cstr ident -> cstr map *)

let cstrmap_subs (sub: S.Subst.t) (cm: cstrmap): cstrmap =
  IM.map (cstr_subst sub) cm

(* pmr: should make sure that subst is well-defined at this point *)
let adjust_slocdep (sub: S.Subst.t) (sd: slocdep): slocdep =
  List.fold_left begin fun sd (sfrom, sto) ->
    (* pmr: this find pattern is common; should be using a MapWithDefault *)
    let olddeps = try SLM.find sfrom sd with Not_found -> [] in
    let newdeps = try SLM.find sto sd with Not_found -> [] in
      sd |> SLM.remove sfrom |> SLM.add sto (newdeps @ olddeps)
  end sd sub

let refine (sd: slocdep) (cm: cstrmap) (sub: S.Subst.t) (sc: cstr) (sto: store): slocdep * cstrmap * S.Subst.t * store * int list =
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
          if cstr_sat sto sc then
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
  mk_cstr !C.currentLoc (CSubtype (ctv1, ctv2))

let mk_locinc (i: index) (ctv: ctype) (s: S.t): cstr =
  mk_cstr !C.currentLoc (CInLoc (i, ctv, s))

let mk_wfsubst (sub: S.Subst.t): cstr =
  mk_cstr !C.currentLoc (CWFSubst sub)

let rec constrain_exp (env: env) (em: ctvemap) (e: C.exp): ctype * ctvemap * cstr list =
  let ct, em, cs = constrain_exp_aux env em e in
    (ct, ExpMap.add e ct em, cs)

and constrain_exp_aux (env: env) (em: ctvemap): C.exp -> ctype * ctvemap * cstr list = function
  | C.Const c                     -> let ctv = ctype_of_const c in (ctv, em, [])
  | C.Lval lv | C.StartOf lv      -> constrain_lval env em lv
  | C.UnOp (uop, e, t)            -> constrain_unop uop env em t e
  | C.BinOp (bop, e1, e2, t)      -> constrain_binop bop env em t e1 e2
  | C.CastE (C.TPtr _, C.Const c) -> let ctv, cs = constrain_constptr c in (ctv, em, cs)
  | C.CastE (ct, e)               -> constrain_cast env em ct e
  | C.SizeOf t                    -> constrain_sizeof em t
  | e                             -> E.s <| C.error "Unimplemented constrain_exp_aux: %a@!@!" C.d_exp e

and constrain_lval ((_, ve) as env: env) (em: ctvemap): C.lval -> ctype * ctvemap * cstr list = function
  | (C.Var v, C.NoOffset)       -> (VM.find v ve, em, [])
  | (C.Mem e, C.NoOffset) as lv ->
      let ctv, em, cs = constrain_exp env em e in
        begin match ctv with
          | CTRef (s, ie) ->
              let ctvlv = lv |> C.typeOfLval |> SI.fresh_heaptype in
              let cs    = mk_locinc ie ctvlv s :: cs in
                (ctvlv, em, cs)
          | _ -> E.s <| C.bug "constraining ref lval gave back non-ref type in constrain_lval@!@!"
        end
  | lv -> E.s <| C.bug "constrain_lval got lval with offset: %a@!@!" C.d_lval lv

and constrain_unop (op: C.unop) (env: env) (em: ctvemap) (t: C.typ) (e: C.exp): ctype * ctvemap * cstr list =
  let ctv, em, cs = constrain_exp env em e in
    match ctv with
      | CTInt _ -> (apply_unop t op, em, cs)
      | _       -> E.s <| C.error "Unimplemented: Haven't considered how to apply unops to references@!"

and apply_unop (rt: C.typ): C.unop -> ctype = function
  | C.LNot -> CTInt (CM.typ_width rt, index_nonneg)
  | C.BNot -> CTInt (CM.typ_width rt, index_top)
  | C.Neg  -> CTInt (CM.typ_width rt, index_top)

and constrain_binop (op: C.binop) (env: env) (em: ctvemap) (t: C.typ) (e1: C.exp) (e2: C.exp): ctype * ctvemap * cstr list =
  let ctv1, em, cs1 = constrain_exp env em e1 in
  let ctv2, em, cs2 = constrain_exp env em e2 in
  let ctv           = apply_binop op t ctv1 ctv2 in
    (ctv, em, List.concat [cs1; cs2])

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
  CTInt (CM.typ_width !C.upointType, index_top)

and apply_rel (_: C.typ) (_: ctype) (_: ctype): ctype =
  CTInt (CM.int_width, index_nonneg)

and apply_unknown (rt: C.typ) (_: ctype) (_: ctype): ctype =
  CTInt (CM.typ_width rt, index_top)

and constrain_constptr: C.constant -> ctype * cstr list = function
  | C.CStr _                                 -> let s = S.fresh S.Abstract in (CTRef (s, IInt 0), [mk_locinc index_nonneg (CTInt (1, index_nonneg)) s])
  | C.CInt64 (v, ik, so) when v = Int64.zero -> let s = S.fresh S.Abstract in (CTRef (s, IBot), [])
  | c                                        -> E.s <| C.error "Cannot cast non-zero, non-string constant %a to pointer@!@!" C.d_const c

and constrain_cast (env: env) (em: ctvemap) (ct: C.typ) (e: C.exp): ctype * ctvemap * cstr list =
  let ctv, em, cs = constrain_exp env em e in
    match C.unrollType ct, C.unrollType <| C.typeOf e with
      | C.TInt (ik, _), C.TPtr _     -> (CTInt (C.bytesSizeOfInt ik, index_nonneg), em, cs)
      | C.TInt (ik, _), C.TFloat _   -> (CTInt (C.bytesSizeOfInt ik, index_top), em, cs)
      | C.TFloat (fk, _), _          -> (CTInt (CM.bytesSizeOfFloat fk, index_top), em, cs)
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
                    index_top
                in (CTInt (C.bytesSizeOfInt ik, iec), em, cs)
            | _ -> E.s <| C.error "Got bogus type in contraining int-int cast@!@!"
          end
      | _ -> (ctv, em, cs)

and constrain_sizeof (em: ctvemap) (t: C.typ): ctype * ctvemap * cstr list =
  (CTInt (CM.int_width, IInt (CM.typ_width t)), em, [])

let constrain_return (env: env) (em: ctvemap) (rtv: ctype): C.exp option -> ctvemap * RA.block_annotation * cstr list = function
    | None   -> if is_void rtv then (em, [], []) else (C.error "Returning void value for non-void function\n\n" |> ignore; assert false)
    | Some e ->
        let ctv, em, cs = constrain_exp env em e in
          (em, [], mk_subty ctv rtv :: cs)

let constrain_arg (env: env) (e: C.exp) ((ctvs, em, css): ctype list * ctvemap * cstr list list): ctype list * ctvemap * cstr list list =
  let ctv, em, cs = constrain_exp env em e in
    (ctv :: ctvs, em, cs :: css)

let constrain_args (env: env) (em: ctvemap) (es: C.exp list): ctype list * ctvemap * cstr list list =
  List.fold_right (constrain_arg env) es ([], em, [])

let constrain_app ((fs, _) as env: env) (em: ctvemap) (f: C.varinfo) (lvo: C.lval option) (args: C.exp list): ctvemap * RA.annotation list * cstr list list =
  let ctvs, em, css  = constrain_args env em args in
  let cf, _          = VM.find f fs in
  let instslocs      = List.map (fun _ -> S.fresh S.Abstract) cf.qlocs in
  let annot          = (List.map2 (fun sfrom sto -> RA.New (sfrom, sto)) cf.qlocs) instslocs in
  let sub            = List.combine cf.qlocs instslocs in
  let ctvfs          = List.map (prectype_subs sub <.> snd) cf.args in
  let stoincs        = prestore_fold (fun ics s i ct -> mk_locinc i (prectype_subs sub ct) (S.Subst.apply sub s) :: ics) [] cf.sto_out in
  let css            = (mk_wfsubst sub :: stoincs)
                       :: ((List.map2 (fun ctva ctvf -> mk_subty ctva ctvf) ctvs) ctvfs) 
                       :: css in
    match lvo with
      | None    -> (em, annot, css)
      | Some lv ->
          let ctvlv, em, cs2 = constrain_lval env em lv in
          let em             = ExpMap.add (C.Lval lv) ctvlv em in
            (em, annot, (mk_subty (prectype_subs sub cf.ret) ctvlv :: cs2) :: css)

let constrain_instr_aux (env: env) ((em, bas, css): ctvemap * RA.block_annotation * cstr list list) (i: C.instr): ctvemap * RA.block_annotation * cstr list list =
  let _ = C.currentLoc := C.get_instrLoc i in
    match i with
      | C.Set (lv, e, _) ->
          let ctv1, em, cs1 = constrain_lval env em lv in
          let ctv2, em, cs2 = constrain_exp env em e in
            (em, [] :: bas, (mk_subty ctv2 ctv1 :: cs1) :: cs2 :: css)
      | C.Call (None, C.Lval (C.Var f, C.NoOffset), args, _) when CM.isVararg f.C.vtype ->
          if not !Constants.safe then C.warn "Unsoundly ignoring vararg call to %a@!@!" CM.d_var f |> ignore else E.s <| C.error "Can't handle varargs";
          let _, em, css2 = constrain_args env em args in
            (em, [] :: bas, css2 @ css)
      | C.Call (lvo, C.Lval (C.Var f, C.NoOffset), args, _) ->
          let em, ba, css2 = constrain_app env em f lvo args in
            (em, ba :: bas, css2 @ css)
      | i -> E.s <| C.bug "Unimplemented constrain_instr: %a@!@!" C.dn_instr i

let constrain_instr (env: env) (em: ctvemap) (is: C.instr list): ctvemap * RA.block_annotation * cstr list =
  let em, bas, css = List.fold_left (constrain_instr_aux env) (em, [], []) is in
    (em, List.rev ([] :: bas), List.concat css)

let constrain_stmt (env: env) (em: ctvemap) (rtv: ctype) (s: C.stmt): ctvemap * RA.block_annotation * cstr list =
  let _ = C.currentLoc := C.get_stmtLoc s.C.skind in
    match s.C.skind with
      | C.Instr is          -> constrain_instr env em is
      | C.If (e, _, _, _)   -> let _, em, cs = constrain_exp env em e in (em, [], cs) (* we'll visit the subblocks later *)
      | C.Break _           -> (em, [], [])
      | C.Continue _        -> (em, [], [])
      | C.Goto _            -> (em, [], [])
      | C.Block _           -> (em, [], [])                              (* we'll visit this later as we iterate through blocks *)
      | C.Loop (_, _, _, _) -> (em, [], [])                              (* ditto *)
      | C.Return (rexp, _)  -> constrain_return env em rtv rexp
      | _                   -> E.s <| C.bug "Unimplemented constrain_stmt: %a@!@!" C.dn_stmt s

let constrain_phi_defs (ve: ctvenv) ((vphi, vdefs): C.varinfo * (int * C.varinfo) list): cstr list =
  let _ = C.currentLoc := vphi.C.vdecl in
    List.map (fun (_, vdef) -> mk_subty (VM.find vdef ve) (VM.find vphi ve)) vdefs

let constrain_phis (ve: ctvenv) (phis: (C.varinfo * (int * C.varinfo) list) list array): cstr list =
  Array.to_list phis |> List.flatten |> List.map (constrain_phi_defs ve) |> List.concat

let constrain_fun (fs: funenv) (cf: cfun) (ve: ctvenv) ({ST.fdec = fd; ST.phis = phis; ST.cfg = cfg}: ST.ssaCfgInfo): ctvemap * RA.block_annotation array * cstr list =
  let _          = C.currentLoc := fd.C.svar.C.vdecl in
  let formalcs   = List.map2 (fun (_, fct) bv -> mk_subty fct (VM.find bv ve)) cf.args fd.C.sformals in
  let phics      = constrain_phis ve phis in
  let blocks     = cfg.Ssa.blocks in
  let bas        = Array.make (Array.length blocks) [] in
  let em,    css =
    M.array_fold_lefti begin fun i (em, css) b ->
      let em, ba, cs = constrain_stmt (fs, ve) em cf.ret b.Ssa.bstmt in
        Array.set bas i ba;
        (em, cs :: css)
    end (ExpMap.empty, []) blocks
  in
  let _      = C.currentLoc := fd.C.svar.C.vdecl in
  let vartys = VM.fold (fun _ ct cts -> ct :: cts) ve [] in
  let cs     = formalcs :: phics :: css |> List.concat in
  let _      =
    if Cs.ck_olev Cs.ol_solve then begin
      P.printf "Constraints for %s:\n\n" fd.C.svar.C.vname;
      List.iter (fun c -> P.printf "%a\n" d_cstr c |> ignore) cs;
      P.printf "\n" |> ignore
    end else ()
  in (em, bas, cs)

let constrain_sci (fs: funenv) (sci: ST.ssaCfgInfo): ctvemap * RA.block_annotation array * cstr list =
  let cf, ve = VM.find sci.ST.fdec.C.svar fs in
    constrain_fun fs cf ve sci

(******************************************************************************)
(*************************** Constraint Dependencies **************************)
(******************************************************************************)

let mk_cstrmap (scs: cstr list): cstrmap =
  List.fold_left (fun cm sc -> IM.add sc.cid sc cm) IM.empty scs

let add_slocdep (id: int) (sd: slocdep) (s: Sloc.t): slocdep =
  let depcs = try SLM.find s sd with Not_found -> [] in
    SLM.add s (id :: depcs) sd

let fresh_sloc_of (c: ctype): ctype =
  match c with
    | CTRef (s, i) -> CTRef (S.fresh S.Abstract, i)
    | _            -> c

(******************************************************************************)
(**************************** Local Shape Inference ***************************)
(******************************************************************************)

type shape =
  {vtyps : (Cil.varinfo * Ctypes.ctype) list;
   etypm : Ctypes.ctemap;
   store : Ctypes.store;
   anna  : Refanno.block_annotation array;
   edgem : Refanno.annotation list Misc.IntIntMap.t;
   theta : Refanno.ctab}

let update_deps (scs: cstr list) (cm: cstrmap) (sd: slocdep): cstrmap * slocdep =
  let cm = List.fold_left (fun cm sc -> IM.add sc.cid sc cm) cm scs in
  let sd = List.fold_left (fun sd sc -> cstrdesc_slocs sc.cdesc |> List.fold_left (add_slocdep sc.cid) sd) sd scs in
    (cm, sd)

let check_out_store_complete (sto_out_formal: store) (sto_out_actual: store): bool =
  prestore_fold begin fun ok l i ct ->
    if SLM.mem l sto_out_formal && prestore_find_index l i sto_out_formal = [] then begin
      C.error "Actual store has binding %a |-> %a: %a, missing from spec for %a\n\n" 
        S.d_sloc l d_index i d_ctype ct S.d_sloc l |> ignore;
      false
    end else
      ok
  end true sto_out_actual

let check_slocs_distinct (error: unit -> S.t * S.t -> P.doc) (sub: S.Subst.t) (slocs: S.t list): unit =
  try
    let s1, s2 = Misc.find_pair (fun s1 s2 -> M.map_pair (S.Subst.apply sub) (s1, s2) |> M.uncurry S.eq) slocs in
      halt <| C.error "%a\n\n" error (s1, s2)
  with Not_found -> ()

let revert_spec_names (subaway: S.Subst.t) (st: store): S.Subst.t =
     st
  |> prestore_domain
  |> List.fold_left (fun sub s -> S.Subst.extend (S.Subst.apply subaway s) s sub) []

type soln = store * ctype VM.t * ctvemap * RA.block_annotation array

let global_alias_error () ((s1, s2): S.t * S.t): P.doc =
  C.error "Global locations %a and %a get unified in function body" S.d_sloc s1 S.d_sloc s2

let quantification_error () ((s1, s2): S.t * S.t): P.doc =
  C.error "Quantified locations %a and %a get unified in function body" S.d_sloc s1 S.d_sloc s2

let global_quantification_error () ((s1, s2): S.t * S.t): P.doc =
  C.error "Global and quantified locations get unified in function body (%a, %a)" S.d_sloc s1 S.d_sloc s2

let rec solve_and_check (cf: cfun) (vars: ctype VM.t) (gst: store) (em: ctvemap) (bas: RA.block_annotation array) (sd: slocdep) (cm: cstrmap): soln =
  let sd, cm, sub, sto = solve sd cm SLM.empty in
  let whole_store      = prestore_upd cf.sto_out gst in
  let _                = check_slocs_distinct global_alias_error sub (prestore_domain gst) in
  let _                = check_slocs_distinct quantification_error sub cf.qlocs in
  let _                = check_slocs_distinct global_quantification_error sub (prestore_domain whole_store) in
  let revsub           = revert_spec_names sub whole_store in
  let sto              = prestore_subs revsub sto in
  let sto              = cf |> cfun_slocs |> List.fold_left (fun sto s -> if SLM.mem s sto then sto else SLM.add s LDesc.empty sto) sto in
  let sd               = adjust_slocdep revsub sd in
  let cm               = cstrmap_subs revsub cm in
  let sub              = S.Subst.compose revsub sub in
  let vars             = VM.map (prectype_subs sub) vars in
  let em               = ExpMap.map (prectype_subs sub) em in
  let bas              = Array.map (RA.subs sub) bas in
    if check_out_store_complete whole_store sto then
      (SLM.fold (fun s _ sto -> SLM.remove s sto) gst sto, vars, em, bas)
    else
      halt <| C.error "Failed checking store typing:\nStore:\n%a\n\ndoesn't match expected type:\n\n%a\n\n" d_store sto d_cfun cf

let d_vartypes () vars =
  P.docList ~sep:(P.dprintf "@!") (fun (v, ct) -> P.dprintf "%s: %a" v.C.vname Ctypes.d_ctype ct) () vars

let print_shape (fname: string) (cf: cfun) ({vtyps = locals; store = st; anna = annot}: shape) (ds: Ind.dcheck list): unit =
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
  let _ = P.printf "Annotations:@!" in
  let _ = P.printf "------@!@!" in
  let _ = P.printf "%a@!@!" RA.d_block_annotation_array annot in
  let _ = P.printf "Deferred Checks:@!" in
  let _ = P.printf "------@!@!" in
  let _ = P.printf "%a@!@!" (P.d_list "\n" Ind.d_dcheck) ds in
    ()

let fresh_local_slocs (ve: ctvenv) =
  VM.mapi (fun v ct -> if v.C.vglob then ct else fresh_sloc_of ct) ve

let infer_shape (fe: funenv) (ve: ctvenv) (gst: store) (scim: Ssa_transform.ssaCfgInfo CilMisc.VarMap.t) (cf: cfun) (sci: ST.ssaCfgInfo): shape * Ind.dcheck list =
  let ve, ds              = sci |> Ind.infer_fun_indices (ctenv_of_funenv fe) ve scim cf |> M.app_fst fresh_local_slocs in
  let em, bas, cs         = constrain_fun fe cf ve sci in
  let whole_store         = prestore_upd cf.sto_out gst in
  let scs                 = prestore_fold (fun cs l i ct -> mk_locinc i ct l :: cs) cs whole_store in
  let cm, sd              = update_deps scs IM.empty SLM.empty in
  let _                   = C.currentLoc := sci.ST.fdec.C.svar.C.vdecl in
  let sto, vtyps, em, bas = solve_and_check cf ve gst em bas sd cm in
  let vtyps               = VM.fold (fun vi vt vtyps -> if vi.C.vglob then vtyps else VM.add vi vt vtyps) vtyps VM.empty in
  let sto                 = prestore_fold (fun sto _ _ -> function CTRef (s, _) -> if SLM.mem s sto then sto else SLM.add s LDesc.empty sto | _ -> sto) sto sto in
  let annot, edgem, theta = RA.annotate_cfg sci.ST.cfg (prestore_domain gst) em bas in
  let shp                 = {vtyps = CM.vm_to_list vtyps;
                             etypm = em;
                             store = sto;
                             anna  = annot;
                             edgem = edgem;
                             theta = theta} in
    if !Cs.verbose_level >= Cs.ol_ctypes || !Cs.ctypes_only then print_shape sci.ST.fdec.C.svar.C.vname cf shp ds;
    (shp, ds)

type funmap = (cfun * Ssa_transform.ssaCfgInfo) SM.t

let declared_funs (cil: C.file) =
  C.foldGlobals cil begin fun fs -> function
    | C.GFun (fd, _)                                      -> fd.C.svar :: fs
    | C.GVarDecl (vi, _) when C.isFunctionType vi.C.vtype -> vi :: fs
    | _                                                   -> fs
  end []

(* API *)
let infer_shapes (cil: C.file) ((funspec, varspec, storespec): cspec) (scis: funmap): (shape * Ind.dcheck list) SM.t =
  let ve = C.foldGlobals cil begin fun ve -> function
             | C.GVarDecl (vi, loc) | C.GVar (vi, _, loc) when not (C.isFunctionType vi.C.vtype) ->
                 begin try
                   VM.add vi (SM.find vi.C.vname varspec |> fst) ve
                 with Not_found ->
                   halt <| C.errorLoc loc "Could not find spec for global var %a\n" CM.d_var vi
                 end
             | _ -> ve
           end VM.empty
  in
  let fe = declared_funs cil
        |> List.map (fun f -> (f, SM.find f.C.vname funspec |> fst))
        |> List.fold_left (fun fe (f, cf) -> VM.add f (funenv_entry_of_cfun cf) fe) VM.empty in
  let scim = SM.fold (fun _ (_, sci) scim -> VM.add sci.ST.fdec.C.svar sci scim) scis VM.empty in
    scis |> SM.map (infer_shape fe ve storespec scim |> M.uncurry)
