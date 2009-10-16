module M   = Misc
module P   = Pretty
module E   = Errormsg
module C   = Cil
module CM  = CilMisc
module VM  = CM.VarMap
module S   = Sloc
module SLM = S.SlocMap
module ST  = Ssa_transform
module Cs  = Constants

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
  | IEUnsign of indexexp

let rec d_indexexp (): indexexp -> P.doc = function
  | IEConst i                 -> d_index () i
  | IEVar iv                  -> d_indexvar () iv
  | IEPlus (ie1, scale, ie2)  -> P.dprintf "%a + %d * %a" d_indexexp ie1 scale d_indexexp ie2
  | IEMinus (ie1, scale, ie2) -> P.dprintf "%a - %d * %a" d_indexexp ie1 scale d_indexexp ie2
  | IEMult (ie1, ie2)         -> P.dprintf "%a * %a" d_indexexp ie1 d_indexexp ie2
  | IEDiv (ie1, ie2)          -> P.dprintf "%a / %a" d_indexexp ie1 d_indexexp ie2
  | IEUnsign ie               -> P.dprintf "(unsigned) %a" d_indexexp ie

let indexexp_vars_aux: indexexp -> indexexp list * indexvar list = function
  | IEPlus (ie1, _, ie2)
  | IEMinus (ie1, _, ie2)
  | IEMult (ie1, ie2)
  | IEDiv (ie1, ie2) -> ([ie1; ie2], [])
  | IEConst _        -> ([], [])
  | IEVar iv         -> ([], [iv])
  | IEUnsign ie      -> ([ie], [])

let indexexp_vars (ie: indexexp): indexvar list =
  M.expand indexexp_vars_aux [ie] []

module IndexSol =
  Misc.MapWithDefault(struct
                        type t      = indexvar
                        let compare = compare

                        type v      = index
                        let default = IBot
                      end)

type indexsol = index IndexSol.t

module ISPrinter = P.MakeMapPrinter(IndexSol)

let d_indexsol =
  ISPrinter.d_map "\n" d_indexvar d_index

let rec indexexp_apply (is: indexsol): indexexp -> index = function
  | IEConst i             -> i
  | IEVar iv              -> IndexSol.find iv is
  | IEUnsign ie           -> index_unsign (indexexp_apply is ie)
  | IEPlus (ie1, x, ie2)  -> index_plus (indexexp_apply is ie1) (index_scale x <| indexexp_apply is ie2)
  | IEMinus (ie1, x, ie2) -> index_minus (indexexp_apply is ie1) (index_scale x <| indexexp_apply is ie2)
  | IEMult (ie1, ie2)     -> index_mult (indexexp_apply is ie1) (indexexp_apply is ie2)
  | IEDiv (ie1, ie2)      -> index_div (indexexp_apply is ie1) (indexexp_apply is ie2)

let refine_index (is: indexsol) (ie: indexexp) (iv: indexvar): indexsol =
  IndexSol.add iv (index_lub (indexexp_apply is ie) (IndexSol.find iv is)) is

let bounded_refine_index (is: indexsol) (ie: indexexp) (iv: indexvar) (ibound: index): indexsol =
  let is = refine_index is ie iv in
    if is_subindex (IndexSol.find iv is) ibound then is else IndexSol.add iv ibound is

(******************************************************************************)
(****************************** Type Constraints ******************************)
(******************************************************************************)

type itypevar = indexexp prectype

let d_itypevar: unit -> itypevar -> P.doc =
  d_prectype d_indexexp

let itypevar_indexvars: itypevar -> indexvar list = function
  | CTInt (_, ie) | CTRef (_, ie) -> indexexp_vars ie

let itypevar_of_ctype: ctype -> itypevar = function
  | CTInt (n, i) -> CTInt (n, IEConst i)
  | CTRef (s, i) -> CTRef (s, IEConst i)

let heap_itypevar (t: C.typ): itypevar =
  match C.unrollType t with
    | C.TInt (ik, _)        -> CTInt (C.bytesSizeOfInt ik, IEConst ITop)
    | C.TFloat _            -> CTInt (CM.typ_width t, IEConst ITop)
    | C.TVoid _             -> CTInt (0, IEConst ITop)
    | C.TPtr _ | C.TArray _ -> CTRef (S.none, IEConst (IInt 0))
    | t                     -> E.s <| E.bug "Unimplemented heap_itypevar: %a@!@!" C.d_type t

let fresh_itypevar (t: C.typ): itypevar =
  match C.unrollType t with
    | C.TInt (ik, _)        -> CTInt (C.bytesSizeOfInt ik, IEVar (fresh_indexvar ()))
    | C.TFloat _            -> CTInt (CM.typ_width t, IEConst ITop)
    | C.TVoid _             -> CTInt (0, IEVar (fresh_indexvar ()))
    | C.TPtr _ | C.TArray _ -> CTRef (S.none, IEVar (fresh_indexvar ()))
    | t                     -> E.s <| E.bug "Unimplemented fresh_itypevar: %a@!@!" C.d_type t

let itypevar_apply (is: indexsol) (itv: itypevar): ctype =
  prectype_map (indexexp_apply is) itv

let is_subitypevar (is: indexsol) (itv1: itypevar) (itv2: itypevar): bool =
  match itv1, itv2 with
    | CTInt (n1, ie1), CTInt (n2, ie2) when n1 = n2 -> is_subindex (indexexp_apply is ie1) (indexexp_apply is ie2)
    | CTRef (_, ie1), CTRef (_, ie2)                -> is_subindex (indexexp_apply is ie1) (indexexp_apply is ie2)
    | _                                             -> false

type ifunvar = indexexp precfun

let d_ifunvar: unit -> ifunvar -> P.doc =
  d_precfun d_indexexp

type itypecstrdesc =
  | ISubtype of itypevar * itypevar
  | IDSubtype of itypevar * itypevar * ctype

let d_itypecstrdesc (): itypecstrdesc -> P.doc = function
  | ISubtype (itv1, itv2)      -> P.dprintf "%a <: %a" d_itypevar itv1 d_itypevar itv2
  | IDSubtype (itv1, itv2, ct) -> P.dprintf "%a <: %a <: %a" d_itypevar itv1 d_itypevar itv2 d_ctype ct

type itypecstr = {itcid: int; itcdesc: itypecstrdesc; itcloc: C.location}

let d_itypecstr () ({itcid = id; itcdesc = itcd; itcloc = loc}: itypecstr): P.doc =
  P.dprintf "%a: %a" C.d_loc loc d_itypecstrdesc itcd

let (fresh_itypecstrid, reset_fresh_itypecstrids) = M.mk_int_factory ()

let mk_isubtypecstr (loc: C.location) (itv1: itypevar) (itv2: itypevar): itypecstr =
  {itcid = fresh_itypecstrid (); itcloc = loc; itcdesc = ISubtype (itv1, itv2)}

let mk_idsubtypecstr (loc: C.location) (itv1: itypevar) (itv2: itypevar) (ctbound: ctype): itypecstr =
  {itcid = fresh_itypecstrid (); itcloc = loc; itcdesc = IDSubtype (itv1, itv2, ctbound)}

let itypecstr_id ({itcid = id}: itypecstr): int =
  id

let itypecstr_rhs_var (itc: itypecstr): indexvar option =
  match itc.itcdesc with
    | ISubtype (_, itv) | IDSubtype (_, itv, _) ->
        match itypevar_indexvars itv with
          | []   -> None
          | [iv] -> Some iv
          | _    -> halt <| C.errorLoc itc.itcloc "Ill-formed constraint: %a\n" d_itypecstr itc

let itypecstr_sat (is: indexsol) (itc: itypecstr): bool =
  match itc.itcdesc with
    | ISubtype (itv1, itv2)           -> is_subitypevar is itv1 itv2
    | IDSubtype (itv1, itv2, ctbound) ->
        let itvbound = itypevar_of_ctype ctbound in
           (not (is_subitypevar is itv1 itvbound) && is_subitypevar is itv2 itvbound && is_subitypevar is itvbound itv2)
        || is_subitypevar is itv1 itv2

let refine_itypecstr (is: indexsol) (itc: itypecstr): indexsol =
  match itc.itcdesc with
    | ISubtype (itv1, itv2) ->
        begin match itv1, itv2 with
          | CTInt (n1, ie), CTInt (n2, IEVar iv) when n1 = n2 -> refine_index is ie iv
          | CTRef (_, ie), CTRef (_, IEVar iv)                -> refine_index is ie iv
          | _                                                 -> halt <| C.errorLoc itc.itcloc "Failed index constraint %a\n\n" d_itypecstr itc
        end
    | IDSubtype (itv1, itv2, ctbound) ->
        begin match itv1, itv2, ctbound with
          | CTInt (n1, ie), CTInt (n2, IEVar iv), CTInt (n3, ib) when n1 = n2 && n2 = n3 -> bounded_refine_index is ie iv ib
          | CTRef (_, ie), CTRef (_, IEVar iv), CTRef (_, ib)                            -> bounded_refine_index is ie iv ib
          | _                                                                            -> halt <| C.errorLoc itc.itcloc "Failed index constraint %a\n\n" d_itypecstr itc
        end

type cstrmap = itypecstr M.IntMap.t

module CSMPrinter = P.MakeMapPrinter(M.IntMap)

let d_cstrmap =
  CSMPrinter.d_map "\n" (fun () -> P.num) d_itypecstr

let mk_cstrmap (itcs: itypecstr list): cstrmap =
  List.fold_left (fun cm itc -> M.IntMap.add itc.itcid itc cm) M.IntMap.empty itcs

module IM = M.MapWithDefault(struct
                               type t = int
                               let compare = compare

                               type v = int list
                               let default = []
                             end)

type cstrdepmap = int list IM.t (* indexvar -> indexcstr deps *)

module CDMPrinter = P.MakeMapPrinter(IM)

let d_cstrdepmap =
  CDMPrinter.d_map "\n" d_indexvar (P.d_list ", " (fun () -> P.num))

let add_cstrdep (cdm: cstrdepmap) (itc: itypecstr): cstrdepmap =
  match itc.itcdesc with
    | ISubtype (itv, _) | IDSubtype (itv, _, _) ->
        itv |> itypevar_indexvars |> List.fold_left (fun cdm iv -> IM.add iv (itc.itcid :: IM.find iv cdm) cdm) cdm

let mk_cstrdepmap (itcs: itypecstr list): cstrdepmap =
  List.fold_left add_cstrdep IM.empty itcs

(* Probably want to introduce some kind of rank on constraints, but if
   we traverse the program in the right order, id is a reasonable
   approximation. *)
module WkList = Heaps.Functional(struct
                                   type t = int
                                   let compare = compare
                                 end)

let wklist_push (wkl: WkList.t) (itcids: int list): WkList.t =
  List.fold_left (M.flip WkList.add) wkl itcids

let rec iter_solve (cm: cstrmap) (cdm: cstrdepmap) (wkl: WkList.t) (is: indexsol): indexsol =
  match try Some (WkList.maximum wkl) with Heaps.EmptyHeap -> None with
    | None       -> is
    | Some itcid ->
        let itc       = M.IntMap.find itcid cm in
        let wkl       = WkList.remove wkl in
        let succs     = itypecstr_rhs_var itc |> function Some iv -> IM.find iv cdm | None -> [] in
        let (wkl, is) = if itypecstr_sat is itc then (wkl, is) else (wklist_push wkl succs, refine_itypecstr is itc) in
          iter_solve cm cdm wkl is

let solve (itcs: itypecstr list): indexsol =
  let cm  = mk_cstrmap itcs in
  let _   = if Cs.ck_olev Cs.ol_solve then P.printf "Constraints:\n\n%a\n\n" d_cstrmap cm |> ignore in
  let cdm = mk_cstrdepmap itcs in
  let _   = if Cs.ck_olev Cs.ol_solve then P.printf "Constraint dependencies:\n\n%a\n\n" d_cstrdepmap cdm |> ignore in
  let wkl = itcs |> List.map itypecstr_id |> wklist_push WkList.empty in
    iter_solve cm cdm wkl IndexSol.empty

(******************************************************************************)
(**************************** Constraint Generation ***************************)
(******************************************************************************)

type varenv = itypevar VM.t

type funenv = ifunvar VM.t

type builtinenv = ifunvar VM.t

type env = varenv * funenv

let rec constrain_exp (env: env) (loc: C.location): C.exp -> itypevar * itypecstr list = function
  | C.Const c                     -> let itv = c |> ctype_of_const |> itypevar_of_ctype in (itv, [])
  | C.Lval lv | C.StartOf lv      -> constrain_lval env loc lv
  | C.UnOp (uop, e, t)            -> constrain_unop uop env loc t e
  | C.BinOp (bop, e1, e2, t)      -> constrain_binop bop env loc t e1 e2
  | C.CastE (C.TPtr _, C.Const c) -> constrain_constptr loc c
  | C.CastE (ct, e)               -> constrain_cast env loc ct e
  | C.SizeOf t                    -> constrain_sizeof loc t
  | e                             -> E.s <| E.error "Unimplemented constrain_exp_aux: %a@!@!" C.d_exp e

and constrain_lval ((ve, _) as env: env) (loc: C.location): C.lval -> itypevar * itypecstr list = function
  | (C.Var v, C.NoOffset)       -> (VM.find v ve, [])
  | (C.Mem e, C.NoOffset) as lv ->
      let itv, cs = constrain_exp env loc e in
        begin match itv with
          | CTRef (s, ie) -> (heap_itypevar <| C.typeOfLval lv, cs)
          | _             -> E.s <| E.bug "fresh_ctvref gave back non-ref type in constrain_lval@!@!"
        end
  | lv -> E.s <| E.bug "constrain_lval got lval with offset: %a@!@!" C.d_lval lv

and constrain_unop (op: C.unop) (env: env) (loc: C.location) (t: C.typ) (e: C.exp): itypevar * itypecstr list =
  let itv, cs = constrain_exp env loc e in
    match itv with
      | CTInt _ -> (apply_unop t op, cs)
(*      | _       -> E.s <| E.unimp "Haven't considered how to apply unops to references@!" *)
      | _       -> E.s <| C.errorLoc loc "Unimplemented: Haven't considered how to apply unops to references@!"

and apply_unop (rt: C.typ): C.unop -> itypevar = function
  | C.LNot -> CTInt (CM.typ_width rt, IEConst (ISeq (0, 1)))
  | C.BNot -> CTInt (CM.typ_width rt, IEConst ITop)
  | C.Neg  -> CTInt (CM.typ_width rt, IEConst ITop)

and constrain_binop (op: C.binop) (env: env) (loc: C.location) (t: C.typ) (e1: C.exp) (e2: C.exp): itypevar * itypecstr list =
  let itv1, cs1 = constrain_exp env loc e1 in
  let itv2, cs2 = constrain_exp env loc e2 in
  let itv, co   = apply_binop op loc t itv1 itv2 in
    (itv, M.maybe_cons co (cs1 @ cs2))

and apply_binop: C.binop -> C.location -> C.typ -> itypevar -> itypevar -> itypevar * itypecstr option = function
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

and apply_arithmetic (f: indexexp -> indexexp -> indexexp) (_: C.location) (rt: C.typ) (itv1: itypevar) (itv2: itypevar): itypevar * itypecstr option =
  match itv1, itv2 with
    | (CTInt (n1, ie1), CTInt (n2, ie2)) -> (CTInt (CM.typ_width rt, f ie1 ie2), None)
    | _                                  -> E.s <| E.bug "Type mismatch in apply_arithmetic@!@!"

and apply_ptrarithmetic (f: indexexp -> int -> indexexp -> indexexp) (loc: C.location) (pt: C.typ) (itv1: itypevar) (itv2: itypevar): itypevar * itypecstr option =
  match C.unrollType pt, itv1, itv2 with
    | C.TPtr (t, _), CTRef (s, ie1), CTInt (n, ie2) when n = CM.int_width ->
        let iv = IEVar (fresh_indexvar ()) in
          (CTRef (s, f ie1 (CM.typ_width t) iv), Some (mk_idsubtypecstr loc (CTInt (n, ie2)) (CTInt (n, iv)) (CTInt (n, ISeq (0, 1)))))
    | _ -> E.s <| E.bug "Type mismatch in constrain_ptrarithmetic@!@!"

and apply_ptrminus (_: C.location) (pt: C.typ) (_: itypevar) (_: itypevar): itypevar * itypecstr option =
  (CTInt (CM.typ_width !C.upointType, IEConst ITop), None)

and apply_rel (_: C.location) (_: C.typ) (_: itypevar) (_: itypevar): itypevar * itypecstr option =
  (CTInt (CM.int_width, IEConst (ISeq (0, 1))), None)

and apply_unknown (_: C.location) (rt: C.typ) (_: itypevar) (_: itypevar): itypevar * itypecstr option =
  (CTInt (CM.typ_width rt, IEConst ITop), None)

and constrain_constptr (loc: C.location): C.constant -> itypevar * itypecstr list = function
  | C.CStr _                                 -> halt <| E.unimp "Haven't implemented string constants yet"
  | C.CInt64 (v, ik, so) when v = Int64.zero -> (CTRef (S.none, IEConst IBot), [])
  | c                                        -> halt <| C.errorLoc loc "Cannot cast non-zero, non-string constant %a to pointer@!@!" C.d_const c

and constrain_cast (env: env) (loc: C.location) (ct: C.typ) (e: C.exp): itypevar * itypecstr list =
  let ect = constrain_exp env loc e in
    match C.unrollType ct, C.unrollType <| C.typeOf e with
      | C.TFloat (fk, _), _        -> (CTInt (CM.bytesSizeOfFloat fk, IEConst ITop), [])
      | C.TInt (ik, _), C.TFloat _ -> (CTInt (C.bytesSizeOfInt ik, IEConst ITop), [])
      | C.TInt (ik, _), C.TPtr _   -> (CTInt (C.bytesSizeOfInt ik, IEConst ITop), [])
      | C.TInt (ik, _), C.TInt _   ->
          begin match ect with
            | CTInt (n, ie), cs ->
                let iec =
                  if n <= C.bytesSizeOfInt ik then
                    (* pmr: what about the sign bit?  this may not always be safe *)
                    if C.isSigned ik then ie else IEUnsign ie
                  else if not !Constants.safe then begin
                    C.warnLoc loc "Unsoundly assuming cast is lossless@!@!" |> ignore;
                    if C.isSigned ik then ie else IEUnsign ie
                  end else
                    IEConst ITop
                in (CTInt (C.bytesSizeOfInt ik, iec), cs)
            | _ -> halt <| C.errorLoc loc "Got bogus type in contraining int-int cast@!@!"
          end
      | _ -> ect

and constrain_sizeof (loc: C.location) (t: C.typ): itypevar * itypecstr list =
  (CTInt (CM.int_width, IEConst (IInt (CM.typ_width t))), [])

let constrain_return (env: env) (rtv: itypevar) (loc: C.location): C.exp option -> itypecstr list = function
    | None   -> if is_void rtv then [] else halt <| C.errorLoc loc "Returning void value for non-void function\n\n"
    | Some e ->
        let itv, cs = constrain_exp env loc e in
          mk_isubtypecstr loc itv rtv :: cs

let constrain_arg (env: env) (loc: C.location) (e: C.exp) ((itvs, css): itypevar list * itypecstr list list): itypevar list * itypecstr list list =
  let itv, cs = constrain_exp env loc e in
    (itv :: itvs, cs :: css)

let constrain_args (env: env) (loc: C.location) (args: C.exp list): itypevar list * itypecstr list list =
  List.fold_right (constrain_arg env loc) args ([], [])

let constrain_app ((_, fe) as env: env) (loc: C.location) (f: C.varinfo) (lvo: C.lval option) (args: C.exp list): itypecstr list list =
  let itvs, css = constrain_args env loc args in
  let ftv       = try VM.find f fe with Not_found -> halt <| C.errorLoc loc "Couldn't find function %a (missing prototype or spec?)\n\n" CM.d_var f in
  let itvfs     = List.map snd ftv.args in
  let css       = List.map2 (fun itva itvf -> mk_isubtypecstr loc itva itvf) itvs itvfs :: css in
    match lvo with
      | None    -> css
      | Some lv ->
          let itvlv, cs2 = constrain_lval env loc lv in
            (mk_isubtypecstr loc ftv.ret itvlv :: cs2) :: css

let constrain_instr_aux (env: env) (css: itypecstr list list): C.instr -> itypecstr list list = function
  | C.Set (lv, e, loc) ->
      let itv1, cs1 = constrain_lval env loc lv in
      let itv2, cs2 = constrain_exp env loc e in
        (mk_isubtypecstr loc itv2 itv1 :: cs1) :: cs2 :: css
  | C.Call (None, C.Lval (C.Var f, C.NoOffset), args, loc) when CM.isVararg f.C.vtype ->
      if not !Constants.safe then C.warnLoc loc "Unsoundly ignoring vararg call to %a@!@!" CM.d_var f |> ignore else E.s <| C.errorLoc loc "Can't handle varargs";
      (constrain_args env loc args |> snd |> List.concat) :: css
  | C.Call (lvo, C.Lval (C.Var f, C.NoOffset), args, loc) ->
      (constrain_app env loc f lvo args |> List.concat) :: css
  | i -> E.s <| E.bug "Unimplemented constrain_instr: %a@!@!" C.dn_instr i

let constrain_instr (env: env) (is: C.instr list): itypecstr list =
  is |> List.fold_left (constrain_instr_aux env) [] |> List.concat

let constrain_stmt (env: env) (rtv: itypevar) (s: C.stmt): itypecstr list =
  match s.C.skind with
    | C.Instr is             -> constrain_instr env is
    | C.If (e, _, _, loc)    -> constrain_exp env loc e |> snd  (* we'll visit the subblocks later *)
    | C.Break _              -> []
    | C.Continue _           -> []
    | C.Goto _               -> []
    | C.Block _              -> []                              (* we'll visit this later as we iterate through blocks *)
    | C.Loop (_, _, _, _)    -> []                              (* ditto *)
    | C.Return (rexp, loc)   -> constrain_return env rtv loc rexp
    | _                      -> E.s <| E.bug "Unimplemented constrain_stmt: %a@!@!" C.dn_stmt s

let maybe_fresh (v: C.varinfo): (C.varinfo * itypevar) option =
  let t = C.unrollType v.C.vtype in
    match t with
      | C.TInt _
      | C.TFloat _
      | C.TPtr _
      | C.TArray _ -> Some (v, fresh_itypevar t)
      | _          ->
          let _ = if !Constants.safe then E.error "not freshing local %s" v.C.vname in
            C.warnLoc v.C.vdecl "Not freshing local %s of tricky type %a@!@!" v.C.vname C.d_type t |> ignore; None

let fresh_vars (vs: C.varinfo list): (C.varinfo * itypevar) list =
  Misc.map_partial maybe_fresh vs

let constrain_phi_defs (ve: varenv) ((vphi, vdefs): C.varinfo * (int * C.varinfo) list): itypecstr list =
  List.map (fun (_, vdef) -> mk_isubtypecstr vphi.C.vdecl (VM.find vdef ve) (VM.find vphi ve)) vdefs

let constrain_phis (ve: varenv) (phis: (C.varinfo * (int * C.varinfo) list) list array): itypecstr list =
  Array.to_list phis |> List.flatten |> List.map (constrain_phi_defs ve) |> List.concat

let dump_constraints (fn: string) (ftv: ifunvar) (cs: itypecstr list): unit =
  let _ = P.printf "Constraints for %s:\n\n" fn in
  let _ = P.printf "%a\n" d_ifunvar ftv in
  let _ = P.printf "%a\n\n" (P.d_list "\n" d_itypecstr) cs in
    ()

let constrain_fun (fe: funenv) ({ST.fdec = fd; ST.phis = phis; ST.cfg = cfg}: ST.ssaCfgInfo): varenv * itypecstr list =
  let blocks      = cfg.Ssa.blocks in
  let bodyformals = fresh_vars fd.C.sformals in
  let locals      = fresh_vars fd.C.slocals in
  let vars        = locals @ bodyformals in
  let ve          = List.fold_left (fun ve (v, itv) -> VM.add v itv ve) VM.empty vars in
  let loc         = fd.C.svar.C.vdecl in
  let ftv         = VM.find fd.C.svar fe in
  let formalcs    = List.map2 (fun (_, at) (_, itv) -> mk_isubtypecstr loc at itv) ftv.args bodyformals in
  let phics       = constrain_phis ve phis in
  let env         = (ve, fe) in
  let css         = Array.fold_left (fun css b -> constrain_stmt env ftv.ret b.Ssa.bstmt :: css) [] blocks in
  let cs          = formalcs :: phics :: css |> List.concat in
  let _           = if Cs.ck_olev Cs.ol_solve then dump_constraints fd.C.svar.C.vname ftv cs in
    (ve, cs)

let fresh_fun_typ (fd: C.fundec): ifunvar =
  let rty, ftyso, _, _ = C.splitFunctionType fd.C.svar.C.vtype in
  let fctys            = match ftyso with None -> [] | Some ftys -> List.map (fun (fn, fty, _) -> (fn, fresh_itypevar fty)) ftys in
    mk_cfun [] fctys (fresh_itypevar rty) SLM.empty SLM.empty

let constrain_prog_fold (fe: funenv) (_: VM.key) (sci: ST.ssaCfgInfo) ((css, fm): itypecstr list list * (ifunvar * itypevar VM.t) VM.t): itypecstr list list * (ifunvar * itypevar VM.t) VM.t =
  let ve, cs = constrain_fun fe sci in
  let fv     = sci.ST.fdec.C.svar in
    (cs :: css, VM.add fv (VM.find fv fe, ve) fm)

let constrain_prog (ctenv: cfun VM.t) (scim: ST.ssaCfgInfo VM.t): itypecstr list * (ifunvar * itypevar VM.t) VM.t =
  let fe      = VM.map (precfun_map (prectype_map (fun i -> IEConst i))) ctenv in
  let fm      = VM.map (fun cf -> (cf, VM.empty)) fe in
  let fe      = VM.fold (fun f {ST.fdec = fd} fe -> VM.add f (fresh_fun_typ fd) fe) scim fe in
  let css, fm = VM.fold (constrain_prog_fold fe) scim ([], fm) in
    (List.concat css, fm)

type indextyping = (cfun * ctype VM.t) VM.t

let d_indextyping () (it: indextyping): P.doc =
     it
  |> M.flip (VM.fold (fun f t it -> if CM.definedHere f then VM.add f t it else it)) VM.empty
  |> CM.VarMapPrinter.d_map
      ~dmaplet:(fun d1 d2 -> P.dprintf "%t\n%t" (fun () -> d1) (fun () -> d2))
      "\n\n"
      CM.d_var
      (fun () (cf, vm) -> P.dprintf "%a\n\nLocals:\n%a\n\n" d_cfun cf (CM.VarMapPrinter.d_map "\n" CM.d_var d_ctype) vm) ()

(* API *)
let infer_indices (ctenv: cfun VM.t) (scim: ST.ssaCfgInfo VM.t): indextyping =
  let cs, fm = constrain_prog ctenv scim in
  let is     = solve cs in
  let _      = if Cs.ck_olev Cs.ol_solve then P.printf "Index solution:\n\n%a\n\n" d_indexsol is |> ignore in
  let it     = VM.map (fun (ifv, vm) -> (precfun_map (itypevar_apply is) ifv, VM.map (prectype_map (indexexp_apply is)) vm)) fm in
  let _      = if Cs.ck_olev Cs.ol_solve then P.printf "Index typing:\n\n%a\n\n" d_indextyping it |> ignore in
    it
