module M = Misc
module P = Pretty
module C = Cil

open Ctypes
open M.Ops

(******************************************************************************)
(****************************** Index Constraints *****************************)
(******************************************************************************)

type indexvar = int

let (fresh_indexvar, reset_fresh_indexvars) = M.mk_int_factory ()

type indexexp =
  | IEInt of int
  | IEVar of indexvar
  | IEPlus of indexvar * (* RHS scale: *) int * indexvar

type indexcstr =
  | ICLess of indexexp * indexvar

module IVM =
  Map.Make
    (struct
       type t      = indexvar
       let compare = compare
     end)

type indexsol = index IVM.t

let indexsol_find (iv: indexvar) (is: indexsol): index =
  try IVM.find iv is with Not_found -> IBot

let indexexp_apply (is: indexsol): indexexp -> index = function
  | IEInt n              -> IInt n
  | IEVar iv             -> indexsol_find iv is
  | IEPlus (iv1, x, iv2) -> index_plus (indexsol_find iv1 is) (index_scale x <| indexsol_find iv2 is)

let refine_index (ie: indexexp) (iv: indexvar) (is: indexsol): indexsol =
  IVM.add iv (index_lub (indexexp_apply is ie) (indexsol_find iv is)) is

let indexcstr_sat (ICLess (ie, iv): indexcstr) (is: indexsol): bool =
  is_subindex (indexexp_apply is ie) (indexsol_find iv is)

(******************************************************************************)
(****************************** Type Constraints ******************************)
(******************************************************************************)

type ctypevar = indexvar prectype

let (fresh_sloc, reset_fresh_slocs) = M.mk_int_factory ()

let fresh_ctvint (n: int): ctypevar =
  CTInt (n, fresh_indexvar ())

let fresh_ctvref (): ctypevar =
  CTRef (fresh_sloc (), fresh_indexvar ())

let ctypevar_apply (is: indexsol): ctypevar -> ctype = function
  | CTInt (n, iv) -> CTInt (n, indexsol_find iv is)
  | CTRef (s, iv) -> CTRef (s, indexsol_find iv is)

type ctypecstr =
  | CTCSubtype of ctypevar * ctypevar

let ctypecstr_replace_sloc (s1: sloc) (s2: sloc) (CTCSubtype (ctv1, ctv2): ctypecstr): ctypecstr =
  CTCSubtype (prectype_replace_sloc s1 s2 ctv1, prectype_replace_sloc s1 s2 ctv2)

let refine_ctype (ctv1: ctypevar) (ctv2: ctypevar) (is: indexsol): indexsol =
  match (ctv1, ctv2) with
    | (CTInt (n1, iv1), CTInt (n2, iv2)) when n1 = n2 -> refine_index (IEVar iv1) iv2 is
    | (CTRef (s1, iv1), CTRef (s2, iv2)) when s1 = s2 -> refine_index (IEVar iv1) iv2 is
    | _                                               -> raise (NoLUB (ctypevar_apply is ctv1, ctypevar_apply is ctv2))

let equalize_ctypes (ctv1: ctypevar) (ctv2: ctypevar) (is: indexsol): indexsol =
  refine_ctype ctv2 ctv1 (refine_ctype ctv1 ctv2 is)

let ctypecstr_sat (CTCSubtype (ctv1, ctv2): ctypecstr) (is: indexsol): bool =
  is_subctype (ctypevar_apply is ctv1) (ctypevar_apply is ctv2)

(******************************************************************************)
(****************************** Store Constraints *****************************)
(******************************************************************************)

type storecstr =
  | SCInc of sloc * indexvar * ctypevar (* (l, i, ct): (i, ct) in store(l) *)

let storecstr_replace_sloc (s1: sloc) (s2: sloc) (SCInc (s3, iv, ctv): storecstr): storecstr =
  SCInc ((if s3 = s1 then s2 else s3), iv, prectype_replace_sloc s1 s2 ctv)

type storesol = indexvar prestore

let storesol_add (l: sloc) (pl: ploc) (ctv: ctypevar) (ss: storesol): storesol =
  SLM.add l (LDesc.add pl ctv (prestore_find l ss)) ss

let refine_store (l: sloc) (iv: indexvar) (ctv: ctypevar) (is: indexsol) (ss: storesol): indexsol * storesol =
  match indexsol_find iv is with
    | IBot   -> (is, ss)
    | IInt n ->
        let pl = PLAt n in
          begin match LDesc.find pl (prestore_find l ss) with
            | []          -> begin try (is, storesol_add l pl ctv ss) with TypeDoesntFit -> assert false end
            | [(_, ctv2)] -> (equalize_ctypes ctv ctv2 is, ss)
            | _           -> assert false
        end
    | ISeq (n, m) ->
        let (ld, is) = LDesc.shrink_period m equalize_ctypes is (prestore_find l ss) in
        let pl       = PLSeq n in
        let pcts     = LDesc.find pl ld in
        let is       = List.fold_left (fun is (_, ctv2) -> equalize_ctypes ctv ctv2 is) is pcts in
          if List.exists (fun (pl2, _) -> ploc_periodic pl2 && ploc_le pl2 pl) pcts then
            (* If this sequence is included in an existing one, there's nothing left to do *)
            (is, ss)
          else
            (* Otherwise, remove "later", overlapping elements and add this sequence.
               (Note if there's no including sequence, all the elements we found previously
                come after this one.) *)
            let ld = List.fold_left (fun ld (pl2, _) -> LDesc.remove pl2 ld) ld pcts in
            let ld = try LDesc.add pl ctv ld with TypeDoesntFit -> assert false in
              (is, SLM.add l ld ss)

let storecstr_sat (SCInc (l, iv, ctv): storecstr) (is: indexsol) (ss: storesol): bool =
  let ld = prestore_find l ss in
  let ct = ctypevar_apply is ctv in
    match indexsol_find iv is with
      | IBot   -> true
      | IInt n ->
          begin match LDesc.find (PLAt n) ld with
            | [(_, ctv2)] -> ct = ctypevar_apply is ctv2
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
                    | [(pl2, ctv2)] -> m mod p = 0 && ploc_contains pl2 pl p && ct = ctypevar_apply is ctv2
                    | _             -> false

(******************************************************************************)
(*************************** Systems of Constraints ***************************)
(******************************************************************************)

type cstr =
  | CSIndex of indexcstr
  | CSCType of ctypecstr
  | CSStore of storecstr

let mk_iless (ie: indexexp) (iv: indexvar) =
  CSIndex (ICLess (ie, iv))

let mk_subty (ctv1: ctypevar) (ctv2: ctypevar) =
  CSCType (CTCSubtype (ctv1, ctv2))

let mk_storeinc (s: sloc) (iv: indexvar) (ctv: ctypevar) =
  CSStore (SCInc (s, iv, ctv))

type store_unifier =
  | SUnify of sloc * sloc

let apply_unifiers (us: store_unifier list) (pct: 'a prectype): 'a prectype =
  List.fold_right (fun (SUnify (s1, s2)) pct -> prectype_replace_sloc s1 s2 pct) us pct

type cstrsol = store_unifier list * indexsol * storesol

let cstr_replace_sloc (s1: sloc) (s2: sloc): cstr -> cstr = function
  | CSCType ctc -> CSCType (ctypecstr_replace_sloc s1 s2 ctc)
  | CSStore sc  -> CSStore (storecstr_replace_sloc s1 s2 sc)
  | CSIndex ic  -> CSIndex ic

let cstr_sat ((_, is, ss): cstrsol): cstr -> bool = function
  | CSIndex ic  -> indexcstr_sat ic is
  | CSCType ctc -> ctypecstr_sat ctc is
  | CSStore sc  -> storecstr_sat sc is ss

let refine ((is, ss): indexsol * storesol): cstr -> indexsol * storesol = function
  | CSIndex (ICLess (ie, iv))         -> (refine_index ie iv is, ss)
  | CSCType (CTCSubtype (ctv1, ctv2)) -> (refine_ctype ctv1 ctv2 is, ss)
  | CSStore (SCInc (l, iv, ctv))      -> refine_store l iv ctv is ss

let rec solve_rec (cs: cstr list) ((sus, is, ss) as csol: cstrsol): cstrsol =
  match (try Some (List.find (fun c -> not (cstr_sat csol c)) cs) with Not_found -> None) with
    | None   -> csol
    | Some c ->
        let (cs, sus, is, ss) =
          try
            let (is, ss) = refine (is, ss) c in
              (cs, sus, is, ss)
          with
            | NoLUB (CTRef (s1, _), CTRef (s2, _)) ->
                let cs = List.map (cstr_replace_sloc s1 s2) cs in
                let ss = SLM.map (LDesc.map (prectype_replace_sloc s1 s2)) (SLM.remove s1 ss) in
                  (cs, SUnify (s1, s2) :: sus, is, ss)
            | NoLUB (ctv1, ctv2) ->
                ignore (P.printf "Can't LUB types %a and %a@!@!" d_ctype ctv1 d_ctype ctv2);
                assert false
            | _ -> assert false
        in solve_rec cs (sus, is, ss)

let solve (cs: cstr list): cstrsol =
  solve_rec cs ([], IVM.empty, SLM.empty)

(******************************************************************************)
(***************************** CIL Types to CTypes ****************************)
(******************************************************************************)

let int_width = C.bytesSizeOfInt C.IInt

let rec typ_width (t: C.typ): int =
  match C.unrollType t with
    | C.TInt (ik, _)                    -> C.bytesSizeOfInt ik
    | C.TPtr _                          -> 1
    | C.TComp (ci, _) when ci.C.cstruct -> List.fold_left (fun w fi -> w + typ_width fi.C.ftype) 0 ci.C.cfields
    | _                                 -> failure "Don't know type width"

let fresh_ctypevar: C.typ -> ctypevar = function
  | C.TInt (ik, _) -> fresh_ctvint (C.bytesSizeOfInt ik)
  | C.TPtr (_, _)  -> fresh_ctvref ()
  | _              -> failure "Tried to fresh crazy type"

(******************************************************************************)
(******************************* Shape Solutions ******************************)
(******************************************************************************)

(* pmr: need some much better way of uniquely identifying expressions! *)
(* pmr: need to catch lvals, too *)
module ExpKey = struct
  type t      = (* instruction location: *) int * C.exp
  let compare = compare
end

module ExpMap = Map.Make (ExpKey)

module ExpMapPrinter = P.MakeMapPrinter(ExpMap)

type ctvemap = ctypevar ExpMap.t

type ctemap = ctype ExpMap.t

let d_ctemap () (em: ctemap): Pretty.doc =
  ExpMapPrinter.d_map "\n" (fun () (_, e) -> C.d_exp () e) d_ctype () em

module IM = M.IntMap

type ctvenv = ctypevar IM.t

type cstremap = ctvemap * cstr list

(******************************************************************************)
(**************************** Constraint Generation ***************************)
(******************************************************************************)

let constrain_const: C.constant -> ctypevar * cstr = function
  | C.CInt64 (v, ik, so) ->
      let iv = fresh_indexvar () in
        (CTInt (C.bytesSizeOfInt ik, iv), mk_iless (IEInt (Int64.to_int v)) iv)
  | _ -> failure "Don't handle non-int constants yet"

let rec constrain_exp_aux (ve: ctvenv) (em: cstremap) (sid: int): C.exp -> ctypevar * cstremap * cstr list = function
  | C.Const c                                  -> let (ctv, c) = constrain_const c in (ctv, em, [c])
  | C.Lval lv                                  -> let (ctv, em) = constrain_lval ve em sid lv in (ctv, em, [])
  | C.BinOp (C.PlusA, e1, e2, _)               -> constrain_plus ve em sid e1 e2
  | C.BinOp (C.PlusPI, e1, e2, C.TPtr (t, _))  -> constrain_ptrplus ve em sid t e1 e2
  | C.BinOp (C.IndexPI, e1, e2, C.TPtr (t, _)) -> constrain_ptrplus ve em sid t e1 e2
  | C.CastE (_, e)                             -> constrain_exp_aux ve em sid e
  | e                                          -> ignore (P.printf "Got crazy exp: %a@!@!" C.d_exp e); assert false

and constrain_plus (ve: ctvenv) (em: cstremap) (sid: int) (e1: C.exp) (e2: C.exp): ctypevar * cstremap * cstr list =
  let (ctv1, em1) = constrain_exp ve em sid e1 in
  let (ctv2, em2) = constrain_exp ve em1 sid e2 in
    match (ctv1, ctv2, fresh_ctvint int_width) with
      | (CTInt (n1, iv1), CTInt (n2, iv2), (CTInt (n3, iv) as ctv)) when n1 = n3 && n2 = n3 ->
          (ctv, em2, [mk_iless (IEPlus (iv1, 1, iv2)) iv])
      | _ -> failure "Type mismatch in constraining arithmetic plus"

and constrain_ptrplus (ve: ctvenv) (em: cstremap) (sid: int) (t: C.typ) (e1: C.exp) (e2: C.exp): ctypevar * cstremap * cstr list =
  let (ctv1, em1) = constrain_exp ve em sid e1 in
  let (ctv2, em2) = constrain_exp ve em1 sid e2 in
    match (ctv1, ctv2) with
      | (CTRef (s, iv1), CTInt (n, iv2)) when n = int_width ->
          let iv = fresh_indexvar () in
            (CTRef (s, iv), em2, [mk_iless (IEPlus (iv1, typ_width t, iv2)) iv])
      | _ -> failure "Type mismatch in constraining pointer plus"

and constrain_lval (ve: ctvenv) (em: cstremap) (sid: int): C.lval -> ctypevar * cstremap = function
  | (C.Var v, C.NoOffset)       -> (IM.find v.C.vid ve, em)
  | (C.Mem e, C.NoOffset) as lv ->
      let (ctv, (ctvm, cs)) = constrain_exp ve em sid e in
      let ctv2              = fresh_ctvref () in
        begin match ctv2 with
          | CTRef (s, iv) ->
              let ctvlv = fresh_ctypevar <| C.typeOfLval lv in
              let cs    = mk_storeinc s iv ctvlv :: mk_subty ctv ctv2 :: cs in
                (ctvlv, (ctvm, cs))
          | _ -> failure "fresh_ctvref gave back non-ref type"
        end
  | lv -> ignore (P.printf "Got crazy lval: %a@!@!" C.d_lval lv); failure "Don't know how to handle fancy lvals yet"

and constrain_exp (ve: ctvenv) (em: cstremap) (sid: int) (e: C.exp): ctypevar * cstremap =
  let (ctv, (ctvm, cs), cs') = constrain_exp_aux ve em sid e in
    (ctv, (ExpMap.add (sid, e) ctv ctvm, cs @ cs'))

let constrain_instr (ve: ctvenv) (em: cstremap): C.instr -> cstremap = function
  | C.Set (lv, e, _) ->
      (* pmr: going out on a limb, I'd say 0 is not the right value *)
      let (ctv1, em1)        = constrain_lval ve em 0 lv in
      let (ctv2, (ctvm, cs)) = constrain_exp ve em1 0 e in
        (ctvm, mk_subty ctv2 ctv1 :: cs)
  | _ -> failure "Can't handle fancy instructions yet"

let rec constrain_block (ve: ctvenv) (em: cstremap) (b: C.block): cstremap =
  List.fold_left (constrain_stmt ve) em b.C.bstmts

(* pmr: set currentLoc so we can have reasonable error messages *)
and constrain_stmt (ve: ctvenv) (em: cstremap) (s: C.stmt): cstremap =
  match s.C.skind with
    | C.Block b            -> constrain_block ve em b
    | C.Instr is           -> List.fold_left (constrain_instr ve) em is
    | C.If (e, b1, b2, _)  -> constrain_if ve em e b1 b2
    | C.Loop (b, _, _, _)  -> constrain_block ve em b
    | C.Break _            -> em
    | C.Continue _         -> em
    | C.Return (Some e, _) -> snd (constrain_exp ve em 0 e)
    | C.Return (None, _)   -> em
    | _                      -> failure "Don't know what to do with crazy statements!"

and constrain_if (ve: ctvenv) (em: cstremap) (e: C.exp) (b1: C.block) (b2: C.block): cstremap =
  let (ctv, (ctvm, cs)) = constrain_exp ve em 0 e in
  let em                = (ctvm, mk_subty ctv (fresh_ctvint int_width) :: cs) in
    constrain_block ve (constrain_block ve em b1) b2

(* pmr: Possibly a hack for now just to get some store locations going *)
let constrain_param: ctypevar -> cstr option = function
  | CTRef (s, iv) -> Some (mk_iless (IEInt 0) iv)
  | ctv           -> None

let fresh_vars (vs: C.varinfo list): (int * ctypevar) list =
  List.map (fun v -> (v.C.vid, fresh_ctypevar v.C.vtype)) vs

let infer_shapes (fd: C.fundec): ctemap * store =
  let locals       = fresh_vars fd.C.slocals in
  let formals      = fresh_vars fd.C.sformals in
  let cs           = M.map_partial (M.compose constrain_param snd) formals in
  let ve           = List.fold_left (fun ve (vid, ctv) -> IM.add vid ctv ve) IM.empty <| locals @ formals in
  let (ctvm, cs)   = constrain_block ve (ExpMap.empty, cs) fd.C.sbody in
  let (us, is, ss) = solve cs in
  let apply_sol    = M.compose (apply_unifiers us) (ctypevar_apply is) in
    (ExpMap.map apply_sol ctvm, SLM.map (LDesc.map apply_sol) ss)
