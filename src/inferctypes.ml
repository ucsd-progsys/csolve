module M  = Misc
module P  = Pretty
module C  = Cil
module Cs = Constants
module E  = Errormsg
module ST = Ssa_transform

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

type indexcstr =
  | ICVarLess of indexexp * indexvar
  | ICConstLess of indexexp * index

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
  | IEConst i             -> i
  | IEVar iv              -> indexsol_find iv is
  | IEPlus (iv1, x, iv2)  -> index_plus (indexsol_find iv1 is) (index_scale x <| indexsol_find iv2 is)
  | IEMinus (iv1, x, iv2) -> index_minus (indexsol_find iv1 is) (index_scale x <| indexsol_find iv2 is)
  | IEMult (iv1, iv2)     -> index_mult (indexsol_find iv1 is) (indexsol_find iv2 is)

let refine_index (ie: indexexp) (iv: indexvar) (is: indexsol): indexsol =
  IVM.add iv (index_lub (indexexp_apply is ie) (indexsol_find iv is)) is

let indexcstr_sat (ic: indexcstr) (is: indexsol): bool =
  match ic with
    | ICVarLess (ie, iv)  -> is_subindex (indexexp_apply is ie) (indexsol_find iv is)
    | ICConstLess (ie, i) -> is_subindex (indexexp_apply is ie) i

(******************************************************************************)
(****************************** Type Constraints ******************************)
(******************************************************************************)

type ctypevar = indexvar prectype

let with_fresh_sloc (f: sloc -> 'a): 'a =
  fresh_sloc () |> f

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
            | []          -> (is, storesol_add l pl ctv ss)
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
            let ld = LDesc.add pl ctv ld in
              (is, SLM.add l ld ss)
    | ITop ->
        let (ld, is) = LDesc.shrink_period (prectype_width ctv) equalize_ctypes is (prestore_find l ss) in
        let (ld, is) = LDesc.foldn (fun _ (ld, is) pl ctv2 -> (LDesc.remove pl ld, equalize_ctypes ctv ctv2 is)) (ld, is) ld in
          (is, SLM.add l (LDesc.add PLEverywhere ctv ld) ss)

let storecstr_sat (SCInc (l, iv, ctv): storecstr) (is: indexsol) (ss: storesol): bool =
  let ld = prestore_find l ss in
  let ct = ctypevar_apply is ctv in
    match indexsol_find iv is with
      | IBot -> true
      | ITop ->
          begin match LDesc.find PLEverywhere ld with
            | [(PLEverywhere, ctv2)] -> ct = ctypevar_apply is ctv2
            | _                      -> false
          end
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

let mk_storeinc (loc: C.location) (s: sloc) (iv: indexvar) (ctv: ctypevar) =
  {cdesc = CSStore (SCInc (s, iv, ctv)); cloc = loc}

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

let mk_const_storeinc (loc: C.location) (l: sloc) (i: index) (ct: ctype): cstr list =
  with_fresh_indexvar <| fun iv ->
    let (ctv, cs) = ctypevar_of_ctype loc ct in
      cs @ [mk_ivarless loc (IEConst i) iv; mk_storeinc loc l iv ctv]

type store_unifier =
  | SUnify of sloc * sloc

let apply_unifiers (us: store_unifier list) (pct: 'a prectype): 'a prectype =
  List.fold_right (fun (SUnify (s1, s2)) pct -> prectype_replace_sloc s1 s2 pct) us pct

type cstrsol = store_unifier list * indexsol * storesol

let cstrdesc_replace_sloc (s1: sloc) (s2: sloc): cstrdesc -> cstrdesc = function
  | CSCType ctc -> CSCType (ctypecstr_replace_sloc s1 s2 ctc)
  | CSStore sc  -> CSStore (storecstr_replace_sloc s1 s2 sc)
  | CSIndex ic  -> CSIndex ic

let cstr_replace_sloc (s1: sloc) (s2: sloc) (c: cstr): cstr =
  {c with cdesc = cstrdesc_replace_sloc s1 s2 c.cdesc}

let cstrdesc_sat ((_, is, ss): cstrsol): cstrdesc -> bool = function
  | CSIndex ic  -> indexcstr_sat ic is
  | CSCType ctc -> ctypecstr_sat ctc is
  | CSStore sc  -> storecstr_sat sc is ss

let cstr_sat (csol: cstrsol) (c: cstr): bool =
  cstrdesc_sat csol c.cdesc

let refine ((is, ss): indexsol * storesol): cstrdesc -> indexsol * storesol = function
  | CSIndex (ICVarLess (ie, iv))      -> (refine_index ie iv is, ss)
  | CSIndex (ICConstLess (ie, i))     -> E.s <| E.error "Index constraint violation: %a <= %a@!@!" d_index (indexexp_apply is ie) d_index i
  | CSCType (CTCSubtype (ctv1, ctv2)) -> (refine_ctype ctv1 ctv2 is, ss)
  | CSStore (SCInc (l, iv, ctv))      ->
      try
        refine_store l iv ctv is ss
      with TypeDoesntFit ->
        let i  = indexsol_find iv is in
        let ct = ctypevar_apply is ctv in
        let ld = LDesc.map (ctypevar_apply is) (prestore_find l ss) in
          E.error "Can't fit %a |-> %a in location %a: @!@!%a@!@!" 
            d_index i d_ctype ct d_sloc l (LDesc.d_ldesc d_ctype) ld;
          raise TypeDoesntFit

let rec solve_rec (cs: cstr list) ((sus, is, ss) as csol: cstrsol): cstrsol =
  match (try Some (List.find (fun c -> not (cstr_sat csol c)) cs) with Not_found -> None) with
    | None   -> csol
    | Some c ->
        let (cs, sus, is, ss) =
          try
            let (is, ss) = refine (is, ss) c.cdesc in
              (cs, sus, is, ss)
          with
            | NoLUB (CTRef (s1, _), CTRef (s2, _)) ->
                let cs = List.map (cstr_replace_sloc s1 s2) cs in
                let ss = SLM.map (LDesc.map (prectype_replace_sloc s1 s2)) (SLM.remove s1 ss) in
                  (cs, SUnify (s1, s2) :: sus, is, ss)
            | NoLUB (ctv1, ctv2) -> E.s <| Cil.errorLoc c.cloc "Incompatible types: %a, %a@!@!" d_ctype ctv1 d_ctype ctv2
            | _                  -> E.s <| Cil.errorLoc c.cloc "Unknown error"
        in solve_rec cs (sus, is, ss)

let solve (cs: cstr list): cstrsol =
  solve_rec cs ([], IVM.empty, SLM.empty)

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
    | C.TInt (ik, _) -> fresh_ctvint (C.bytesSizeOfInt ik)
    | C.TPtr (_, _)  -> fresh_ctvref ()
    | _              -> E.s <| E.bug "Unimplemented fresh_ctypevar: %a@!@!" C.d_type t

(******************************************************************************)
(******************************* Shape Solutions ******************************)
(******************************************************************************)

(* pmr: need to check that expressions have unique types (which should certainly hold anyway) *)
module ExpKey = struct
  type t      = C.exp
  let compare = compare
end

module ExpMap = Map.Make (ExpKey)

module ExpMapPrinter = P.MakeMapPrinter(ExpMap)

type ctvemap = ctypevar ExpMap.t

type ctemap = ctype ExpMap.t

let d_ctemap () (em: ctemap): Pretty.doc =
  ExpMapPrinter.d_map "\n" C.d_exp d_ctype () em

let d_vartypes () vars =
  P.docList ~sep:(P.dprintf "@!") (fun (v, ct) -> P.dprintf "%s: %a" v.C.vname Ctypes.d_ctype ct) () vars

module IM = M.IntMap

type ctvenv = ctypevar IM.t

type cstremap = ctvemap * cstr list

(******************************************************************************)
(************************ Function Stubs (REMOVE ASAP) ************************)
(******************************************************************************)

let malloc_stub (loc: C.location): ctypevar option * ctypevar list * cstr list =
  with_fresh_sloc <| fun s -> with_fresh_indexvar <| fun iv -> (Some (CTRef (s, iv)), [fresh_ctvint int_width], [mk_ivarless loc (IEConst (IInt 0)) iv])

let free_stub (loc: C.location): ctypevar option * ctypevar list * cstr list =
  (None, [fresh_ctvref ()], [])

let bzero_stub (loc: C.location): ctypevar option * ctypevar list * cstr list =
  (None, [fresh_ctvref (); fresh_ctvint int_width], [])

let nondet_stub (loc: C.location): ctypevar option * ctypevar list * cstr list =
  with_fresh_indexvar <| fun iv -> (Some (CTInt (int_width, iv)), [], [mk_ivarless loc (IEConst ITop) iv])

let assert_stub (loc: C.location): ctypevar option * ctypevar list * cstr list =
  (None, [fresh_ctvint int_width], [])

let fopen_stub (loc: C.location): ctypevar option * ctypevar list * cstr list =
  (Some (fresh_ctvref ()), [fresh_ctvref (); fresh_ctvref ()], [])

let fclose_stub (loc: C.location): ctypevar option * ctypevar list * cstr list =
  (None, [fresh_ctvref ()], [])

let fflush_stub (loc: C.location): ctypevar option * ctypevar list * cstr list =
  with_fresh_indexvar <| fun iv -> (Some (CTInt (int_width, iv)), [fresh_ctvref ()], [mk_ivarless loc (IEConst ITop) iv])

let feof_stub (loc: C.location): ctypevar option * ctypevar list * cstr list =
  with_fresh_indexvar <| fun iv -> (Some (CTInt (int_width, iv)), [fresh_ctvref ()], [mk_ivarless loc (IEConst ITop) iv])

let fgetc_stub (loc: C.location): ctypevar option * ctypevar list * cstr list =
  with_fresh_indexvar <| fun iv -> (Some (CTInt (int_width, iv)), [fresh_ctvref ()], [mk_ivarless loc (IEConst (ISeq (-1, 1))) iv])

let fputc_stub (loc: C.location): ctypevar option * ctypevar list * cstr list =
  with_fresh_indexvar <| fun iv -> (Some (CTInt (int_width, iv)), [fresh_ctvint int_width; fresh_ctvref ()], [mk_ivarless loc (IEConst ITop) iv])

let gets_stub (loc: C.location): ctypevar option * ctypevar list * cstr list =
  with_fresh_sloc <| fun s -> with_fresh_indexvar <| fun iv -> (Some (CTRef (s, iv)), [CTRef (s, iv)], [])

let atoi_stub (loc: C.location): ctypevar option * ctypevar list * cstr list =
  with_fresh_indexvar <| fun iv -> (Some (CTInt (int_width, iv)), [fresh_ctvref ()], [mk_ivarless loc (IEConst ITop) iv])

let __ctype_b_loc_stub (loc: C.location): ctypevar option * ctypevar list * cstr list =
  with_fresh_sloc <| fun s1 -> with_fresh_indexvar <| fun iv1 ->
    with_fresh_sloc <| fun s2 -> with_fresh_indexvar <| fun iv2 -> with_fresh_indexvar <| fun ic -> with_fresh_indexvar <| fun id ->
      (Some (CTRef (s1, iv1)), [], [mk_ivarless loc (IEConst (IInt 0)) iv1;
                                    mk_ivarless loc (IEConst (IInt 128)) iv2;
                                    mk_storeinc loc s1 iv1 (CTRef (s2, iv2));
                                    mk_ivarless loc (IEConst (ISeq (0, short_width))) id;
                                    mk_ivarless loc (IEConst ITop) ic;
                                    mk_storeinc loc s2 id (CTInt (short_width, ic))])

let exit_stub (loc: C.location): ctypevar option * ctypevar list * cstr list =
  (None, [fresh_ctvint int_width], [])

let tolower_stub (loc: C.location): ctypevar option * ctypevar list * cstr list =
  if !Constants.safe then E.s <| E.bug "Can't assume tolower's param is a letter@!" else C.warnLoc loc "Unsoundly assuming tolower is passed a letter@!" |> ignore;
  with_fresh_indexvar <| fun iv -> (Some (CTInt (int_width, iv)), [fresh_ctvint int_width], [mk_ivarless loc (IEConst (ISeq (97, 1))) iv])

let longjmp_stub (loc: C.location): ctypevar option * ctypevar list * cstr list =
  (None, [fresh_ctvref (); fresh_ctvint int_width], [])

let _setjmp_stub (loc: C.location): ctypevar option * ctypevar list * cstr list =
  with_fresh_indexvar <| fun iv -> (Some (CTInt (int_width, iv)), [fresh_ctvref (); fresh_ctvint int_width], [mk_ivarless loc (IEConst ITop) iv])

let qsort_stub (loc: C.location): ctypevar option * ctypevar list * cstr list =
  (None, [fresh_ctvref (); fresh_ctvint int_width; fresh_ctvint int_width; fresh_ctvref ()], [])

let isatty_stub (loc: C.location): ctypevar option * ctypevar list * cstr list =
  with_fresh_indexvar <| fun iv -> (Some (CTInt (int_width, iv)), [fresh_ctvint int_width], [mk_ivarless loc (IEConst (ISeq (0, 1))) iv])

let fun_stubs =
  [
    ("malloc", malloc_stub);
    ("free", free_stub);
    ("bzero", bzero_stub);
    ("nondet", nondet_stub);
    ("assert", assert_stub);
    ("fopen", fopen_stub);
    ("fclose", fclose_stub);
    ("fflush", fflush_stub);
    ("feof", feof_stub);
    ("fgetc", fgetc_stub);
    ("fputc", fputc_stub);
    ("gets", gets_stub);
    ("atoi", atoi_stub);
    ("__ctype_b_loc", __ctype_b_loc_stub);
    ("exit", exit_stub);
    ("tolower", tolower_stub);
    ("longjmp", longjmp_stub);
    ("_setjmp", _setjmp_stub);
    ("qsort", qsort_stub);
    ("isatty", isatty_stub);
  ]

let printf_funs = ["printf"; "fprintf"]

(******************************************************************************)
(**************************** Constraint Generation ***************************)
(******************************************************************************)

let constrain_const (loc: C.location): C.constant -> ctypevar * cstr = function
  | C.CInt64 (v, ik, _) -> with_fresh_indexvar <| fun iv -> (CTInt (C.bytesSizeOfInt ik, iv), mk_ivarless loc (IEConst (index_of_int (Int64.to_int v))) iv)
  | C.CChr c            -> with_fresh_indexvar <| fun iv -> (CTInt (int_width, iv), mk_ivarless loc (IEConst (IInt (Char.code c))) iv)
  | c                   -> E.s <| E.bug "Unimplemented constrain_const: %a@!@!" C.d_const c

let rec constrain_exp_aux (ve: ctvenv) (em: cstremap) (loc: C.location): C.exp -> ctypevar * cstremap * cstr list = function
  | C.Const c                     -> let (ctv, c) = constrain_const loc c in (ctv, em, [c])
  | C.Lval lv                     -> let (ctv, em) = constrain_lval ve em loc lv in (ctv, em, [])
  | C.UnOp (uop, e, t)            -> constrain_unop uop ve em loc t e
  | C.BinOp (bop, e1, e2, t)      -> constrain_binop bop ve em loc t e1 e2
  | C.CastE (C.TPtr _, C.Const c) -> constrain_constptr em loc c
  | C.CastE (ct, e)               -> constrain_cast ve em loc ct e
  | C.AddrOf lv                   -> constrain_addrof ve em loc lv
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
    apply_binop op em2 loc t ctv1 ctv2

and apply_binop: C.binop -> cstremap -> C.location -> C.typ -> ctypevar -> ctypevar -> ctypevar * cstremap * cstr list = function
  | C.PlusA                                 -> constrain_arithmetic (fun iv1 iv2 -> IEPlus (iv1, 1, iv2))
  | C.MinusA                                -> constrain_arithmetic (fun iv1 iv2 -> IEMinus (iv1, 1, iv2))
  | C.Mult                                  -> constrain_arithmetic (fun iv1 iv2 -> IEMult (iv1, iv2))
  | C.Mod                                   -> constrain_mod
  | C.PlusPI | C.IndexPI                    -> constrain_ptrarithmetic (fun iv1 x iv2 -> IEPlus (iv1, x, iv2))
  | C.MinusPI                               -> constrain_ptrarithmetic (fun iv1 x iv2 -> IEMinus (iv1, x, iv2))
  | C.MinusPP                               -> constrain_ptrminus
  | C.Lt | C.Gt | C.Le | C.Ge | C.Eq | C.Ne -> constrain_rel
  | C.BAnd | C.BOr | C.BXor                 -> constrain_bitop
  | C.Shiftlt | C.Shiftrt                   -> constrain_shift
  | bop                                     -> E.s <| E.bug "Unimplemented apply_binop: %a@!@!" C.d_binop bop

and constrain_arithmetic (f: indexvar -> indexvar -> indexexp) (em: cstremap) (loc: C.location) (rt: C.typ) (ctv1: ctypevar) (ctv2: ctypevar): ctypevar * cstremap * cstr list =
    match (ctv1, ctv2, fresh_ctvint <| typ_width rt) with
      | (CTInt (n1, iv1), CTInt (n2, iv2), (CTInt (n3, iv) as ctv)) ->
          (ctv, em, [mk_ivarless loc (f iv1 iv2) iv])
      | _ -> E.s <| E.bug "Type mismatch in constrain_arithmetic@!@!"

and constrain_ptrarithmetic (f: indexvar -> int -> indexvar -> indexexp) (em: cstremap) (loc: C.location) (pt: C.typ) (ctv1: ctypevar) (ctv2: ctypevar): ctypevar * cstremap * cstr list =
    match (C.unrollType pt, ctv1, ctv2) with
      | (C.TPtr (t, _), CTRef (s, iv1), CTInt (n, iv2)) when n = int_width ->
          with_fresh_indexvar (fun iv -> (CTRef (s, iv), em, [mk_ivarless loc (f iv1 (typ_width t) iv2) iv]))
      | _ -> E.s <| E.bug "Type mismatch in constrain_ptrarithmetic@!@!"

and constrain_ptrminus (em: cstremap) (loc: C.location) (pt: C.typ) (_: ctypevar) (_: ctypevar): ctypevar * cstremap * cstr list =
  with_fresh_indexvar <| fun iv -> (CTInt (typ_width !C.upointType, iv), em, [mk_ivarless loc (IEConst ITop) iv])

and constrain_rel (em: cstremap) (loc: C.location) (_: C.typ) (_: ctypevar) (_: ctypevar): ctypevar * cstremap * cstr list =
  with_fresh_indexvar (fun iv -> (CTInt (int_width, iv), em, [mk_ivarless loc (IEConst (ISeq (0, 1))) iv]))

and constrain_bitop (em: cstremap) (loc: C.location) (rt: C.typ) (_: ctypevar) (_: ctypevar): ctypevar * cstremap * cstr list =
  with_fresh_indexvar (fun iv -> (CTInt (typ_width rt, iv), em, [mk_ivarless loc (IEConst ITop) iv]))

and constrain_shift (em: cstremap) (loc: C.location) (rt: C.typ) (_: ctypevar) (_: ctypevar): ctypevar * cstremap * cstr list =
  with_fresh_indexvar (fun iv -> (CTInt (typ_width rt, iv), em, [mk_ivarless loc (IEConst ITop) iv]))

and constrain_mod (em: cstremap) (loc: C.location) (rt: C.typ) (_: ctypevar) (_: ctypevar): ctypevar * cstremap * cstr list =
  with_fresh_indexvar (fun iv -> (CTInt (typ_width rt, iv), em, [mk_ivarless loc (IEConst ITop) iv]))

and constrain_constptr (em: cstremap) (loc: C.location): C.constant -> ctypevar * cstremap * cstr list = function
  | C.CStr _ ->
      with_fresh_sloc <| fun s -> with_fresh_indexvar <| fun ivr -> with_fresh_indexvar <| fun ivl -> with_fresh_indexvar begin fun ivc ->
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

type funmap = (cfun * Ssa_transform.ssaCfgInfo) Misc.StringMap.t

let instantiate_args (loc: C.location) (argcts: (string * ctype) list): ctypevar list * cstr list =
  let (argctvs, argcts) = argcts |> List.map (M.compose (ctypevar_of_ctype loc) snd) |> List.split in
    (argctvs, List.flatten argcts)

let instantiate_ret (loc: C.location): ctype option -> ctypevar option * cstr list = function
  | Some rct ->
      let (ctv, cs) = ctypevar_of_ctype loc rct in
        (Some ctv, cs)
  | None -> (None, [])

let instantiate_store (loc: C.location) (st: store): cstr list =
  prestore_fold (fun css l i ct -> mk_const_storeinc loc l i ct :: css) [] st |> List.concat

let instantiate_function (loc: C.location) (env: ctypeenv) (f: string): ctypevar option * ctypevar list * cstr list =
  let ({args = argcts; ret = rcto; abs_out = oas}, subs) = M.StringMap.find f env |> cfun_instantiate in
    (* pmr: do we need oas = ias on the common parts? *)
  let (argctvs, argcs) = instantiate_args loc argcts in
  let (rctvo, rctocs)  = instantiate_ret loc rcto in
  let storecs          = instantiate_store loc oas in
    (rctvo, argctvs, List.concat [storecs; rctocs; argcs])

let constrain_app (env: ctypeenv) (ve: ctvenv) (em: cstremap) (loc: C.location) (f: string) (lvo: C.lval option) (args: C.exp list): cstremap =
  let (ctvs, (ctvm, argcs)) = constrain_args ve em loc args in
  let (rtvo, atvs, ics)     = instantiate_function loc env f in
  let (ctvm, cs)            = (ctvm, List.concat [List.map2 (mk_subty loc) ctvs atvs; ics; argcs]) in
    match (lvo, rtvo) with
      | (None, _)           -> (ctvm, cs)
      | (Some _, None)      -> E.s <| C.errorLoc loc "Attempting to assign void value in call@!"
      | (Some lv, Some rtv) ->
          let (lvctv, (ctvm, cs)) = constrain_lval ve (ctvm, cs) loc lv in
            (ctvm, mk_subty loc rtv lvctv :: cs)

let constrain_instr (env: ctypeenv) (ve: ctvenv) (em: cstremap): C.instr -> cstremap = function
  | C.Set (lv, e, loc) ->
      let (ctv1, em1)        = constrain_lval ve em loc lv in
      let (ctv2, (ctvm, cs)) = constrain_exp ve em1 loc e in
        (ctvm, mk_subty loc ctv2 ctv1 :: cs)
  | C.Call (None, C.Lval (C.Var {C.vname = f}, C.NoOffset), args, loc) when List.mem f printf_funs ->
      if not !Constants.safe then C.warnLoc loc "Unsoundly ignoring printf-style call@!@!" |> ignore else E.s <| C.errorLoc loc "Can't handle printf";
      constrain_args ve em loc args |> snd
  | C.Call (lvo, C.Lval (C.Var {C.vname = f}, C.NoOffset), args, loc) ->
      constrain_app env ve em loc f lvo args
  | i -> E.s <| E.bug "Unimplemented constrain_instr: %a@!@!" C.dn_instr i

let rec constrain_block (env: ctypeenv) (ve: ctvenv) (em: cstremap) (b: C.block): cstremap =
  List.fold_left (constrain_stmt env ve) em b.C.bstmts

and constrain_stmt (env: ctypeenv) (ve: ctvenv) (em: cstremap) (s: C.stmt): cstremap =
  match s.C.skind with
    | C.Block b              -> constrain_block env ve em b
    | C.Instr is             -> List.fold_left (constrain_instr env ve) em is
    | C.If (e, b1, b2, _)    -> constrain_if env ve em e b1 b2
    | C.Loop (b, _, _, _)    -> constrain_block env ve em b
    | C.Break _              -> em
    | C.Continue _           -> em
    | C.Goto _               -> em
    | C.Return (Some e, loc) -> snd (constrain_exp ve em loc e)
    | C.Return (None, _)     -> em
    | _                      -> E.s <| E.bug "Unimplemented constrain_stmt: %a@!@!" C.dn_stmt s

and constrain_if (env: ctypeenv) (ve: ctvenv) (em: cstremap) (e: C.exp) (b1: C.block) (b2: C.block): cstremap =
  let (ctv, (ctvm, cs)) = constrain_exp ve em Cil.builtinLoc e in
    constrain_block env ve (constrain_block env ve (ctvm, cs) b1) b2

(* pmr: Possibly a hack for now just to get some store locations going *)
(* pmr: Let's be honest here: the following is flat-out wrong. *)
let constrain_param: ctypevar -> cstr option = function
  | CTRef (_, iv) -> if not !Cs.safe then Some (mk_ivarless Cil.builtinLoc (IEConst (IInt 0)) iv) else E.s <| E.error "Can't constrain reference parameter@!"
  | CTInt (_, iv) -> Some (mk_ivarless Cil.builtinLoc (IEConst ITop) iv)

let maybe_fresh (v: C.varinfo): (C.varinfo * ctypevar) option =
  let t = C.unrollType v.C.vtype in
  match t with
  | C.TInt _ 
  | C.TPtr _ -> Some (v, fresh_ctypevar t)
  | _        -> let _ = if !Constants.safe then E.error "not freshing local %s" v.C.vname in
                C.warnLoc v.C.vdecl "Not freshing local %s of tricky type %a@!@!" v.C.vname C.d_type t 
                |> ignore; None

let fresh_vars (vs: C.varinfo list): (C.varinfo * ctypevar) list =
  Misc.map_partial maybe_fresh vs

let mk_phi_defs_cs (ve: ctvenv) ((vphi, vdefs): C.varinfo * (int * C.varinfo) list): cstr list =
  List.map (fun (_, vdef) -> mk_subty vphi.C.vdecl (IM.find vdef.C.vid ve) (IM.find vphi.C.vid ve)) vdefs

let mk_phis_cs (ve: ctvenv) (phis: (C.varinfo * (int * C.varinfo) list) list array): cstr list =
  Array.to_list phis |> List.flatten |> List.map (mk_phi_defs_cs ve) |> List.concat

let infer_sci_shapes ({ST.fdec = fd; ST.phis = phis}: ST.ssaCfgInfo): (C.varinfo * ctype) list * ctemap * store =
  let locals       = fresh_vars fd.C.slocals in
  let formals      = fresh_vars fd.C.sformals in
  let ve           = List.fold_left (fun ve (v, ctv) -> IM.add v.C.vid ctv ve) IM.empty <| locals @ formals in
  let cs           = M.map_partial (M.compose constrain_param snd) formals in
  let cs           = mk_phis_cs ve phis @ cs in
  let (ctvm, cs)   = constrain_block M.StringMap.empty ve (ExpMap.empty, cs) fd.C.sbody in
  let (us, is, ss) = solve cs in
  let apply_sol    = M.compose (apply_unifiers us) (ctypevar_apply is) in
    (List.map (fun (v, ctv) -> (v, apply_sol ctv)) locals,
     ExpMap.map apply_sol ctvm,
     SLM.map (LDesc.map apply_sol) ss)

type shape = (C.varinfo * ctype) list * ctemap * store (* * block_annotation array *)

let constrain_cfg (env: ctypeenv) (vars: ctypevar IM.t) (cfg: Ssa.cfgInfo): cstremap =
  M.array_fold_lefti (fun i em b -> constrain_stmt env vars em b.Ssa.bstmt) (ExpMap.empty, []) cfg.Ssa.blocks

let infer_shape (env: ctypeenv) ({args = argcts; abs_in = ias}: cfun) ({ST.fdec = fd; ST.phis = phis; ST.cfg = cfg}: ST.ssaCfgInfo): shape =
  let loc                 = fd.C.svar.C.vdecl in
  let (formals, formalcs) = instantiate_args loc argcts in
  let bodyformals         = fresh_vars fd.C.sformals in
  let bodyformalcs        = List.map2 (fun f (_, bf) -> mk_subty loc f bf) formals bodyformals in
  let locals              = fresh_vars fd.C.slocals in
  let vars                = locals @ bodyformals |> List.fold_left (fun ve (v, ctv) -> IM.add v.C.vid ctv ve) IM.empty in
  let phics               = mk_phis_cs vars phis in
  let storecs             = instantiate_store loc ias in
  let (ctvm, bodycs)      = constrain_cfg env vars cfg in
  let (us, is, ss)        = List.concat [formalcs; bodyformalcs; phics; storecs; bodycs] |> solve in
  let apply_sol           = M.compose (apply_unifiers us) (ctypevar_apply is) in
    (List.map (fun (v, ctv) -> (v, apply_sol ctv)) locals,
     ExpMap.map apply_sol ctvm,
     SLM.map (LDesc.map apply_sol) ss)

let infer_shapes (env: ctypeenv) (scis: funmap): shape Misc.StringMap.t =
  M.StringMap.map (fun (cft, sci) -> infer_shape env (fst (cfun_instantiate cft)) sci) scis
