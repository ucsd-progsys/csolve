(* what if we assert two different locs for a pointer in memory?
   make sure we crash if types conflict! *)

module C   = Cil
module E   = Errormsg
module P   = Pretty
module M   = Misc
module CM  = CilMisc
module SM  = M.StringMap
module A   = Ast
module FI  = FixInterface
module Ct  = Ctypes
module I   = Ct.Index
module S   = Sloc

module RCt = Ctypes.RefCTypes.CType
module RFl = Ctypes.RefCTypes.Field
module RLD = Ctypes.RefCTypes.LDesc
module RS  = Ctypes.RefCTypes.Store
module RCf = Ctypes.RefCTypes.CFun
module RSp = Ctypes.RefCTypes.Spec

open M.Ops

(******************************************************************************)
(************************* Annotations From Attributes ************************)
(******************************************************************************)

let slocAttribute = "lcc_sloc"
let predAttribute = "lcc_predicate"

let indexOfAttrs ats = 
  if CM.has_pos_attr ats then I.nonneg else I.top

let slocOfAttrs =
  let slocTable = Hashtbl.create 17 in
    fun ats ->
         ats
      |> CM.getStringAttrs slocAttribute
      |> M.ex_one "Cannot get single sloc"
      |> M.do_memo slocTable S.fresh_abstract []

let ptrIndexOfAttrs ats =
  I.mk_singleton 0

let typePredicate t =
      t
  |>  CM.typeAttrs
  |>  CM.getStringAttrs predAttribute
  |>: (Lexing.from_string <+> RefParse.pred RefLex.token)
  |>  A.pAnd

(******************************************************************************)
(***************************** Type Preprocessing *****************************)
(******************************************************************************)

let freshSlocName, _ = M.mk_string_factory "LOC"

(* Should only bother with pointer sigs *)
let ensureSloc t =
  let ats = CM.typeAttrs t in
    if C.hasAttribute slocAttribute ats then
      t
    else
      C.typeAddAttributes [C.Attr (slocAttribute, [C.AStr (freshSlocName ())])] t

(******************************************************************************)
(***************** Conversion from CIL Types to Refined Types *****************)
(******************************************************************************)

let ctypeOfCilBaseType = function
  | C.TVoid ats        -> Ct.Int (0,                           indexOfAttrs ats)
  | C.TInt (ik,   ats) -> Ct.Int (C.bytesSizeOfInt ik,         indexOfAttrs ats)
  | C.TFloat (fk, ats) -> Ct.Int (CM.bytesSizeOfFloat fk,      indexOfAttrs ats)
  | C.TEnum (ei,  ats) -> Ct.Int (C.bytesSizeOfInt ei.C.ekind, indexOfAttrs ats)
  | C.TPtr (t, ats)    -> Ct.Ref (slocOfAttrs ats,             ptrIndexOfAttrs ats)
  | _                  -> assertf "ctypeOfCilBaseType: non-base!"

let refctypeOfCilType t =
  FI.t_pred (ctypeOfCilBaseType t) (A.Symbol.of_string "V") (typePredicate t)

let addReftypeToStore sto loc s i rct =
     rct
  |> RS.Data.add_and_fold_overlap sto loc begin fun _ sto ct1 ct2 ->
       if ct1 = ct2 then ((), sto) else
         E.s <| C.errorLoc loc "Conflicting types for store location %a, index %a: %a, %a"
             S.d_sloc s Ct.Index.d_index i RCt.d_ctype ct1 RCt.d_ctype ct2
     end () s i
  |> snd

let addTypeToStore loc sto s i t =
  t |> refctypeOfCilType |> addReftypeToStore loc sto s i

(* todo: memoize on type sig *)
(* make sure that the sloc we ensure is always the one that we use in both
   places *)
let rec closeTypeInStore loc sto = function
  | t when not (C.isPointerType t) -> sto
  | C.TPtr (t, ats)                ->
      (* Note i should be determined by atts plus i passed in *)
      let s   = slocOfAttrs ats in
      let t   = ensureSloc t in
      let sto = closeTypeInStore loc sto t in
        addTypeToStore sto loc s (I.mk_singleton 0) t
  | _ -> assert false

let argType (x, t, ats) =
  (* is ats ever non-empty? otherwise we're duplicating toTypeSig here *)
  (x, t |> C.typeAddAttributes ats |> ensureSloc)

let refstoreOfTypes ts =
  List.fold_left (closeTypeInStore C.locUnknown) RS.empty ts

let refcfunOfType t =
  let ret, argso, _, _ = C.splitFunctionType t in
  let argts            = argso |> C.argsToList |>: argType in
  let sto              = refstoreOfTypes (ret :: List.map snd argts) in
    some <|
      RCf.make
        (List.map (M.app_snd refctypeOfCilType) argts)
        []
        sto
        (refctypeOfCilType ret)
        sto

let fundefsOfFile cil = 
  C.foldGlobals cil begin fun acc -> function
    | C.GFun (fd, _) as g -> SM.add fd.C.svar.C.vname g acc
    | _                   -> acc
  end SM.empty

let isBuiltin = Misc.is_prefix "__builtin"

let updFunm spec funm loc fn = function
  | _ when SM.mem fn spec     -> funm
  | _ when isBuiltin fn       -> funm
  | t when C.isFunctionType t -> M.sm_protected_add false fn (refcfunOfType t) funm
  | _                         -> funm 

let funspecsOfFunm funspec funm =
     SM.empty
  |> SM.fold begin fun _ d funm -> match d with 
     | C.GFun (fd, loc)    -> updFunm funspec funm loc fd.C.svar.C.vname fd.C.svar.C.vtype
     | C.GVarDecl (v, loc) -> updFunm funspec funm loc v.C.vname v.C.vtype
     | _                   -> funm
     end funm 
  |> Misc.sm_bindings
  |> Misc.map_partial (function (x, Some y) -> Some (x,y) | _ -> None)

let specsOfFile spec file =
  let fn = file |> fundefsOfFile |> funspecsOfFunm (RSp.funspec spec) in
    (fn, [], RS.empty)
