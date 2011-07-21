(* what if we assert two different locs for a pointer in memory?
   make sure we crash if types conflict! *)

module C   = Cil
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

let typeSigPredicate ts =
      ts
  |>  C.typeSigAttrs
  |>  CM.getStringAttrs predAttribute
  |>: (Lexing.from_string <+> RefParse.pred RefLex.token)
  |>  A.pAnd

(******************************************************************************)
(***************************** Type Preprocessing *****************************)
(******************************************************************************)

let freshSlocName, _ = M.mk_string_factory "LOC"

let ensureSloc ts =
  let ats = C.typeSigAttrs ts in
    if C.hasAttribute slocAttribute ats then
      ts
    else
      CM.typeSigAddAttrs ts [C.Attr (slocAttribute, [C.AStr (freshSlocName ())])]

(******************************************************************************)
(***************** Conversion from CIL Types to Refined Types *****************)
(******************************************************************************)

let toTypeSig t =
  t |> C.typeSig |> ensureSloc

let ctypeOfCilBaseTypeSig = function
  | C.TSBase t ->
      begin match t with
        | C.TVoid ats        -> Ct.Int (0,                           indexOfAttrs ats)
        | C.TInt (ik,   ats) -> Ct.Int (C.bytesSizeOfInt ik,         indexOfAttrs ats)
        | C.TFloat (fk, ats) -> Ct.Int (CM.bytesSizeOfFloat fk,      indexOfAttrs ats)
        | C.TEnum (ei,  ats) -> Ct.Int (C.bytesSizeOfInt ei.C.ekind, indexOfAttrs ats)
        | _                  -> assertf "ctypeOfCilBaseTypeSig: non-base!"
      end
  | C.TSPtr (t, ats) -> Ct.Ref (slocOfAttrs ats, ptrIndexOfAttrs ats)
  | _                -> assertf "ctypeOfCilBaseTypeSig: non-base!"

let refctypeOfCilTypeSig ts =
  FI.t_pred (ctypeOfCilBaseTypeSig ts) (A.Symbol.of_string "V") (typeSigPredicate ts)

(* todo: memoize on type sig *)
let rec refstoreOfTypeSig loc i = function
  | C.TSBase _        -> RS.empty
  | C.TSComp _        -> assert false
  | C.TSFun _         -> assert false
  | C.TSPtr (ts, ats) ->
      (* need memo here to avoid going into sloc loop *)
      (* Note i should be determined by atts plus i passed in *)
      let s   = slocOfAttrs ats in
      let ts  = ensureSloc ts in
      let sto = refstoreOfTypeSig loc (ptrIndexOfAttrs ats) ts in
        ts |> ldescOfTypeSig loc i |> RS.Data.add sto s
  | _ -> assert false

and ldescOfTypeSig loc i = function
  | C.TSBase _ as ts -> [(i, ts |> ensureSloc |> refctypeOfCilTypeSig |> RFl.create Ct.Nonfinal)] |> RLD.create loc 
  | _                -> assert false

let argTypeSig (x, t, ats) =
  (* is ats ever non-empty? otherwise we're duplicating toTypeSig here *)
  (x, t |> C.typeSig |> M.flip CM.typeSigAddAttrs ats |> ensureSloc)

let refstoreOfTypeSigs ts =
  ts |>: refstoreOfTypeSig C.locUnknown (I.mk_singleton 0) |>  List.fold_left RS.upd RS.empty

let refcfunOfType t =
  let ret, argso, _, _ = C.splitFunctionType t in
  let retts, argtss    = (toTypeSig ret, argso |> C.argsToList |>: argTypeSig) in
  let sto              = refstoreOfTypeSigs (retts :: List.map snd argtss) in
    some <|
      RCf.make
        (List.map (M.app_snd refctypeOfCilTypeSig) argtss)
        []
        sto
        (refctypeOfCilTypeSig retts)
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
