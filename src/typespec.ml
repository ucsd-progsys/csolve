module C   = Cil
module E   = Errormsg
module P   = Pretty
module M   = Misc
module CM  = CilMisc
module SM  = M.StringMap
module A   = Ast
module FI  = FixInterface
module FA  = FixAstInterface
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

let ensureSloc t =
  if C.isPointerType t then
    let ats = CM.typeAttrs t in
      if C.hasAttribute slocAttribute ats then
        t
      else
        C.typeAddAttributes [C.Attr (slocAttribute, [C.AStr (freshSlocName ())])] t
  else t

let arraySizes sz1 sz2 = match sz1, sz2 with
  | Some (C.Const (C.CInt64 (i, ik, _))), Some (C.Const (C.CInt64 (j, _, _))) ->
    Some (C.Const (C.CInt64 (Int64.mul i j, ik, None)))
  | None, None -> None
  | _          -> assert false

let rec flattenArray = function
  | C.TArray (C.TArray (c, iszo, _), oszo, _) ->
    flattenArray (C.TArray (c, arraySizes iszo oszo, []))
  | t -> t

let indexOfArrayElements t b = match t, b with
  | t, Some (C.Const (C.CInt64 (i, _, _))) ->
    let sz = CM.bytesSizeOf t in
    let c  = Int64.to_int i - 1 in
      I.mk_sequence 0 sz (Some 0) (Some (c * sz))
  | t, None -> I.mk_sequence 0 (CM.bytesSizeOf t) (Some 0) None

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

let rec componentsOfType t = match t |> C.unrollType |> flattenArray with
  | C.TArray (t, b, _)   -> [("", indexOfArrayElements t b, ensureSloc t)]
  | C.TComp (ci, _) as t ->
    M.flap
      begin fun f -> match componentsOfField t f with
        | [(_, i, t)] -> [(f.C.fname, i, t)]
        | cs          -> cs
      end ci.C.cfields
  | t -> [("", I.mk_singleton 0, ensureSloc t)]

and componentsOfField t f =
  let off = C.Field (f, C.NoOffset) |> CM.bytesOffset t |> I.mk_singleton in
       f.C.ftype
    |> componentsOfType
    |> List.map (M.app_snd3 <| I.plus off)

let rec closeTypeInStore loc sto t = match C.unrollType t with
  | C.TPtr (t, ats) ->
    let tcs    = componentsOfType t in
    let s      = slocOfAttrs ats in
    let fldsub = List.map (fun (fn, i, _) -> (FA.name_of_string fn, FI.name_of_sloc_index s i)) tcs in
      List.fold_left
        begin fun sto ((_, i, t) as tc) ->
          let sto = closeTypeComponentInStore loc sto tc in
               t
            |> refctypeOfCilType
            |> FI.t_subs_names fldsub
            |> addReftypeToStore sto loc s i
        end sto tcs
  | _ -> sto

and closeTypeComponentInStore loc sto (_, _, t) =
  closeTypeInStore loc sto t

let argType (x, t, ats) =
  (x, t |> C.typeAddAttributes ats |> ensureSloc)

let refstoreOfTypes ts =
  List.fold_left (closeTypeInStore C.locUnknown) RS.empty ts

let refcfunOfType t =
  let ret, argso, _, _ = C.splitFunctionType t in
  let ret              = ensureSloc ret in
  let argts            = argso |> C.argsToList |>: argType in
  let sto              = refstoreOfTypes (ret :: List.map snd argts) in
    some <|
      RCf.make
        (List.map (M.app_snd refctypeOfCilType) argts)
        []
        sto
        (refctypeOfCilType ret)
        sto

(******************************************************************************)
(******************************* Gathering Specs ******************************)
(******************************************************************************)

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
