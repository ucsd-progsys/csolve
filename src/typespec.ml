module C   = Cil
module E   = Errormsg
module P   = Pretty
module M   = Misc
module SM  = M.StringMap
module CM  = CilMisc
module VS  = CM.VarSet
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

let slocAttribute     = "lcc_sloc"
let gslocAttribute    = "lcc_gsloc"
let predAttribute     = "lcc_predicate"
let externOkAttribute = "lcc_extern_ok"

let indexOfAttrs ats = 
  if CM.has_pos_attr ats then I.nonneg else I.top

let slocTable = Hashtbl.create 17

let getSlocAttribute ats =
  if C.hasAttribute slocAttribute ats then slocAttribute else gslocAttribute

let slocOfAttrs ats =
     ats
  |> CM.getStringAttrs (getSlocAttribute ats)
  |> M.ex_one "Type does not have a single sloc"
  |> M.do_memo slocTable S.fresh_abstract []

let getSlocByName n =
  Hashtbl.find n slocTable

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
  if C.isPointerType t || C.isArrayType t then
    let ats = CM.typeAttrs t in
      if C.hasAttribute slocAttribute ats || C.hasAttribute gslocAttribute ats then
        t
      else
        C.typeAddAttributes [C.Attr (slocAttribute, [C.AStr (freshSlocName ())])] t
  else t

let argType (x, t, ats) =
  (x, t |> C.typeAddAttributes ats |> ensureSloc)

let nameArg =
  let freshArgName, _ = M.mk_string_factory "ARG" in
    fun x -> if x = "" then freshArgName () else x

let arraySizes sz1 sz2 = match sz1, sz2 with
  | Some (C.Const (C.CInt64 (i, ik, _))), Some (C.Const (C.CInt64 (j, _, _))) ->
    Some (C.Const (C.CInt64 (Int64.mul i j, ik, None)))
  | None, None -> None
  | _          -> assert false

let rec flattenArray = function
  | C.TArray (C.TArray (c, iszo, _), oszo, _) ->
    flattenArray (C.TArray (c, arraySizes iszo oszo, []))
  | t -> t

(* Breaks w/ zero-sized arrays? *)
let indexOfArrayElements t b = match t, b with
  | t, Some (C.Const (C.CInt64 (i, _, _))) ->
    let sz = CM.bytesSizeOf t in
    let c  = Int64.to_int i - 1 in
      I.mk_sequence 0 sz (Some 0) (Some (c * sz))
  | t, _ -> I.mk_sequence 0 (CM.bytesSizeOf t) (Some 0) None

let indexOfPointerContents t = match t |> C.unrollType |> flattenArray with
  | C.TArray (t, b, _)                         -> indexOfArrayElements t b
  | C.TPtr (t, ats) when CM.has_array_attr ats -> I.mk_sequence 0 (CM.bytesSizeOf t) (Some 0) None
  | C.TPtr _                                   -> I.mk_singleton 0
  | _                                          -> assert false

(******************************************************************************)
(***************** Conversion from CIL Types to Refined Types *****************)
(******************************************************************************)

let ctypeOfCilBaseType mem = function
  | C.TVoid ats          -> Ct.Int (0,                           indexOfAttrs ats)
  | C.TInt (ik,   ats)   -> Ct.Int (C.bytesSizeOfInt ik,         indexOfAttrs ats)
  | C.TFloat (fk, ats)   -> Ct.Int (CM.bytesSizeOfFloat fk,      indexOfAttrs ats)
  | C.TEnum (ei,  ats)   -> Ct.Int (C.bytesSizeOfInt ei.C.ekind, indexOfAttrs ats)
  | C.TArray (t, _, ats) -> Ct.Ref (slocOfAttrs ats,             ptrIndexOfAttrs ats)
  | C.TPtr (t, ats)      ->
    begin match CM.typeName t with
      | Some n when SM.mem n mem -> Ct.Ref (SM.find n mem,   ptrIndexOfAttrs ats)
      | _                        -> Ct.Ref (slocOfAttrs ats, ptrIndexOfAttrs ats)
    end
  | _ -> assertf "ctypeOfCilBaseType: non-base!"

let refctypeOfCilType mem t =
  FI.t_pred (ctypeOfCilBaseType mem t) (A.Symbol.of_string "V") (typePredicate t)

let addReftypeToStore sto loc s i rct =
     rct
  |> RS.Data.add_and_fold_overlap sto loc begin fun _ sto ct1 ct2 ->
       if ct1 = ct2 then ((), sto) else
         E.s <| C.errorLoc loc "Conflicting types for store location %a, index %a: %a, %a"
             S.d_sloc s Ct.Index.d_index i RCt.d_ctype ct1 RCt.d_ctype ct2
     end () s i
  |> snd

let rec componentsOfType t = match t |> C.unrollType |> flattenArray with
  | C.TArray (t, b, _) ->
    t |> componentsOfType |>: M.app_snd3 (I.plus <| indexOfArrayElements t b)
  | C.TComp (ci, _) as t ->
    M.flap
      begin fun f -> match componentsOfField t f with
        | [(_, i, t)] -> [(f.C.fname, i, t)]
        | cs          -> cs
      end ci.C.cfields
  | t -> [("", I.mk_singleton 0, ensureSloc t)]

and componentsOfField t f =
  let off = C.Field (f, C.NoOffset) |> CM.bytesOffset t |> I.mk_singleton in
    f.C.ftype |> componentsOfType |>: (M.app_snd3 <| I.plus off)

let alreadyClosedType mem t = match CM.typeName t with
  | Some n -> SM.mem n mem
  | _      -> false

let rec closeTypeInStoreAux loc mem sto t = match C.unrollType t with
  | C.TPtr (tb, _) when alreadyClosedType mem tb -> sto
  | C.TPtr (tb, ats) | C.TArray (tb, _, ats)     ->
    let s      = slocOfAttrs ats in
    let mem    = match CM.typeName tb with Some n -> SM.add n s mem | _ -> mem in
    let tcs    = tb |> componentsOfType |>: M.app_snd3 (I.plus <| indexOfPointerContents t) in
    let fldsub = List.map (fun (fn, i, _) -> (FA.name_of_string fn, FI.name_of_sloc_index s i)) tcs in
      List.fold_left
        begin fun sto (_, i, t) ->
          let sto = closeTypeInStoreAux loc mem sto t in
               t
            |> refctypeOfCilType mem
            |> FI.t_subs_names fldsub
            |> addReftypeToStore sto loc s i
        end sto tcs
  | _ -> sto

let closeTypeInStore loc sto t =
  closeTypeInStoreAux loc SM.empty sto t

let refstoreOfTypes ts =
  List.fold_left (closeTypeInStore C.locUnknown) RS.empty ts

(* Need to assert WF: no type has both global and local sloc annotation *)
class globalLocCollector = object (self)
  inherit C.nopCilVisitor

  val mutable glocs = []

  method addGlobalLoc s =
    glocs <- s :: glocs

  method vattr atr =
    if C.hasAttribute gslocAttribute [atr] then
      [atr] |> slocOfAttrs |> self#addGlobalLoc;
      C.DoChildren

  method getGlobalLocs =
    M.sort_and_compact glocs
end

let globalLocsOfTypes ts =
  let glc = new globalLocCollector in
    List.iter (C.visitCilType (glc :> C.cilVisitor) <+> ignore) ts;
    glc#getGlobalLocs

let refcfunOfType t =
  let ret, argso, _, _ = C.splitFunctionType t in
  let ret              = ensureSloc ret in
  let argts            = argso |> C.argsToList |>: (argType <+> M.app_fst nameArg) in
  let rootts           = ret :: List.map snd argts in
  let glocs            = globalLocsOfTypes rootts in
  let _, sto           = rootts |> refstoreOfTypes |> RS.partition (M.flip List.mem glocs) in
    some <|
      RCf.make
        (List.map (M.app_snd <| refctypeOfCilType SM.empty) argts)
        glocs
        sto
        (refctypeOfCilType SM.empty ret)
        sto

(******************************************************************************)
(******************************* Gathering Specs ******************************)
(******************************************************************************)

let checkDeclarationWellFormed v =
  if v.C.vstorage = C.Extern && not (C.hasAttribute externOkAttribute v.C.vattr) then
    E.s <| C.errorLoc v.C.vdecl
        "%s is declared extern. Make sure its spec is ok and add the OKEXTERN attribute."
        v.C.vname;
  ()

let declarationsOfFile file =
     VS.empty
  |> C.foldGlobals file begin fun vars -> function
       | C.GVarDecl (v, _) | C.GVar (v, _, _) -> VS.add v vars
       | C.GFun (fd, _)                       -> VS.add fd.C.svar vars
       | _                                    -> vars
     end
  |> VS.elements
  >> List.iter checkDeclarationWellFormed
  |> List.partition (fun v -> C.isFunctionType v.C.vtype)
  |> M.app_snd (List.map (fun v -> {v with C.vtype = ensureSloc v.C.vtype}))

let isBuiltin = Misc.is_prefix "__builtin"

let updFunM spec funm loc fn = function
  | _ when SM.mem fn spec     -> funm
  | _ when isBuiltin fn       -> funm
  | t when C.isFunctionType t -> M.sm_protected_add false fn (refcfunOfType t) funm
  | _                         -> funm 

let funspecsOfFuns funspec funs =
     List.fold_left begin fun funm v ->
      updFunM funspec funm v.C.vdecl v.C.vname v.C.vtype
     end SM.empty funs
  |> Misc.sm_bindings
  |> Misc.map_partial (function (x, Some y) -> Some (x,y) | _ -> None)

let updVarM spec varm loc vn = function
  | _ when SM.mem vn spec           -> varm
  | t when not (C.isFunctionType t) -> M.sm_protected_add false vn (refctypeOfCilType SM.empty t) varm
  | _                               -> varm

let globalSpecsOfVars varspec vars =
     List.fold_left begin fun varm v ->
       updVarM varspec varm v.C.vdecl v.C.vname v.C.vtype
     end SM.empty vars
  |> M.sm_bindings

(* in the end, there should only ever be one file, so maybe we should specialize to that *)
let specsOfFile spec file =
  let funs, vars = declarationsOfFile file in
    (funspecsOfFuns (RSp.funspec spec) funs,
     globalSpecsOfVars (RSp.varspec spec) vars,
     vars |>: (fun {C.vtype = t} -> t) |> refstoreOfTypes)
