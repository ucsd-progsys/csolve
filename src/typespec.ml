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
module SC  = ScalarCtypes

module RCt = Ctypes.RefCTypes.CType
module RFl = Ctypes.RefCTypes.Field
module RLD = Ctypes.RefCTypes.LDesc
module RS  = Ctypes.RefCTypes.Store
module RCf = Ctypes.RefCTypes.CFun
module RSp = Ctypes.RefCTypes.Spec

open M.Ops

(******************************************************************************)
(***************************** Array Manipulation *****************************)
(******************************************************************************)

(* Breaks w/ zero-sized arrays? *)
let indexOfArrayElements t b = match t, b with
  | t, Some (C.Const (C.CInt64 (i, _, _))) ->
    let sz = CM.bytesSizeOf t in
    let c  = Int64.to_int i - 1 in
      I.mk_sequence 0 sz (Some 0) (Some (c * sz))
  | t, _ -> I.mk_sequence 0 (CM.bytesSizeOf t) (Some 0) None


let arraySizes sz1 sz2 = match sz1, sz2 with
  | Some (C.Const (C.CInt64 (i, ik, _))), Some (C.Const (C.CInt64 (j, _, _))) ->
    Some (C.Const (C.CInt64 (Int64.mul i j, ik, None)))
  | None, None -> None
  | _          -> assert false

let rec flattenArray = function
  | C.TArray (C.TArray (c, iszo, _), oszo, _) ->
    flattenArray (C.TArray (c, arraySizes iszo oszo, []))
  | t -> t

(******************************************************************************)
(************************* Annotations From Attributes ************************)
(******************************************************************************)

let concretePrefix = '!'

let isLocNameAbstract s =
  not (String.get s 0 = concretePrefix)

let abstractLocNameOfStr s =
  if isLocNameAbstract s then s else String.sub s 1 (String.length s - 1)

let rec getSloc =
  let slocTable = Hashtbl.create 17 in
    fun s ->
      if isLocNameAbstract s then
        M.do_memo slocTable S.fresh_abstract [] s
      else
        M.do_memo slocTable (abstractLocNameOfStr <+> getSloc <+> S.fresh_concrete) s s

let slocNameOfAttrs ats =
     ats
  |> CM.getStringAttrs CM.slocAttribute
  |> M.ex_one "Type does not have a single sloc"

let slocOfAttrs ats =
     ats
  |> slocNameOfAttrs
  |> getSloc

let finalityOfAttrs ats =
  if C.hasAttribute CM.finalAttribute ats then Ct.Final else Ct.Nonfinal

let predOfAttrs ats =
      ats
  |>  CM.getStringAttrs CM.predAttribute
  |>: (Lexing.from_string <+> RefParse.pred RefLex.token)
  |>  A.pAnd

let vv = A.Symbol.of_string "V"

let ptrIndexOfPredAttrs tb pred ats =
  if CM.has_array_attr ats then
    indexOfArrayElements tb None
  else if C.hasAttribute CM.predAttribute ats then
    SC.ref_index_of_pred vv pred
  else
    I.mk_singleton 0

let ptrReftypeOfAttrs tb ats =
  let pred = predOfAttrs ats in
    FI.t_pred (Ct.Ref (slocOfAttrs ats, ptrIndexOfPredAttrs tb pred ats)) vv pred

let intReftypeOfAttrs width ats =
  let pred = predOfAttrs ats in
    FI.t_pred
      (Ct.Int (width, SC.data_index_of_pred vv pred))
      vv
      pred

(******************************************************************************)
(***************************** Type Preprocessing *****************************)
(******************************************************************************)

let freshSlocName, _ = M.mk_string_factory "LOC"

let ensureSlocAttr t =
  if C.isPointerType t || C.isArrayType t then
    let ats = C.typeAttrs t in
      if C.hasAttribute CM.slocAttribute ats then
        t
      else
        C.typeAddAttributes [C.Attr (CM.slocAttribute, [C.AStr (freshSlocName ())])] t
  else t

let argType (x, t, ats) =
  (x, t |> C.typeAddAttributes ats |> ensureSlocAttr)

let nameArg =
  let freshArgName, _ = M.mk_string_factory "ARG" in
    fun x -> if x = "" then freshArgName () else x

let indexOfPointerContents t = match t |> C.unrollType |> flattenArray with
  | C.TArray (t, b, _)                         -> indexOfArrayElements t b
  | C.TPtr (t, ats) when CM.has_array_attr ats -> I.mk_sequence 0 (CM.bytesSizeOf t) (Some 0) None
  | C.TPtr _                                   -> I.mk_singleton 0
  | _                                          -> assert false

(******************************************************************************)
(****************** Checking Type Annotation Well-Formedness ******************)
(******************************************************************************)

let assertSlocNotConcrete ats =
  if C.hasAttribute CM.slocAttribute ats then
    assert (ats |> slocNameOfAttrs |> isLocNameAbstract)

let assertStoreTypeWellFormed t =
 t |> C.typeAttrs |> assertSlocNotConcrete

let assertGlobalVarTypeWellFormed v =
  v.C.vtype |> C.typeAttrs |> assertSlocNotConcrete

let checkDeclarationWellFormed v =
  if v.C.vstorage = C.Extern && not (C.hasAttribute CM.externOkAttribute v.C.vattr) then
    E.s <| C.errorLoc v.C.vdecl
        "%s is declared extern. Make sure its spec is ok and add the OKEXTERN attribute."
        v.C.vname

(******************************************************************************)
(***************** Conversion from CIL Types to Refined Types *****************)
(******************************************************************************)

let refctypeOfCilType mem t = match C.unrollType t with
  | C.TVoid ats          -> intReftypeOfAttrs 0 ats
  | C.TInt (ik,   ats)   -> intReftypeOfAttrs (C.bytesSizeOfInt ik) ats
  | C.TFloat (fk, ats)   -> intReftypeOfAttrs (CM.bytesSizeOfFloat fk) ats
  | C.TEnum (ei,  ats)   -> intReftypeOfAttrs (C.bytesSizeOfInt ei.C.ekind) ats
  | C.TArray (t, _, ats) -> ptrReftypeOfAttrs t ats
  | C.TPtr (t, ats)      ->
    begin match CM.typeName t with
      | Some n when SM.mem n mem -> begin
             ats
          |> ptrReftypeOfAttrs t
          |> function (Ct.Ref (s, _) as t) -> FI.t_subs_locs [s, SM.find n mem] t
        end
      | _ -> ptrReftypeOfAttrs t ats
    end
  | _ -> assertf "refctypeOfCilType: non-base!"

let addReffieldToStore sto loc s i rfld =
  if rfld |> RFl.type_of |> RCt.width = 0 then RS.Data.ensure_sloc sto s else
       rfld
    |> RS.Data.add_field_fold_overlap sto loc begin fun _ sto fld1 fld2 ->
         if fld1 = fld2 then ((), sto) else
           E.s <| C.errorLoc loc "Conflicting fields for store location %a, index %a: %a, %a"
               S.d_sloc s Ct.Index.d_index i RFl.d_field fld1 RFl.d_field fld2
       end () s i
    |> snd

let addRefcfunToStore sto loc s rcf =
  if RS.Function.mem sto s then
    let storcf = RS.Function.find sto s in
      if RCf.same_shape rcf storcf then sto else
        E.s <| C.errorLoc loc "Conflicting function types for location %a:@!%a@!%a"
                 S.d_sloc s RCf.d_cfun storcf RCf.d_cfun rcf
  else RS.Function.add sto s rcf

let instantiateTypeLocation sub t =
   if C.isPointerType t then
     let ats = C.typeAttrs t in
          ats
       |> CM.setStringAttr CM.slocAttribute (List.assoc (slocNameOfAttrs ats) sub)
       |> C.setTypeAttrs t
   else t

let instantiateStruct ats tcs =
     ats
  |> C.filterAttributes CM.instantiateAttribute
  |> List.map begin function
      | C.Attr (n, [C.AStr nfrom; C.AStr nto]) -> (nfrom, nto)
      | _                                      -> assert false (* pmr: better fail message *)
     end
  |> List.fold_right begin fun (_, _, t) sub ->
       if C.isPointerType t then
         let s = t |> C.typeAttrs |> slocNameOfAttrs in
           if List.mem_assoc s sub then sub else (s, freshSlocName ()) :: sub
       else sub
     end tcs
  |> fun sub -> List.map (M.app_thd3 <| instantiateTypeLocation sub) tcs

let rec componentsOfType t = match t |> C.unrollType |> flattenArray with
  | C.TArray (t, b, _) ->
    t |> componentsOfType |>: M.app_snd3 (I.plus <| indexOfArrayElements t b)
  | C.TComp (ci, ats) as t ->
       ci.C.cfields
    |> M.flap begin fun f -> match componentsOfField t f with
        | [(_, i, t)] -> [(f.C.fname, i, t)]
        | cs          -> cs
       end
    |> instantiateStruct ats
  | t -> [("", I.mk_singleton 0, ensureSlocAttr t)]

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
        begin fun sto (fn, i, t) ->
          let sto = closeTypeInStoreAux loc mem sto t in
            if C.isFunctionType t then t |> preRefcfunOfType |> addRefcfunToStore sto loc s else
                 t
              >> assertStoreTypeWellFormed
              |> refctypeOfCilType mem
              |> FI.t_subs_names fldsub
              |> RFl.create (t |> C.typeAttrs |> finalityOfAttrs) {Ct.fname = Some fn; Ct.ftype = None} (* Some t, but doesn't parse *)
              |> addReffieldToStore sto loc s i
        end sto tcs
  | _ -> sto

and closeTypeInStore loc sto t =
  closeTypeInStoreAux loc SM.empty sto t

and preRefstoreOfTypes ts =
  List.fold_left (closeTypeInStore C.locUnknown) RS.empty ts

(* Converts function variable v to a refcfun, but the store includes
   contents for global locations. This is fixed by
   refcfunOfPreRefcfun. *)
and preRefcfunOfType t =
  let ret, argso, _, ats = t |> C.unrollType |> C.splitFunctionType in
  let ret                = ensureSlocAttr ret in
  let argts              = argso |> C.argsToList |>: (argType <+> M.app_fst nameArg) in
  let glocs              = ats |> CM.getStringAttrs CM.globalAttribute |>: getSloc in
  let allOutStore        = ret :: List.map snd argts |> preRefstoreOfTypes in
  let argrcts            = List.map (M.app_snd <| refctypeOfCilType SM.empty) argts in
  let retrct             = refctypeOfCilType SM.empty ret in
  let allInStore         = RS.restrict allOutStore (M.map_partial (snd <+> RCt.sloc) argrcts) in
    RCf.make argrcts glocs allInStore retrct allOutStore

let updateGlobalStore gsto gstoUpd =
     gsto
  |> List.fold_right (M.flip RS.Data.ensure_sloc) (RS.Data.domain gstoUpd)
  |> M.flip (RS.Data.fold_fields
               (fun sto s i f -> addReffieldToStore sto C.locUnknown s i f))
      gstoUpd
  |> M.flip (RS.Function.fold_locs
               (fun s rcf sto -> addRefcfunToStore sto C.locUnknown s rcf))
      gstoUpd

let rec refcfunOfPreRefcfun gsto prcf =
  let gsto, aostof = refstoreOfPreRefstore gsto prcf.Ct.sto_out in
  let gstof, ostof = RS.partition (M.flip List.mem prcf.Ct.globlocs) aostof in
  let istof, _     = RS.partition (RS.mem prcf.Ct.sto_in) ostof in
    ({prcf with Ct.sto_in = istof; Ct.sto_out = ostof}, updateGlobalStore gsto gstof)

and refstoreOfPreRefstore gsto sto =
  RS.Function.fold_locs begin fun s prcf (gsto, sto) ->
    let rcf, gsto = refcfunOfPreRefcfun gsto prcf in
      (gsto, addRefcfunToStore sto C.locUnknown s rcf)
  end (gsto, RS.data sto) sto

(******************************************************************************)
(******************************* Gathering Specs ******************************)
(******************************************************************************)

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
  |> M.app_snd (List.map (fun v -> {v with C.vtype = ensureSlocAttr v.C.vtype}))

let isBuiltin = Misc.is_prefix "__builtin"

let globalSpecOfFuns funspec gsto funs =
     funs
  |> List.fold_left begin fun (funm, gsto) v ->
     let fn, ty = (v.C.vname, C.typeAddAttributes v.C.vattr v.C.vtype) in
       if C.isFunctionType ty && not (SM.mem fn funspec || isBuiltin fn) then
         let rcf, gsto = ty |> preRefcfunOfType |> refcfunOfPreRefcfun gsto in
           (M.sm_protected_add false fn (rcf, C.hasAttribute CM.checkTypeAttribute v.C.vattr) funm, gsto)
       else (funm, gsto)
     end (SM.empty, gsto)
  |> M.app_fst SM.to_list

let updVarM spec varm v =
  if not (SM.mem v.C.vname spec || C.isFunctionType v.C.vtype) then begin
    assertGlobalVarTypeWellFormed v;
    M.sm_protected_add false v.C.vname (refctypeOfCilType SM.empty v.C.vtype) varm
  end else
    varm

let varSpecOfVars varspec vars =
     vars
  |> List.fold_left (fun varm v -> updVarM varspec varm v) SM.empty
  |> SM.to_list

(* in the end, there should only ever be one file, so maybe we should specialize to that *)
let specsOfFile spec file =
  let funs, vars   = declarationsOfFile file in
  let pgsto        = vars |>: (fun {C.vtype = t} -> t) |> preRefstoreOfTypes in
  let gsto         = pgsto |> refstoreOfPreRefstore pgsto |> fst in
  let fspecs, gsto = globalSpecOfFuns (RSp.funspec spec) gsto funs in
    (fspecs, varSpecOfVars (RSp.varspec spec) vars, gsto)
