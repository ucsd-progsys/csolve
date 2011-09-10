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
module I   = Index
module S   = Sloc
module SLM = S.SlocMap
module SC  = ScalarCtypes

module RCt = Ctypes.RefCTypes.CType
module RFl = Ctypes.RefCTypes.Field
module RLD = Ctypes.RefCTypes.LDesc
module RS  = Ctypes.RefCTypes.Store
module RCf = Ctypes.RefCTypes.CFun
module RSp = Ctypes.RefCTypes.Spec
module RU  = RS.Unify

open M.Ops

(******************************************************************************)
(***************************** Array Manipulation *****************************)
(******************************************************************************)

(* Breaks w/ zero-sized arrays? *)
let indexOfArrayElements t b ats = match t, b with
  | t, Some (C.Const (C.CInt64 (i, _, _)))
      when not <| C.hasAttribute CM.ignoreBoundAttribute ats->
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

let vv  = A.Symbol.of_string "V"
let evv = A.eVar vv

let roomForPred tb =
  A.pAtom (A.eBin (FA.eApp_bend evv, A.Minus, evv), A.Ge, A.eInt (CM.bytesSizeOf tb))

let nonnullRoomForPred tb =
  A.pImp (A.pAtom (evv, A.Ne, A.eInt 0), roomForPred tb)

let nonnullPred = A.pAtom (evv, A.Gt, A.eInt 0)

let eqBlockBeginPred = A.pAtom (evv, A.Eq, FA.eApp_bbegin evv)

let roomForPredsOfAttrs ats =
      ats
  |>  List.filter (function C.Attr (an, _) -> an = CM.roomForAttribute || an = CM.nonnullRoomForAttribute)
  |>: function
      | C.Attr (an, [C.ASizeOf tb]) ->
          if an = CM.roomForAttribute then roomForPred tb else nonnullRoomForPred tb
      | _ -> assert false

let rawPredsOfAttrs ats =
      ats
  |>  CM.getStringAttrs CM.predAttribute
  |>: fun predStr ->
        try
          predStr |> Lexing.from_string |> RefParse.pred RefLex.token
        with Parsing.Parse_error ->
          E.s <| C.error "Could not parse predicate: %s@!" predStr

let pointerLayoutAttributes =
  [CM.arrayAttribute;
   CM.predAttribute;
   CM.roomForAttribute;
   CM.nonnullRoomForAttribute;
   CM.ignoreBoundAttribute]

let hasOneAttributeOf of_ats ats =
  List.exists (M.flip C.hasAttribute ats) of_ats

let annotatedPointerBaseType ats tb = match C.filterAttributes CM.layoutAttribute ats with
  | []                          -> tb
  | [C.Attr (_, [C.ASizeOf t])] -> t
  | ats                         -> E.s <| C.error "Bad layout on pointer: %a@!" C.d_attrlist ats

let defaultPredsOfAttrs tbo ats = match tbo with
  | Some tb when not (hasOneAttributeOf pointerLayoutAttributes ats) ->
    let tb = annotatedPointerBaseType ats tb in
      [nonnullRoomForPred tb; nonnullPred; eqBlockBeginPred]
  | _ -> []

let predOfAttrs tbo ats =
  A.pAnd (rawPredsOfAttrs ats ++ roomForPredsOfAttrs ats ++ defaultPredsOfAttrs tbo ats)

let ptrIndexOfPredAttrs tb pred ats =
  let hasArray, hasPred = (CM.has_array_attr ats, C.hasAttribute CM.predAttribute ats) in
  let arrayIndex        = if hasArray then indexOfArrayElements tb None ats else I.top in
  let predIndex         = if hasPred then I.ref_index_of_pred vv pred else I.top in
    if hasArray || hasPred then I.glb arrayIndex predIndex else I.of_int 0

let ptrReftypeOfAttrs tb ats =
  let pred = predOfAttrs (Some tb) ats in
    FI.t_spec_pred (Ct.Ref (slocOfAttrs ats, ptrIndexOfPredAttrs tb pred ats)) vv pred

let intReftypeOfAttrs width ats =
  let pred = predOfAttrs None ats in
    FI.t_spec_pred
      (Ct.Int (width, I.data_index_of_pred vv pred))
      vv
      pred

(******************************************************************************)
(***************************** Type Preprocessing *****************************)
(******************************************************************************)

let freshSlocName, _ = M.mk_string_factory "LOC"

class slocEnsurer = object
  inherit C.nopCilVisitor

  method vtype t =
    if C.isPointerType t || C.isArrayType t then
      let ats = C.typeAttrs t in
        if C.hasAttribute CM.slocAttribute ats then
          C.DoChildren
        else
          C.ChangeDoChildrenPost
            (C.typeAddAttributes [C.Attr (CM.slocAttribute, [C.AStr (freshSlocName ())])] t, id)
    else C.DoChildren
end

let ensureSlocAttrs =
  let se: C.cilVisitor = new slocEnsurer in
    fun t -> C.visitCilType se t

let argType (x, t, ats) =
  (x, t |> C.typeAddAttributes ats |> ensureSlocAttrs)

let nameArg x =
  if x = "" then CM.fresh_arg_name () else x

let indexOfPointerContents t = match t |> C.unrollType |> flattenArray with
  | C.TArray (tb, b, ats)                       -> indexOfArrayElements tb b ats
  | C.TPtr (tb, ats) when CM.has_array_attr ats -> indexOfArrayElements tb None ats
  | C.TPtr _                                    -> I.of_int 0
  | _                                           -> assert false

(******************************************************************************)
(****************** Checking Type Annotation Well-Formedness ******************)
(******************************************************************************)

let assertSlocNotConcrete ats =
  if C.hasAttribute CM.slocAttribute ats then
    assert (ats |> slocNameOfAttrs |> isLocNameAbstract)

let assertStoreTypeWellFormed t =
 t |> C.typeAttrs |> assertSlocNotConcrete

let assertExternDeclarationsValid vs =
     vs
  |> List.filter begin fun v ->
       v.C.vstorage = C.Extern && not (C.hasAttribute CM.externOkAttribute v.C.vattr)
     end
  >> List.iter begin fun v ->
       ignore <| C.errorLoc
         v.C.vdecl
         "%s is declared extern. Make sure its spec is ok and add the OKEXTERN attribute."
         v.C.vname
     end
  |> fun vs -> assert (vs = [])

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
          |> function
             | (Ct.Ref (s, _) as t) -> RCt.subs [s, SM.find n mem] t
             | _                    -> assert false
        end
      | _ -> ptrReftypeOfAttrs t ats
    end
  | _ -> assertf "refctypeOfCilType: non-base!"

let heapRefctypeOfCilType mem t =
     t
  |> refctypeOfCilType mem
  |> function | Ct.Int (n, (_, r)) -> Ct.Int (n, (I.top, r))
              | Ct.Ref _ as rct    -> rct

let addReffieldToStore sub sto s i rfld =
  if rfld |> RFl.type_of |> RCt.width = 0 then (sub, RS.Data.ensure_sloc sto s) else
    rfld |> RU.add_field sto sub s i |> M.swap

class structInstantiator (ats) = object (self)
  inherit C.nopCilVisitor

  val mutable sub =
    M.map_partial begin function
      | C.Attr (n, [C.AStr nfrom; C.AStr nto])
          when n = CM.instantiateAttribute ->
        Some (nfrom, nto)
      | _ -> None
    end ats

  method private ensureSubst s =
    if not <| List.mem_assoc s sub then sub <- (s, freshSlocName ()) :: sub

  method vtype t =
    if C.isPointerType t then
      t |> C.typeAttrs |> slocNameOfAttrs |> self#ensureSubst;
    C.DoChildren

  method vattr = function
    | C.Attr (n, [C.AStr nfrom; C.AStr nto])
        when n = CM.instantiateAttribute ->
      self#ensureSubst nto;
      C.ChangeTo [C.Attr (CM.instantiateAttribute, [C.AStr nfrom; C.AStr (List.assoc nto sub)])]
    | C.Attr (n, [C.AStr sloc])
        when n = CM.slocAttribute ->
      self#ensureSubst sloc;
      C.ChangeTo [C.Attr (CM.slocAttribute, [C.AStr (List.assoc sloc sub)])]
    | _ -> C.DoChildren
end

let instantiateStruct ats tcs =
  let instr = new structInstantiator ats in
    List.map (M.app_thd3 <| C.visitCilType (instr :> C.cilVisitor)) tcs

let rec componentsOfTypeAux = function
  | C.TArray (t, b, ats) ->
    t |> componentsOfType |>: M.app_snd3 (I.plus <| indexOfArrayElements t b ats)
  | C.TComp (ci, ats) as t ->
       ci.C.cfields
    |> M.flap begin fun f -> match componentsOfField t f with
        | [(_, i, t)] -> [(f.C.fname, i, t)]
        | cs          -> cs
       end
    |> instantiateStruct ats
  | t -> [("", I.of_int 0, ensureSlocAttrs t)]

and componentsOfType t =
  let t  = t |> C.unrollType |> flattenArray in
  let cs = componentsOfTypeAux t in
    if t |> C.typeAttrs |> C.hasAttribute CM.finalAttribute then
      List.map (M.app_thd3 <| C.typeAddAttributes [C.Attr (CM.finalAttribute, [])]) cs
    else cs

and componentsOfField t f =
  let off = C.Field (f, C.NoOffset) |> CM.bytesOffset t |> I.of_int in
    f.C.ftype |> componentsOfType |>: (M.app_snd3 <| I.plus off)

let alreadyClosedType mem t = match CM.typeName t with
  | Some n -> SM.mem n mem
  | _      -> false

let rec closeTypeInStoreAux mem sub sto t = match C.unrollType t with
  | C.TPtr (tb, _) when alreadyClosedType mem tb -> (sub, sto)
  | C.TPtr (tb, ats) | C.TArray (tb, _, ats)     ->
    let s      = slocOfAttrs ats in
    let tb     = annotatedPointerBaseType ats tb in
    let mem    = match CM.typeName tb with Some n -> SM.add n s mem | _ -> mem in
    let tcs    = tb |> componentsOfType |>: M.app_snd3 (I.plus <| indexOfPointerContents t) in
    let fldsub = List.map (fun (fn, i, _) -> (FA.name_of_string fn, FI.name_of_sloc_index s i)) tcs in
      List.fold_left
        begin fun (sub, sto) (fn, i, t) ->
          let sub, sto = closeTypeInStoreAux mem sub sto t in
            if C.isFunctionType t then t |> preRefcfunOfType |> RU.add_fun sto sub s |> M.swap else
                 t
              >> assertStoreTypeWellFormed
              |> heapRefctypeOfCilType mem
              |> FI.t_subs_names fldsub
              |> RFl.create (t |> C.typeAttrs |> finalityOfAttrs) {Ct.fname = Some fn; Ct.ftype = None} (* Some t, but doesn't parse *)
              |> addReffieldToStore sub sto s i
        end (sub, sto) tcs
  | _ -> (sub, sto)

and closeTypeInStore sub sto t =
  closeTypeInStoreAux SM.empty sub sto t

and preRefstoreOfTypes ts =
  List.fold_left (M.uncurry closeTypeInStore) (S.Subst.empty, RS.empty) ts

(* Converts function variable v to a refcfun, but the store includes
   contents for global locations. This is fixed by
   refcfunOfPreRefcfun. *)
and preRefcfunOfType t =
  let ret, argso, _, ats = t |> C.unrollType |> C.splitFunctionType in
  let ret                = ensureSlocAttrs ret in
  let argts              = argso |> C.argsToList |>: (argType <+> M.app_fst nameArg) in
  let sub, allOutStore   = ret :: List.map snd argts |> preRefstoreOfTypes in
  let argrcts            = argts |>: (M.app_snd <| (refctypeOfCilType SM.empty <+> RCt.subs sub)) in
  let retrct             = ret |> refctypeOfCilType SM.empty |> RCt.subs sub in
  let allInStore         = RS.restrict allOutStore (M.map_partial (snd <+> RCt.sloc) argrcts) in
  let glocs              = ats |> CM.getStringAttrs CM.globalAttribute |>: getSloc |> M.flap (RS.reachable allOutStore) in
    RCf.make argrcts glocs allInStore retrct allOutStore

let updateGlobalStore sub gsto gstoUpd =
     (sub, List.fold_right (M.flip RS.Data.ensure_sloc) (RS.Data.domain gstoUpd) gsto)
  |> M.flip (RS.Data.fold_fields
               (fun (sub, sto) s i f -> addReffieldToStore sub sto s i f))
      gstoUpd
  |> M.swap
  |> M.flip (RS.Function.fold_locs
               (fun s rcf (sto, sub) -> RU.add_fun sto sub s rcf))
      gstoUpd

let rec refcfunOfPreRefcfun sub gsto prcf =
  let gsto, aostof, sub = refstoreOfPreRefstore sub gsto prcf.Ct.sto_out in
  let gstof, ostof      = RS.partition (M.flip List.mem prcf.Ct.globlocs) aostof in
  let istof, _          = RS.partition (RS.mem prcf.Ct.sto_in) ostof in
  let gsto, sub         = updateGlobalStore sub gsto gstof in
  let globs             = prcf.Ct.globlocs |>: S.Subst.apply sub |> M.sort_and_compact in
    (RCf.subs {prcf with Ct.globlocs = globs; Ct.sto_in = istof; Ct.sto_out = ostof} sub, gsto, sub)

and refstoreOfPreRefstore sub gsto sto =
     sto
  |> RS.Function.fold_locs begin fun s prcf (gsto, (sto, sub)) ->
       let rcf, gsto, sub = refcfunOfPreRefcfun sub gsto prcf in
         (gsto, RU.add_fun sto sub s rcf)
     end (gsto, (RS.data sto, S.Subst.empty))
  |> fun (gsto, (sto, sub)) -> (gsto, sto, sub)

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
  |> List.partition (fun v -> C.isFunctionType v.C.vtype)
  |> M.app_snd (List.map (fun v -> {v with C.vtype = ensureSlocAttrs v.C.vtype}))

let isBuiltin = Misc.is_prefix "__builtin"

let specTypeOfFun v =
  if C.hasAttribute CM.checkTypeAttribute v.C.vattr then Ct.IsSubtype else Ct.HasShape

let globalSpecOfFuns sub gsto funs =
     funs
  |> List.fold_left begin fun (funm, gsto, sub) v ->
     let _      = C.currentLoc := v.C.vdecl in
     let fn, ty = (v.C.vname, C.typeAddAttributes v.C.vattr v.C.vtype) in
       if C.isFunctionType ty && not (isBuiltin fn) then
         let rcf, gsto, sub = ty |> preRefcfunOfType |> refcfunOfPreRefcfun sub gsto in
           (M.sm_protected_add false fn (rcf, specTypeOfFun v) funm, gsto, sub)
       else (funm, gsto, sub)
     end (SM.empty, gsto, sub)
  |> M.app_fst3 SM.to_list

let specTypeOfVar v = match v.C.vstorage with
  | C.Extern -> Ct.HasType
  | _        ->
    if C.hasAttribute CM.checkTypeAttribute v.C.vattr then Ct.IsSubtype else Ct.HasShape

let updVarM sub varm v =
  if not <| C.isFunctionType v.C.vtype then begin
    let _ = C.currentLoc := v.C.vdecl in
      M.sm_protected_add
        false
        v.C.vname
        (v.C.vtype |> refctypeOfCilType SM.empty |> RCt.subs sub, specTypeOfVar v)
        varm
  end else
    varm

let varSpecOfVars sub vars =
     vars
  |> List.fold_left (fun varm v -> updVarM sub varm v) SM.empty
  |> SM.to_list

let storeTypeSpecs varSpec gsto =
     gsto
  |> RS.domain
  |> List.fold_left (fun sts l -> SLM.add l Ct.HasShape sts) SLM.empty
  |> List.fold_right begin fun (_, (t, vst)) sts ->
           t
        |> RCt.sloc
        |> M.maybe
        |> RS.reachable gsto
        |> List.fold_left begin fun sts l ->
             SLM.add l (Ct.specTypeMax vst (SLM.find l sts)) sts
           end sts
     end varSpec

let specsOfDecs funs vars =
  let sub, pgsto        = vars |>: (fun {C.vtype = t} -> t) |> preRefstoreOfTypes in
  let gsto, _, sub      = pgsto |> refstoreOfPreRefstore sub pgsto in
  let fspecs, gsto, sub = globalSpecOfFuns sub gsto funs in
  let vspecs            = varSpecOfVars sub vars in
    (fspecs, vspecs, gsto, storeTypeSpecs vspecs gsto)

let writeSpec (funspec, varspec, storespec, sts) outfilename =
  let oc = open_out outfilename in
    Ctypes.RefCTypes.Store.Data.fold_locs begin fun l ld _ ->
      Pretty.fprintf oc "loc %a %a %a\n\n"
        S.d_sloc l Ct.d_specTypeRel (SLM.find l sts) RLD.d_ldesc ld |> ignore
    end () storespec;
    Ctypes.RefCTypes.Store.Function.fold_locs begin fun l cf _ ->
      Pretty.fprintf oc "loc %a %a@!  @[%a@]@!@!"
        S.d_sloc l Ct.d_specTypeRel (SLM.find l sts) RCf.d_cfun cf |> ignore
    end () storespec;
    List.iter begin fun (vn, (ct, spt)) ->
      Pretty.fprintf oc "%s %a @[%a@]\n\n" vn Ctypes.d_specTypeRel spt Ctypes.RefCTypes.CType.d_ctype ct |> ignore
    end varspec;
    List.iter begin fun (fn, (cf, spt)) ->
      Pretty.fprintf oc "%s %a@!  @[%a@]\n\n" fn Ctypes.d_specTypeRel spt Ctypes.RefCTypes.CFun.d_cfun cf |> ignore
    end funspec;
    close_out oc

let writeSpecOfFile file outfilename =
  let _          = E.log "START: Generating Specs \n" in
  let funs, vars = declarationsOfFile file in
  let spec       = specsOfDecs funs vars in
  let _          = writeSpec spec outfilename in
  let _          = assertExternDeclarationsValid (funs ++ vars) in
    ignore <| E.log "DONE: Generating Specs \n"
