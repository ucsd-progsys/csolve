module C   = Cil
module M   = Misc
module CM  = CilMisc
module SM  = M.StringMap
module A   = Ast
module FI  = FixInterface
module Ct  = Ctypes
module RCt = Ctypes.RefCTypes.CType
module RCS = Ctypes.RefCTypes.Store
module RCf = Ctypes.RefCTypes.CFun
module RSp = Ctypes.RefCTypes.Spec

open M.Ops

let indexOfAttrs ats = 
  if CM.has_pos_attr ats then Ct.Index.nonneg else Ct.Index.top

let ctypeOfCilBaseTypeSig = function
  | C.TSBase t ->
      begin match t with
        | C.TVoid ats        -> Ct.Int (0,                           indexOfAttrs ats)
        | C.TInt (ik,   ats) -> Ct.Int (C.bytesSizeOfInt ik,         indexOfAttrs ats)
        | C.TFloat (fk, ats) -> Ct.Int (CM.bytesSizeOfFloat fk,      indexOfAttrs ats)
        | C.TEnum (ei,  ats) -> Ct.Int (C.bytesSizeOfInt ei.C.ekind, indexOfAttrs ats)
        | _                  -> assertf "ctypeOfCilBaseTypeSig: non-base!"
      end
  | _ -> assertf "ctypeOfCilBaseTypeSig: non-base!"

let predicateOfAttr = function
  | C.Attr ("lcc_predicate", [C.AStr p]) -> p |> Lexing.from_string |> RefParse.pred RefLex.token |> some
  | _                                    -> None

let typeSigPredicate ts =
  ts |> C.typeSigAttrs |> Misc.map_partial predicateOfAttr |> A.pAnd

let refctypeOfCilTypeSig ts =
  FI.t_pred (ctypeOfCilBaseTypeSig ts) (A.Symbol.of_string "V") (typeSigPredicate ts)

let refargOfCilArg (x, t, atts) =
  let ts = t |> C.typeSig in
    (x, ts |> C.setTypeSigAttrs (C.typeSigAttrs ts ++ atts) |> refctypeOfCilTypeSig)

let refcfunOfType t =
  let ret, argso, _, _ = C.splitFunctionType t in
    some <|
      RCf.make
        (argso |> M.resl_opt id |> List.map refargOfCilArg)
        []
        RCS.empty
        (ret |> C.typeSig |> refctypeOfCilTypeSig)
        RCS.empty

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
    (fn, [], RCS.empty)
