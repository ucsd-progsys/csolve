module C   = Cil
module M   = Misc
module CM  = CilMisc
module A   = Ast
module FI  = FixInterface
module Ct  = Ctypes
module RCt = Ctypes.RefCTypes.CType
module RCS = Ctypes.RefCTypes.Store
module RCf = Ctypes.RefCTypes.CFun

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
  | C.Attr ("lcc_predicate", [C.AStr p]) -> assert false
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
    RCf.make
      (argso |> M.resl_opt id |> List.map refargOfCilArg)
      []
      RCS.empty
      (ret |> C.typeSig |> refctypeOfCilTypeSig)
      RCS.empty
