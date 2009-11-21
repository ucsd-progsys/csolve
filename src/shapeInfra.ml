module C  = Cil
module CM = CilMisc
module S  = Sloc

open Ctypes
open Misc.Ops

let rec typealias_attrs: C.typ -> C.attributes = function
  | C.TNamed (ti, a) -> a @ typealias_attrs ti.C.ttype
  | _                -> []

let fresh_heaptype (t: C.typ): ctype =
  let ats1 = typealias_attrs t in
    match C.unrollType t with
      | C.TInt (ik, _)                           -> CTInt (C.bytesSizeOfInt ik, index_top)
      | C.TEnum (ei, _)                          -> CTInt (C.bytesSizeOfInt ei.C.ekind, index_top)
      | C.TFloat _                               -> CTInt (CM.typ_width t, index_top)
      | C.TVoid _                                -> void_ctype
      | C.TPtr (t, ats2) | C.TArray (t, _, ats2) -> CTRef (S.fresh S.Abstract, if CM.has_array_attr (ats1 @ ats2) then ISeq (0, CM.typ_width t, Pos) else IInt 0)
      | _                                        -> halt <| C.bug "Unimplemented fresh_heaptype: %a@!@!" C.d_type t
