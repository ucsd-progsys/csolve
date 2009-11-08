module C  = Cil
module CM = CilMisc
module S  = Sloc

open Ctypes
open Misc.Ops

let rec typealias_attrs: C.typ -> C.attributes = function
  | C.TNamed (ti, a) -> a @ typealias_attrs ti.C.ttype
  | _                -> []

let ptrkind_of_ciltyp (t: C.typ): ptrkind =
  t |> C.typeSig |> function
    | C.TSPtr (_, attrs) -> if CM.has_unchecked_attr attrs then Unchecked else Checked
    | _                  -> halt <| C.error "Can't get ptrkind for type %a\n" C.d_type t

let fresh_heaptype (t: C.typ): ctype =
  let ats1 = typealias_attrs t in
    match C.unrollType t with
      | C.TInt (ik, _)                           -> CTInt (C.bytesSizeOfInt ik, ITop)
      | C.TEnum (ei, _)                          -> CTInt (C.bytesSizeOfInt ei.C.ekind, ITop)
      | C.TFloat _                               -> CTInt (CM.typ_width t, ITop)
      | C.TVoid _                                -> void_ctype
      | C.TPtr (t, ats2) | C.TArray (t, _, ats2) -> CTRef (S.fresh S.Abstract, Checked, if CM.has_array_attr (ats1 @ ats2) then ISeq (0, CM.typ_width t) else IInt 0)
      | _                                        -> halt <| C.bug "Unimplemented fresh_heaptype: %a@!@!" C.d_type t
