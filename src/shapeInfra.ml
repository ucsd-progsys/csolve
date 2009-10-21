module C  = Cil
module CM = CilMisc
module S  = Sloc

open Ctypes
open Misc.Ops

let fresh_heaptype (t: C.typ): ctype =
  match C.unrollType t with
    | C.TInt (ik, _)        -> CTInt (C.bytesSizeOfInt ik, ITop)
    | C.TFloat _            -> CTInt (CM.typ_width t, ITop)
    | C.TVoid _             -> void_ctype
    | C.TPtr _ | C.TArray _ -> CTRef (S.fresh S.Abstract, IInt 0)
    | _                     -> halt <| C.bug "Unimplemented fresh_ctype: %a@!@!" C.d_type t
