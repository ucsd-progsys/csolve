module CM = CilMisc
module FP = CM.FunPtrDetector
module F  = Frontc

open Cil
open Misc.Ops
open Printf

let argc  = Array.length Sys.argv

let main () =
  let _          =
    asserts (argc > 1) "Usage: %s fname" Sys.argv.(0) in
  let fname      = Sys.argv.(1) in
  let cil        = F.parse fname () in
  let st         = CM.FunPtrDetector.build_summary cil in
    if st.FP.has_prop then
      exit (st.FP.metric + 1)
    else exit 0

let _ =
  main ()
