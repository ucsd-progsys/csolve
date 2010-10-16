module CM = CilMisc
module FP = CM.FunPtrDetector
module F  = Frontc

open Cil
open Misc.Ops
open Printf

let argc  = Array.length Sys.argv

let main () =
  let _          =
    asserts (argc = 2) "Usage: %s fname" Sys.argv.(0) in
  let fname      = Sys.argv.(1) in
  let cil        = F.parse fname () in
  let st         = CM.FunPtrDetector.build_summary cil in
  let _          =
    if st.FP.has_prop then
      printf "%s uses function pointers: %d\n" fname st.FP.metric in
  let rv         =
    if st.FP.has_prop then 1 else 0 in
  exit rv

let _ =
  main ()
