module C  = Cil
module P  = Pretty
module ST = Ssa_transform
module I  = Inferctypes

open Misc.Ops

(* pmr: This file is just a temporary addition until the main thing is in a state where
   we can use it to infer plain types. *)
let mydebug = true 

let scis_of_file cil = 
  Cil.foldGlobals cil
    (fun acc g ->
      match g with 
      | Cil.GFun (fdec,loc) -> 
          let sci = ST.fdec_to_ssa_cfg fdec loc in
          let _   = if mydebug then ST.print_sci sci in
          sci::acc
      | _ -> acc) [] 

let rename_locals cil =
  Cil.iterGlobals cil
  (function Cil.GFun(fd,_) -> 
    let fn   = fd.Cil.svar.Cil.vname in
    let locs = List.map (fun v -> (v.Cil.vname <- (v.Cil.vname^"@"^fn));v) fd.Cil.slocals in
    let fmls = List.map (fun v -> (v.Cil.vname <- (v.Cil.vname^"@"^fn));v) fd.Cil.sformals in
    fd.Cil.slocals <- locs ;
    fd.Cil.sformals <- fmls
  | _ -> ())

let mk_cfg cil =
  let _ =
    Cil.foldGlobals cil
      (fun fds -> function
         | (Cil.GFun(fd,_) as fundec) ->
             let _ = fundec in
               Psimplify.doGlobal fundec;
               Inliner.doGlobal fds fundec;
               Cil.prepareCFG fd;
               Cil.computeCFGInfo fd false;
               (fd.Cil.svar.Cil.vid, fd) :: fds
         | _ -> fds) []
  in ()

let mk_cil fname =
  let cil = Frontc.parse fname () |> Simplemem.simplemem in
  let _   = Heapify.heapifyNonArrays := true;
            Heapify.default_heapify cil;
            Rmtmps.removeUnusedTemps cil;
            mk_cfg cil;
            rename_locals cil in
  cil

let print_sci_shapes sci =
  let fname = sci.ST.fdec.C.svar.C.vname in
    try
      let (ctem, st) = I.infer_sci_shapes sci in
      let _ = P.printf "%s@!" fname in
      let _ = P.printf "============@!@!" in
      let _ = P.printf "Locals:@!" in
      let _ = P.printf "-------@!@!" in
      let _ = P.printf "%a@!@!" I.d_ctemap ctem in
      let _ = P.printf "Store:@!" in
      let _ = P.printf "------@!@!" in
      let _ = P.printf "%a@!@!" Ctypes.d_store st in
        ()
    with _ ->
      ignore (P.printf "Couldn't get shapes for %s, continuing with next@!@!" fname)

let infer_shapes file =
  let cil  = mk_cil file in
  let scis = scis_of_file cil in
     scis
      |> List.rev
      |> List.iter print_sci_shapes

let mk_options () =
  let fs = ref [] in
  let us = "Usage: sinfer <options> [source-file] \n options are:" in
  let _  = Arg.parse Constants.arg_spec (fun s -> fs := s::!fs) us in
  match !fs with
  | [fn] -> fn
  | []   -> assertf "Bug: No input file specified!"
  | _    -> assertf "Bug: More than one input file specified!"

let main () = 
  infer_shapes <| mk_options ()

let _ = main ()
