module C  = Cil
module P  = Pretty
module ST = Ssa_transform
module I  = Inferctypes
module T  = Ctypes
module E  = Errormsg

open Misc.Ops

(* pmr: This file is just a temporary addition until the main thing is in a state where
   we can use it to infer plain types. *)
let mydebug = true 
 
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
  Cil.iterGlobals cil
    (function
       | Cil.GFun(fd,_) ->
           Cil.prepareCFG fd;
           Cil.computeCFGInfo fd false
       | _ -> ())

let mk_cil fname =
  let cil = Frontc.parse fname () |> Simplemem.simplemem in
  let _   = Pheapify.heapifyNonArrays := true;
            Pheapify.default_heapify cil;
            Rmtmps.removeUnusedTemps cil;
            Psimplify.simplify cil;
            CilMisc.purify cil;
            mk_cfg cil;
            rename_locals cil in
  cil

let print_sci_shapes spec scis =
  I.infer_shapes spec scis |> Misc.StringMap.iter begin fun fname {I.vtyps = locals; I.store = st} ->
    let _ = P.printf "%s@!" fname in
    let _ = P.printf "============@!@!" in
    let _ = P.printf "Locals:@!" in
    let _ = P.printf "-------@!@!" in
    let _ = P.printf "%a@!@!" I.d_vartypes locals in
    let _ = P.printf "Store:@!" in
    let _ = P.printf "------@!@!" in
    let _ = P.printf "%a@!@!" Ctypes.d_store st in
      ()
  end

let add_sci spec map sci =
  let fname = sci.ST.fdec.C.svar.C.vname in
    try
      Misc.StringMap.add fname (Misc.StringMap.find fname spec, sci) map
    with Not_found ->
      E.s <| E.error "Couldn't find spec for function %s@!@!" fname

let infer_shapes file =
  let cil  = mk_cil file in
  let spec = Specparse.read_spec ["lib.spec"; file ^ ".spec"] |> Specparse.cfun_spec_of_spec in
  let scis = ST.scis_of_file cil |> List.fold_left (add_sci spec) Misc.StringMap.empty in
    print_sci_shapes spec scis

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
