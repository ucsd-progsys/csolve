module C  = Cil
module P  = Pretty
module ST = Ssa_transform
module I  = Inferctypes
module T  = Ctypes

open Misc.Ops

(* pmr: This file is just a temporary addition until the main thing is in a state where
   we can use it to infer plain types. *)
let mydebug = true 

let scis_of_file cil = 
  Cil.foldGlobals cil begin
    fun acc g ->
      match g with 
      | Cil.GFun (fdec,loc) -> (ST.fdec_to_ssa_cfg fdec loc) :: acc 
      | _                   -> acc
  end []
  |> (fun scis -> let _ = if mydebug then ST.print_scis scis in scis)
  
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
            mk_cfg cil;
            rename_locals cil in
  cil

let wloc = Ctypes.ALoc (-1)

let wref = Ctypes.CTRef (wloc, Ctypes.IInt 0)

let wstore = Ctypes.SLM.add wloc (Ctypes.LDesc.add (Ctypes.PLAt 4) wref Ctypes.LDesc.empty) Ctypes.SLM.empty

let walk_type: Ctypes.cfun = {Ctypes.qlocs   = [wloc];
                              Ctypes.args    = [("p", wref)];
                              Ctypes.ret     = None;
                              Ctypes.abs_in  = wstore;
                              Ctypes.abs_out = wstore;
                              Ctypes.con_in  = Ctypes.SLM.empty;
                              Ctypes.con_out = Ctypes.SLM.empty}

let mloc = Ctypes.ALoc (-1)

let malloc_type: Ctypes.cfun = {Ctypes.qlocs   = [mloc];
                                Ctypes.args    = [("sz", Ctypes.CTInt (4, Ctypes.ITop))];
                                Ctypes.ret     = Some (Ctypes.CTRef (mloc, Ctypes.IInt 0));
                                Ctypes.abs_in  = Ctypes.SLM.empty;
                                Ctypes.abs_out = Ctypes.SLM.add mloc (Ctypes.LDesc.empty) Ctypes.SLM.empty;
                                Ctypes.con_in  = Ctypes.SLM.empty;
                                Ctypes.con_out = Ctypes.SLM.empty}

let main_type: Ctypes.cfun = {Ctypes.qlocs   = [];
                              Ctypes.args    = [];
                              Ctypes.ret     = None;
                              Ctypes.abs_in  = Ctypes.SLM.empty;
                              Ctypes.abs_out = Ctypes.SLM.empty;
                              Ctypes.con_in  = Ctypes.SLM.empty;
                              Ctypes.con_out = Ctypes.SLM.empty}

let funtys =
  [
    ("main", main_type);
    ("malloc", malloc_type);
    ("walk", walk_type);
  ]

let print_sci_shapes funs scis =
  I.infer_shapes funs scis |> Misc.StringMap.iter begin fun fname (locals, _, st) ->
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

let add_sci map sci =
  let fname = sci.ST.fdec.C.svar.C.vname in
    Misc.StringMap.add fname (List.assoc fname funtys, sci) map

let infer_shapes file =
  let cil  = mk_cil file in
  let scis = scis_of_file cil |> List.fold_left add_sci Misc.StringMap.empty in
  let funs = List.fold_left (fun m (f, ty) -> Misc.StringMap.add f ty m) Misc.StringMap.empty funtys in
    print_sci_shapes funs scis

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
