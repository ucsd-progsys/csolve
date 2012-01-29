module BS  = BNstats
module SM  = Ast.Symbol.SMap
module Co  = Constants 
module C   = FixConstraint
module F   = Format
module T   = Toplevel
module ID  = IndexDomain
module SID = Solve.Make (ID)

module Misc = FixMisc open Misc.Ops

(*****************************************************************)
(********************* Hooking into Solver ***********************)
(*****************************************************************)

let print_raw_cs ppf = function
  | [] -> F.fprintf ppf "SAT \n \n \n"
  | cs -> F.fprintf ppf "UNSAT [%s] \n \n \n" (Misc.map_to_string (C.id_of_t <+> string_of_int) cs)

let save_raw fname cs s = 
  let oc  = open_out fname in
  let ppf = F.formatter_of_out_channel oc in
  let _   = print_now ("Fixpoint: save_raw into file = " ^ fname ^ " : BEGIN \n") in
  F.fprintf ppf "%a \n" print_raw_cs cs; 
  F.fprintf ppf "%a \n" ID.print s;
  F.fprintf ppf "@.";
  F.print_flush ();
  close_out oc;
  print_now "Fixpoint: save_raw: END \n"

let solve ac  =
  Misc.with_ref_at Constants.slice false begin fun () ->
    let _       = print_now "Fixpoint: Creating  CI\n" in
    let ctx, s  = BS.time "create" SID.create ac in
    let _       = print_now "Fixpoint: Solving \n" in
    let s, cs'  = BS.time "solve" (SID.solve ctx) s in
    let _       = print_now "Fixpoint: Saving Result \n" in
    let _       = BS.time "save" (save_raw !Co.out_file cs') s in
    let _       = print_now "Fixpoint: Saving Result DONE \n" in
      cs'
  end

let dump_solve ac = 
  let cs' = solve { ac with FixConfig.bm = SM.map ID.mkbind ac.FixConfig.bm } in
  let _   = BNstats.print stdout "Fixpoint Solver Time \n" in
  match cs' with 
  | [] -> (F.printf "\nSAT\n" ; exit 0)
  | _  -> (F.printf "\nUNSAT\n" ; exit 1)

(*****************************************************************)
(*********************** Main ************************************)
(*****************************************************************)

let usage = "Usage: indexSolver.native <options> [source-files]\noptions are:"

let main () =
  let cs  = usage |> Toplevel.read_inputs |> snd in 
    dump_solve cs 

let _ = main ()
