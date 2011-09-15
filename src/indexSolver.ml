module BS  = BNstats
module SM  = Ast.Symbol.SMap
module Co  = Constants 
module C   = FixConstraint
module F   = Format
module T   = Toplevel
module IA  = Index.AbstractDomain
module SIA = Solve.Make (IA)

open Misc.Ops

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
  F.fprintf ppf "%a \n" IA.print s;
  F.fprintf ppf "@.";
  F.print_flush ();
  close_out oc;
  print_now "Fixpoint: save_raw: END \n"

let solve ac  = 
  let _       = print_now "Fixpoint: Creating  CI\n" in
  let ctx, s  = BS.time "create" SIA.create ac in
  let _       = print_now "Fixpoint: Solving \n" in
  let s, cs'  = BS.time "solve" (SIA.solve ctx) s in

  let _       = print_now "Fixpoint: Saving Result \n" in
  let _       = BS.time "save" (save_raw !Co.out_file cs') s in
  let _       = print_now "Fixpoint: Saving Result DONE \n" in
  cs'

let dump_solve ac = 
  let cs' = solve { ac with Config.bm = SM.map IA.mkbind ac.Config.bm } in
  let _   = BNstats.print stdout "Fixpoint Solver Time \n" in
  match cs' with 
  | [] -> (F.printf "\nSAT\n" ; exit 0)
  | _  -> (F.printf "\nUNSAT\n" ; exit 1)


(*****************************************************************)
(********************* Generate Imp Program **********************)
(*****************************************************************)

let dump_imp a = 
  (List.map (fun c -> Config.Cst c) a.Config.cs ++ List.map (fun c -> Config.Wfc c) a.Config.ws)
  |> ToImp.mk_program
  |> F.fprintf F.std_formatter "%a" Imp.print_program_as_c 
  |> fun _ -> assert false

(*****************************************************************)
(***************** Generate Simplified Constraints ***************)
(*****************************************************************)

let simplify_ts x = 
  if !Co.dump_simp = "andrey" 
  then (x |> List.map Simplification.simplify_t 
          |> List.filter (not <.> Simplification.is_tauto_t)
          |> Simplification.simplify_ts)
  else FixSimplify.simplify_ts x

let dump_simp ac = 
  let ac    = {ac with Config.cs = simplify_ts ac.Config.cs; Config.bm = SM.empty; Config.qs = []} in
  let ctx,_ = BS.time "create" SIA.create ac in
  let s0    = IA.create ac in 
  let _     = BS.time "save" (SIA.save !Co.save_file ctx) s0 in
  exit 1

(*****************************************************************)
(*********************** Main ************************************)
(*****************************************************************)

let usage = "Usage: indexSolver.native <options> [source-files]\noptions are:"

let main () =
  let cs  = usage |> Toplevel.read_inputs |> snd in 
  if !Co.dump_imp then 
    dump_imp cs 
  else if !Co.dump_simp <> "" then 
    dump_simp cs
  else
    dump_solve cs 

let _ = main ()
