(*
 * Copyright © 2009 The Regents of the University of California. 
 * All rights reserved. Permission is hereby granted, without written 
 * agreement and without 
 * license or royalty fees, to use, copy, modify, and distribute this 
 * software and its documentation for any purpose, provided that the 
 * above copyright notice and the following two paragraphs appear in 
 * all copies of this software. 
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY 
 * FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES 
 * ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN 
 * IF THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY 
 * OF SUCH DAMAGE. 
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES, 
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY 
 * AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED HEREUNDER IS 
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION 
 * TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 *)

open Misc.Ops

(******* This module contains globals representing "flags" **************)
let annotsep_name       = "\n\n=+=\n\n"
let global_name         = "GLOBAL"
let lib_path            = Sys.executable_name |> Filename.dirname |> ref

let file: string option ref = ref None         (* last commandline param*)
let safe                = ref false            (* -safe *)
let manual              = ref false            (* -manual *)
let save_file           = ref "out"            (* -save *)
let dump_ref_constraints= ref false            (* -drconstr *)
let ctypes_only         = ref false            (* -ctypes *)
let ol_default          = 2
let verbose_level       = ref ol_default       (* -v *)
let latex_file: string option ref = ref None   (* translate to LaTeX file *)
let armc_file: string option ref  = ref None   (* translate to ARMC file *)
let q_armc_file: string option ref = ref None   (* translate to Q'ARMC file *)
let hc_armc_file: string option ref = ref None   (* translate to HC'ARMC file *)
let dot_file: string option ref = ref None   (* translate to dot file *)
let purify_function_application = ref true  (* replace fun-terms by existentially quantified variables *)
let ptag                        = ref true  (* -ptag *)
let genspec                     = ref false (* -genspec *)
let typespec                    = ref false (* -typespec *)
let simplify_t                  = ref true  (* simplify and prune vacouos FixConstraint.t constraints *)
let root                        = ref ""    (* root function *)
let true_unconstrained          = ref true  (* -true_unconstrained *)
let do_nothing                  = ref false (* -nop *)
(* JHALA: what do these do ? *)
let psimple       = ref true            (* -psimple *)
let no_simple     = ref false           (* -no-simple *)
let verify_simple = ref false           (* -verify-simple *)
let dump_graph    = ref false           (* -dgraph *)
let dropcalls     = ref false           (* -dropcalls *)

(****************************************************************)
(************* Output levels ************************************)
(****************************************************************)

(* verbosity levels by purpose *)
let ol_always = 0
let ol_solve_error = 1
let ol_warning = 1
let ol_solve_master = 2
let ol_solve_stats = 2
let ol_timing = 2
let ol_warn_mlqs = 3
let ol_normalized = 3
let ol_ctypes = 3
let ol_dquals = 4 
let ol_unique_names = 5 (* must be > ol_dquals *)
let ol_solve = 10 
let ol_refine = 11 
let ol_scc = 12 
let ol_dump_env = 10 
let ol_axioms = 5
let ol_dump_prover = 20
let ol_verb_constrs = 21
let ol_dump_wfs = 22
let ol_dump_meas = 30
let ol_dump_quals = 50
let ol_insane = 200

let verb_stack = ref []
let null_formatter = Format.make_formatter (fun a b c -> ()) ignore
let nprintf a = Format.fprintf null_formatter a
let ck_olev l = l <= !verbose_level


let bprintf b = if b then Format.printf else nprintf
let cprintf l = if ck_olev l then Format.printf else nprintf
let ecprintf l = if ck_olev l then Format.eprintf else nprintf

let fcprintf ppf l = if ck_olev l then Format.fprintf ppf else nprintf

let icprintf printer l ppf = if ck_olev l then printer ppf else printer null_formatter

let cprintln l s = if ck_olev l then Printf.ksprintf (Format.printf "@[%s@\n@]") s else nprintf

let elevate_olev l = if ck_olev l then () else verb_stack := !verbose_level :: !verb_stack; verbose_level := l

let restore_olev = match !verb_stack with x :: xs -> verbose_level := x; verb_stack := xs | _ -> ()




(*****************************************************************)
(*********** Command Line Options ********************************)
(*****************************************************************)

(* taken from dsolve/liquid/liquid.ml *)

let arg_spec = 
  [("-save", 
    Arg.String (fun s -> save_file := s), 
    "Save constraints to file [out]"); 
   ("-dropcalls",
     Arg.Set dropcalls,
     "Ignore function calls during consgen [false]");
   ("-drconstr", 
    Arg.Set dump_ref_constraints, 
    "Dump refinement constraints [false]");
   ("-ctypes",
    Arg.Set ctypes_only,
    "Infer ctypes only [false]");
   ("-safe", 
    Arg.Set safe, 
    "run in failsafe mode [false]");
   ("-manual",
    Arg.Set manual,
    "only verify manually-inserted checks");
   ("-ptag", 
    Arg.Set ptag, 
    "prioritize constraints using lexico-ordering on tags [true]");
   ("-genspec", 
    Arg.Set genspec, 
    "Generate spec file only [false]");
   ("-typespec",
    Arg.Set typespec,
    "Use type-directed spec generation");
   ("-root",
    Arg.String (fun s -> root := s),
    "Use root function []");
   ("-psimple", 
    Arg.Set psimple, 
    "prioritize simple constraints [true]");
   ("-dgraph", 
    Arg.Set dump_graph, 
    "dump constraints SCC to constraints.dot [false]");
   ("-notruekvars",
    Arg.Clear true_unconstrained,
    "true unconstrained kvars [true]");
   ("-v", Arg.Int (fun c -> verbose_level := c), 
              "<level> Set degree of analyzer verbosity:\n\
               \032    0      No output\n\
               \032    1      +Verbose errors\n\
               \032    [2]    +Verbose stats, timing\n\
               \032    3      +Print normalized source\n\
               \032    11     +Verbose solver\n\
               \032    13     +Dump constraint graph\n\
               \032    64     +Drowning in output");
   ("-latex", 
    Arg.String (fun s -> 
		  let l = String.length s in
		    if l = 0 || String.sub s (l-4) 4 <> ".tex" then
		      print_endline "-latex: invalid parameter"
		    else
		      latex_file := Some s),
    "translates constraints to LaTeX file"
   );
   ("-armc", 
    Arg.String (fun s -> 
		  let l = String.length s in
		    if l = 0 then
		      print_endline "-armc: invalid parameter"
		    else
		      armc_file := Some s),
    "translate constraints to ARMC file"
   );
   ("-qarmc", 
    Arg.String (fun s -> 
		  let l = String.length s in
		    if l = 0 then
		      print_endline "-qarmc: invalid parameter"
		    else
		      q_armc_file := Some s),
    "translate constraints to Q'ARMC file"
   );
   ("-hcarmc", 
    Arg.String (fun s -> 
		  let l = String.length s in
		    if l = 0 then
		      print_endline "-hcarmc: invalid parameter"
		    else
		      hc_armc_file := Some s),
    "translate constraints to HC'ARMC file"
   );
   ("-dot", 
    Arg.String (fun s -> 
		  let l = String.length s in
		    if l = 0 || String.sub s (l-4) 4 <> ".dot" then
		      print_endline "-dot: invalid parameter"
		    else
		      dot_file := Some s),
    "translate constraints to dot file"
   );
   ("-keep-uif", 
    Arg.Clear purify_function_application,
    "do not replace function terms by existentially quantified variables"
   );
   ("-no-simplify-t", 
    Arg.Clear simplify_t,
    "do not simplify and prune vacuously satisfiable FixConstraint.fit"
   );
   ("-libpath",
    Arg.String (fun s -> lib_path := s), 
    ("library path for default spec, quals ["^(!lib_path)^"]"));
   ("-nop",
    Arg.Set do_nothing,
    "do nothing (useful for regression tests known to be broken)")
  ]


let is_prefix p s = 
  let reg = Str.regexp p in
  Str.string_match reg s 0

(****************************** CIL Specific ********************************)

let is_pure_function s =
  s = "validptr" || 
  s = "assert" || 
  s = "assume"

let is_cil_tempvar s = 
  Misc.is_prefix "__cil_tmp" s || 
  Misc.is_prefix "tmp___" s ||
  Misc.is_prefix "mem_" s

let suffix_of_fn = fun fn -> "_" ^ fn

let rename_local = fun fn vn -> vn ^ (suffix_of_fn fn)

let unrename_local fn vn = 
  let s = suffix_of_fn fn in 
  if not (Misc.is_suffix s vn) then vn else 
    String.sub vn 0 (String.length vn - (String.length s))

(******************************************************************)
(*************** Paths for builtin specs, quals etc ***************)
(******************************************************************)

let get_lib_hquals      = fun () -> Filename.concat !lib_path "lib.hquals"
let get_lib_spec        = fun () -> Filename.concat !lib_path "lib.spec"
let get_lib_h           = fun () -> Filename.concat !lib_path "lib.h"




