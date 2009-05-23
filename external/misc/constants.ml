(*
 * Copyright © 2009 The Regents of the University of California. All rights reserved. 
 *
 * Permission is hereby granted, without written agreement and without 
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

(** This module contains globals representing "flags" **************)

let safe                 = ref false            (* -safe *)
let global_name          = "GLOBAL"
let save_file            = ref "out"            (* -save *)
let dump_ref_constraints = ref false            (* -drconstr *)
let ol_default           = 2
let verbose_level        = ref ol_default       (* -v *)
(* JHALA: what do these do ? *)
let psimple       = ref true            (* -psimple *)
let no_simple     = ref false           (* -no-simple *)
let verify_simple = ref false           (* -verify-simple *)
let dump_graph    = ref false           (* -dgraph *)
let ctypes        = ref false           (* -ctypes *)

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
   ("-ctypes",
     Arg.Set ctypes,
     "Compute CTypes (also inlines functions) [false]");
   ("-drconstr", 
    Arg.Set dump_ref_constraints, 
    "Dump refinement constraints [false]");
   ("-safe", 
    Arg.Set safe, 
    "run in failsafe mode [false]");
   ("-psimple", 
    Arg.Set psimple, 
    "prioritize simple constraints [true]");
   ("-dgraph", 
    Arg.Set dump_graph, 
    "dump constraints SCC to constraints.dot [false]");
   ("-v", Arg.Int (fun c -> verbose_level := c), 
              "<level> Set degree of analyzer verbosity:\n\
               \032    0      No output\n\
               \032    1      +Verbose errors\n\
               \032    [2]    +Verbose stats, timing\n\
               \032    3      +Print normalized source\n\
               \032    11     +Verbose solver\n\
               \032    13     +Dump constraint graph\n\
               \032    64     +Drowning in output") 
  ]

