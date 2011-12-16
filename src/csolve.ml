(*
 * Copyright © 1990-2009 The Regents of the University of California. All rights reserved. 
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
 *
 *)


(* This file is part of the CSolve Project.*)

module F  = Frontc
module CK = Check
module E  = Errormsg
module A  = Ast
module SM = Misc.StringMap
module SS = Misc.StringSet
module Sy = Ast.Symbol
module BS = BNstats
module C   = FixConstraint
module P   = Pretty
module FI  = FixInterface
module Co  = Constants
module Sp  = Ctypes.RefCTypes.Spec
module RCt = Ctypes.RefCTypes
module U   = Unix
module S   = Sys
module CM  = CilMisc

open Misc.Ops
open Pretty

type outfile = { 
  fname: string;
  fchan: out_channel 
} 

let mydebug = false 

(***********************************************************************)
(****************** TBD: CIL Prepasses *********************************)
(***********************************************************************)

let rename_locals cil =
  Cil.iterGlobals cil
  (function Cil.GFun(fd,_) -> 
    let fn   = fd.Cil.svar.Cil.vname in
    List.iter (fun v -> v.Cil.vname <- Co.rename_local fn v.Cil.vname) fd.Cil.slocals;
    List.iter (fun v -> v.Cil.vname <- Co.rename_local fn v.Cil.vname) fd.Cil.sformals
  | _ -> ())

let parse_file fname =
  let _ = ignore (E.log "Parsing %s\n" fname) in
    Frontc.parse fname ()

let mk_cfg cil =
  Cil.iterGlobals cil begin function
    | Cil.GFun(fd,_) ->
        Cil.prepareCFG fd;
        Cil.computeCFGInfo fd false
    | _ -> ()
  end

let print_header () = 
  Printf.printf " \n \n";
  Printf.printf "$ %s \n" (String.concat " " (Array.to_list Sys.argv));
  Printf.printf "© Copyright 2009 Regents of the University of California.\n";
  Printf.printf "All Rights Reserved.\n"

let mk_options toolname () =
  let us = "Usage: "^toolname^" <options> [source-file] \n options are:" in
  let _  = Arg.parse Co.arg_spec (fun s -> Co.file := Some s) us in
  match !Co.file with
  | Some fn -> Misc.absolute_name fn
  | None    -> assertf "Bug: No input file specified!"

(***********************************************************************)
(*************** Generating Specifications *****************************)  
(***********************************************************************)

(* wipe this *)
let spec_includes file =
  Cil.foldGlobals file begin fun fs -> function
    | Cil.GPragma (Cil.Attr ("csolve", [Cil.ACons ("include", []); Cil.AStr f]), _) 
        -> f :: fs
    | _                                                          
        -> fs 
  end []
  >> (String.concat ", " <+> E.log "Including Specs: %s \n" <+> ignore)  

(* {{{
let add_spec fn spec_src = 
  let _  = E.log "Parsing spec: %s \n" fn in
  let _  = Errorline.startFile fn in
  try
    let ic = open_in fn in
    ic |> Lexing.from_channel
       |> set_lex_start_pos fn
       |> RefParse.specs RefLex.token
       >> (RCt.Spec.store <+> RCt.Store.closed <+> Misc.flip asserts "Global store not closed") 
       >> (fun _ -> close_in ic)
       |> RCt.Spec.add spec_src
  with Sys_error s ->
    let _ = E.warn "Error reading spec: %s@!@!Continuing without spec...@!@!" s in
    spec_src


}}} *)

let set_lex_start_pos file lb =
  let p = {Lexing.pos_fname = file; Lexing.pos_lnum = 1; Lexing.pos_cnum = 0; Lexing.pos_bol = 0} in
    {lb with Lexing.lex_start_p = p; Lexing.lex_curr_p = p}

let parseOneSpec fn = 
  let _  = E.log "Parsing spec: %s \n" fn in
  let _  = Errorline.startFile fn in
  try
    let ic = open_in fn in
    ic |> Lexing.from_channel
       |> set_lex_start_pos fn
       |> RefParse.specs RefLex.token
       >> (Sp.store <+> RCt.Store.closed RCt.Store.empty <+> Misc.flip asserts "Global store not closed")
       >> (fun _ -> close_in ic)
       |> some
  with Sys_error s ->
    let _ = E.warn "Error reading spec: %s@!@!Continuing without spec...@!@!" s in
    None 

let spec_of_file outprefix file =
  let specfile = outprefix^".autospec" in
    Typespec.writeSpecOfFile file specfile;
    specfile |> parseOneSpec |> Misc.maybe

let decs_of_file cil = 
  let reachf = CM.reachable cil in
  Cil.foldGlobals cil (fun acc g -> g :: acc) []
  |> List.rev  
  |> Misc.map_partial CM.dec_of_global
  |> Misc.filter (function CM.FunDec (vn,_,_) -> reachf vn | _ -> true)

let project_spec fns spec =
  let fspec = spec
           |> Sp.funspec
           |> SM.mapi (fun fn (cf, b) -> (cf, if SS.mem fn fns then Ctypes.IsSubtype else b)) in
  Sp.make fspec (Sp.varspec spec) (Sp.store spec) (Sp.locspectypes spec)

let incremental_spec outprefix fns spec =
  match parseOneSpec (outprefix ^".infspec") with
  | Some ispec -> ispec |> project_spec fns |> RCt.Spec.add spec 
  | None -> E.s (E.error "Incremental Checking Requires a saved spec file; see above for details.")

let incremental_decs fns decs =
  List.filter begin function 
    | CM.FunDec (v,_,_) -> SS.mem v fns
    | _                 -> true
  end decs

let obligations outprefix file cil =
  let spec = spec_of_file outprefix file in
  let decs = decs_of_file file in
  (if SS.is_empty !Co.inccheck then 
    (spec, decs)
   else 
    ( incremental_spec outprefix !Co.inccheck spec
    , incremental_decs !Co.inccheck decs))

(************************************************************************)
(************************* Original CSolve ******************************)
(************************************************************************)

let outChannel : outfile option ref    = ref None
let mergedChannel : outfile option ref = ref None

let parseOneFile (fname: string) : Cil.file =
  (* PARSE and convert to CIL *)
  if !Cilutil.printStages then ignore (E.log "Parsing %s\n" fname);
  let cil = F.parse fname () in
  
    (* sm: remove unused temps to cut down on gcc warnings  *)
    (* (Stats.time "usedVar" Rmtmps.removeUnusedTemps cil);  *)
    (* (trace "sm" (dprintf "removing unused temporaries\n")); *)
    (Rmtmps.removeUnusedTemps cil);
    cil
 (* Pre-passes from blastCilInterface.ml:
  * one return value
  * simplify boolean expressions *)
let mydebug = false 

let location_of_constraint tgr = 
  FixConstraint.tag_of_t <+> CilTag.t_of_tag <+> CilTag.loc_of_t tgr

let print_unsat_locs tgr s ucs =
  let ucs = Misc.fsort (location_of_constraint tgr) ucs in
  List.iter begin fun c ->
    printf "\n%a:\n\n%a\n" 
      Cil.d_loc (location_of_constraint tgr c) 
      (fun () -> FixConstraint.print_t (Some s) |> CilMisc.doc_of_formatter) c
    |> ignore
  end ucs

let cil_of_file file =
  file |> Simplemem.simplemem 
       >> CilMisc.unfloat 
       >> CilMisc.Pheapify.doVisit 
       >> Psimplify.simplify
       >> Simpleret.simpleret 
       >> Rmtmps.removeUnusedTemps 
       >> CilMisc.purify 
       >> CilMisc.CopyGlobal.doVisit
       >> CilMisc.NameNullPtrs.doVisit
       >> mk_cfg 
       >> rename_locals 

let liquidate file =
  let log       = open_out "csolve.log" in
  let _         = E.logChannel := log in
  let _         = Co.setLogChannel log in
  let cil       = BS.time "Parse: source" cil_of_file file in
  let _         = EffectDecls.parseEffectDecls cil in
  let _         = E.log "DONE: cil parsing \n" in
  let fn        = !Co.csolve_file_prefix (* file.Cil.fileName *) in
  let qs        = Misc.flap FixAstInterface.quals_of_file [Co.get_lib_hquals (); (!Co.csolve_file_prefix ^ ".hquals")] in
  let _         = E.log "DONE: qualifier parsing \n" in
  let spec,decs = BS.time "Parse: spec" (obligations !Co.csolve_file_prefix file) cil in
  let _         = E.log "DONE: spec parsing \n" in
  let tgr, ci   = BS.time "Cons: Generate" (Consgen.create cil spec) decs in
  let _         = E.log "DONE: constraint generation \n" in
  let s', cs'   = Consindex.solve ci fn qs in
  let _         = E.log "DONE SOLVING" in
  let _         = Annots.dump_annots (Some s') in
  let _         = if SS.is_empty !Co.inccheck then Annots.dump_infspec decs s' in
  let _         = print_unsat_locs tgr s' cs' in
  let _         = BS.print log "\nCSolve Time \n" in
  match cs' with 
  | [] -> let _ = printf "\nSAFE\n"   in cil
  | _  -> let _ = printf "\nUNSAFE\n" in exit 1

let rec processOneFile (cil: Cil.file) =
  begin
    if !Cilutil.doCheck then begin
      ignore (E.log "First CIL check\n");
      if not (CK.checkFile [] cil) && !Cilutil.strictChecking then begin
        E.bug ("CIL's internal data structures are inconsistent "
               ^^"(see the warnings above).  This may be a bug "
               ^^"in CIL.\n")
      end
    end;
    (match !outChannel with
      None -> ()
    | Some c ->
        (* Note GCC gets mad if we print block attributes *)
        Stats.time "printCIL" 
	(Cil.dumpFile CilMisc.noBlockAttrPrinter c.fchan c.fname) cil);

    let _ = liquidate cil in
    if !E.hadErrors then
      E.s (E.error "Error while processing file; see above for details.");

  end

let setTimeout () =
  if !Constants.timeout > 0 then
    let pid = U.fork () in
    if pid != 0 then
      let kill = fun _ -> U.kill pid 9; exit 3 in
      let _    = S.set_signal S.sigalrm (S.Signal_handle kill) in
      let _    = U.alarm !Constants.timeout in
        match snd <| U.wait () with
          | U.WEXITED n -> exit n
          | _           -> exit 3

(* pmr: Note this is ganked from CIL *)        
(***** MAIN *****)  
let theMain () =
  let usageMsg = "Usage: csolve [options] source-files" in
  (* Processign of output file arguments *)
  let openFile (what: string) (takeit: outfile -> unit) (fl: string) = 
    if !E.verboseFlag then
      ignore (Printf.printf "Setting %s to %s\n" what fl);
    (try takeit { fname = fl;
                  fchan = open_out fl }
    with _ ->
      raise (Arg.Bad ("Cannot open " ^ what ^ " file " ^ fl)))
  in
  let outName = ref "" in
  (* sm: enabling this by default, since I think usually we
   * want 'cilly' transformations to preserve annotations; I
   * can easily add a command-line flag if someone sometimes
   * wants these suppressed *)
  Cil.print_CIL_Input := true;

  (*********** COMMAND LINE ARGUMENTS *****************)
  let blankLine = ("", Arg.Unit (fun _ -> ()), "") in

  (* Construct the arguments for the features configured from the Makefile *)
  let argDescr = Ciloptions.options @ 
        [ 
          "--out", Arg.String (openFile "output" 
                                 (fun oc -> outChannel := Some oc)),
              " the name of the output CIL file.\n\t\t\t\tThe cilly script sets this for you.";
          "--mergedout", Arg.String (openFile "merged output"
                                       (fun oc -> mergedChannel := Some oc)),
              " specify the name of the merged file";
          "--csolveprefix", Arg.Set_string Co.csolve_file_prefix,
              " specify prefix to use for spec, hquals, ssa, annot files"
        ] @ blankLine :: List.map (Misc.app_fst3 (fun s -> "-" ^ s)) Constants.arg_spec in
  begin
    (* this point in the code is the program entry point *)

    Stats.reset Stats.HardwareIfAvail;

    (* parse the command-line arguments *)
    Arg.current := 0;
    let envopts = try "CSOLVEFLAGS" |> S.getenv |> Misc.chop_star " \\|=" |> Array.of_list with Not_found -> [| |] in
      begin
        try Arg.parse_argv (Array.concat [Sys.argv; envopts]) (Arg.align argDescr) Ciloptions.recordFile usageMsg
        with Arg.Help usage -> Format.printf "%s" usage; exit 0
      end;
    if !Constants.do_nothing then exit 0;
    setTimeout ();
    Cil.initCIL ();
    Cil.useLogicalOperators := true;

    Ciloptions.fileNames := List.rev !Ciloptions.fileNames;

    if !Cilutil.testcil <> "" then begin
      Testcil.doit !Cilutil.testcil
    end else

      (* parse each of the files named on the command line, to CIL *)
      let files = List.map parseOneFile !Ciloptions.fileNames in

      (* if there's more than one source file, merge them together; *)
      (* now we have just one CIL "file" to deal with *)
      let one =
        match files with
          [one] -> one
        | [] -> E.s (E.error "No arguments for CIL")
        | _ ->
            let merged =
              Stats.time "merge" (Mergecil.merge files)
                (if !outName = "" then "stdout" else !outName) in
            if !E.hadErrors then
              E.s (E.error "There were errors during merging");
            (* See if we must save the merged file *)
            (match !mergedChannel with
              None -> ()
            | Some mc -> begin
                let oldpci = !Cil.print_CIL_Input in
                Cil.print_CIL_Input := true;
                Stats.time "printMerged"
                  (Cil.dumpFile !Cil.printerForMaincil mc.fchan mc.fname) merged;
                Cil.print_CIL_Input := oldpci
            end);
            merged
      in

      if !E.hadErrors then
        E.s (E.error "Cabs2cil had some errors");

      (* process the CIL file (merged if necessary) *)
      processOneFile one
  end
;;
                                        (* Define a wrapper for main to 
                                         * intercept the exit *)
let failed = ref false 

let cleanup () = 
  if !E.verboseFlag || !Cilutil.printStats then
    Stats.print stderr "Timings:\n";
  if !E.logChannel != stderr then 
    close_out (! E.logChannel);  
  (match ! outChannel with Some c -> close_out c.fchan | _ -> ())


(* Without this handler, cilly.asm.exe will quit silently with return code 0
   when a segfault happens. *)
let handleSEGV code =
  if !Cil.currentLoc == Cil.locUnknown then
    E.log  "**** Segmentation fault (possibly a stack overflow)\n"
  else begin
    E.log ("**** Segmentation fault (possibly a stack overflow) "^^
           "while processing %a\n")
      Cil.d_loc !Cil.currentLoc
  end;
  exit code

let _ = Sys.set_signal Sys.sigsegv (Sys.Signal_handle handleSEGV);

begin 
  try
    theMain (); 
  with F.CabsOnly -> (* this is OK *) ()
end;
cleanup ();
exit (if !failed then 1 else 0)

