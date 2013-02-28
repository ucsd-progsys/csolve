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

module F   = Frontc
module CK  = Check
module E   = Errormsg
module A   = Ast
module Sy  = Ast.Symbol
module BS  = BNstats
module C   = FixConstraint
module P   = Pretty
module FI  = FixInterface
module Co  = Constants
module Ci  = Consindex
module Sp  = Ctypes.RefCTypes.Spec
module RCt = Ctypes.RefCTypes
module U   = Unix
module S   = Sys
module ST = Ssa_transform
module CM  = CilMisc
module T   = CilTag
module Misc = FixMisc 
module SM = Misc.StringMap
module SS = Misc.StringSet

open Pretty
open Misc.Ops


type outfile = { 
  fname: string;
  fchan: out_channel 
} 

let mydebug = false 

(***********************************************************************)
(****************** TBD: CIL Prepasses *********************************)
(***********************************************************************)

let dump_globals cil = 
  Cil.iterGlobals cil (fun g -> ignore <| Pretty.printf "GLOBAL: %a\n" Cil.d_global g)

let rename_locals cil =
  Cil.iterGlobals cil begin function 
    | Cil.GFun(fd,_) -> 
        let fn = fd.Cil.svar.Cil.vname in
        List.iter begin fun v -> 
          v.Cil.vname <- CM.rename_local fn v.Cil.vname
        end (fd.Cil.slocals ++ fd.Cil.sformals)
    | _ ->  ()
  end;
  Cil.visitCilFile (new Cil.nopCilVisitor) cil (* rebuild type-sig with new formals *)

  (* 
      let _  = List.iter (fun v -> ) fd.Cil.slocals in
      let _  = List.iter (fun v -> v.Cil.vname <- CM.rename_local fn v.Cil.vname) fd.Cil.sformals in 
    let _  = fd.Cil.sformals
             |>: (fun v -> Cil.copyVarinfo v (CM.rename_local fn v.Cil.vname))
             |> Cil.setFormals fd 
    ()
  | _ -> ())
*)

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
  Printf.printf "© Copyright 2009-12 Regents of the University of California.\n";
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
    Typespec.writeSpecOfFile file specfile
    |> Ctypes.RefCTypes.Spec.map begin fun rct ->
        Ctypes.ctype_of_refctype rct
        |> Misc.flip FI.t_ctype_refctype rct
    end
    (* specfile |> parseOneSpec |> Misc.maybe *)

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

let obligations outprefix file =
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

let mydebug = false 

(*
let loc_of_tag tgr = T.t_of_tag <+> T.loc_of_t tgr
*)

let loc_of_cstr tgr = FixConstraint.tag_of_t <+> T.loc_of_tag tgr

let d_cstr_error tgr () c = 
  let loc, cause = FixConstraint.tag_of_t c
                   |> (T.loc_of_tag tgr <*> T.cause_of_tag tgr)
  in Pretty.dprintf "%a %a" Cil.d_loc loc (T.d_cause tgr) cause

let print_result_short tgr res oc = match res.FI.unsats with 
  | [] -> 
        fprintf oc "*********************************************************\n"
      ; fprintf oc "********************** SAFE *****************************\n"
      ; fprintf oc "*********************************************************\n"
  | cs ->
        let cs = Misc.fsort (loc_of_cstr tgr) cs in 
        fprintf oc "**********************************************************\n"
      ; (cs |>: fprintf oc "******** ERROR %a \n" (d_cstr_error tgr))
      ; fprintf oc "**********************************************************\n"
 (* ; (cs |>: loc_of_cstr tgr |>: fprintf oc "******** ERROR %a \n" Cil.d_loc) *)   
 
let print_result_long tgr res oc =
  res.FI.unsats 
  |> Misc.fsort (loc_of_cstr tgr)
  |> List.iter begin fun c -> 
       fprintf oc "\n%a:\n\n%a\n" Cil.d_loc (loc_of_cstr tgr c) 
       (fun () -> FixConstraint.print_t (Some res.FI.soln) |> CM.doc_of_formatter) c
       |> ignore
     end

let print_result tgr res oc = 
  print_result_long tgr res oc;
  print_result_short tgr res oc

let print_unsat_locs tgr res =
  print_result tgr res stdout;
  Misc.with_out_file (!Co.csolve_file_prefix ^ ".out") (print_result tgr res)

let cil_transform_phase_1 file =
  file |> Psimplemem.simplemem 
       >> CM.unfloat 
       >> CM.Pheapify.doVisit 
       >> Psimplify.simplify
       >> Simpleret.simpleret 
       >> Rmtmps.removeUnusedTemps 
       >> CM.purify 
       >> CM.CopyGlobal.doVisit
       >> CM.NameNullPtrs.doVisit
       >> mk_cfg

let cil_transform_phase_2 file = 
  file >> rename_locals
       (* >> (CM.varExprMap <+> ignore) *)

let dump_annots cil qs tgr res =
  let s     = res.FI.soln                                         in
  let cs    = res.FI.unsats                                       in
  let cones = res.FI.ucones |>: (Ast.Cone.map (T.loc_of_tag tgr)) in 
  let binds = Annots.dump_bindings ()                             in
  let files = CM.source_files cil                                 in
  let files = if !Co.web_demo then [List.hd files] else files     in
  (* 1. Dump Text Annots *)
  Annots.dump_annots (Some s);
  (* 2. Dump HTML Annots *)
  AnnotsJson.dump_html files qs tgr s cs cones binds

let dumpFile cil = 
  let log = open_out (!Co.csolve_file_prefix ^ ".csolve.save.c") in
  let _   = Cil.dumpFile CM.noBlockAttrPrinter log ".csolve.save.c"  cil in
  let _   = close_out log in
  ()

let liquidate cil =
  let _         = dumpFile cil in
  let log       = open_out (!Co.csolve_file_prefix ^ ".log") in
  let _         = E.logChannel := log in
  let _         = Co.setLogChannel log in

  let cil       = BS.time "Cil: phase 1" cil_transform_phase_1 cil in
  let spec,decs = BS.time "Parse: spec" (obligations !Co.csolve_file_prefix) cil in
  let _         = E.log "DONE: spec parsing \n" in
  let cil       = BS.time "Cil: phase 2" cil_transform_phase_2 cil in

  let _         = EffectDecls.parseEffectDecls cil in
  let _         = E.log "DONE: cil parsing \n" in
  let _         = Co.save_file := !Co.csolve_file_prefix ^ ".out.fq" in
  let qfs       = (!Co.csolve_file_prefix ^ ".hquals") :: if !Co.no_lib_hquals then [] else [Co.get_lib_hquals ()] in
  let qs        = GenQuals.quals_of_cil cil ++ Misc.flap FixAstInterface.quals_of_file qfs in
  let _         = E.log "DONE: qualifier parsing \n" in
  let scim      = ST.scim_of_decs decs  in 
  let _         = E.log "\nDONE: SSA conversion \n" in
  let tgr       = scim |> SM.to_list |> Misc.map snd |> T.create in
  let _         = E.log "\nDONE: TAG initialization\n" in
  let gst,ci    = BS.time "Cons: Generate" (Consgen.create cil spec decs scim) tgr in
  let _         = E.log "DONE: constraint generation \n" in
  let res       = Ci.solve ci !Co.csolve_file_prefix qs in
  let _         = E.log "DONE SOLVING" in
  let _         = if !Co.vannots then BS.time "Annots: dump" dump_annots cil qs tgr res in
  let _         = if SS.is_empty !Co.inccheck then Annots.dump_infspec gst decs qs res.FI.soln in
  let _         = print_unsat_locs tgr res in
  let _         = BS.print log "\nCSolve Time \n" in
  match res.FI.unsats with 
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
	(Cil.dumpFile CM.noBlockAttrPrinter c.fchan c.fname) cil);

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
          "--csolveprefix", Arg.String Co.set_csolve_file_prefix,
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
    Pretty.flushOften := true;
    theMain (); 
  with F.CabsOnly -> (* this is OK *) ()
end;
cleanup ();
exit (if !failed then 1 else 0)


