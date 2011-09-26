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

(* This file is part of the liquidC Project.*)


module BS = BNstats
module  P = Pretty
module  C = FixConstraint
module CM = CilMisc
module YM = Ast.Symbol.SMap
module SM = Misc.StringMap
module  Q = Ast.Qualifier 

module FA = FixAstInterface
module FI = FixInterface

module Ct = Ctypes
module SPA = Solve.Make (PredAbs)
module SIA = Solve.Make (AbstractDomain)
module Ix = Index  

open Misc.Ops
open Cil

(****************************************************************)
(********************** Constraint Indexing *********************)
(****************************************************************)

type t = {
  scim : Ssa_transform.ssaCfgInfo SM.t;
  wfm  : C.wf list SM.t;
  cm   : C.t list SM.t;
  defm : (varinfo * Ct.refctype) list SM.t;
  depm : C.dep list SM.t;
}

type bind = PredAbs.bind

(* API *)
let create (ws, cs, des, ds) = 
  { scim  = SM.empty
  ; wfm   = SM.empty |> SM.add Constants.global_name ws
  ; cm    = SM.empty |> SM.add Constants.global_name cs
  ; defm  = SM.empty |> SM.add Constants.global_name des
  ; depm  = SM.empty |> SM.add Constants.global_name ds }

(* API *)
let add me fn sci (ws, cs, des, ds) =
  { scim = SM.add fn sci me.scim 
  ; wfm  = SM.add fn ws  me.wfm 
  ; cm   = SM.add fn cs  me.cm 
  ; defm = SM.add fn des me.defm
  ; depm = SM.add fn ds  me.depm }

let find me fn = (SM.find fn me.scim, SM.find fn me.wfm, SM.find fn me.cm)

let iter me f = 
  SM.iter (fun fn _ -> f fn (find me fn)) me.scim

let fold me f =
  SM.fold (fun fn _ b -> f fn (find me fn) b) me.scim

let get_wfs  = fun me -> SM.fold (fun _ ws acc -> ws ++ acc) me.wfm  []
let get_cs   = fun me -> SM.fold (fun _ cs acc -> cs ++ acc) me.cm   []
let get_defs = fun me -> SM.fold (fun _ des acc -> des ++ acc) me.defm []
let get_deps = fun me -> SM.fold (fun _ ds acc -> ds ++ acc) me.depm []

let (++) = P.concat

(* API *)
let print so () me =
  match so with 
  | None -> (* print constraints *) 
      fold me begin
        fun fn (_, wfs, cs) d ->
          P.dprintf "Ref-Constraints for %s \n" fn ++
          CM.doc_of_formatter (Misc.pprint_many true "\n" (C.print_t None)) cs ++
          P.dprintf "WF-Constraints for %s \n" fn ++
          CM.doc_of_formatter (Misc.pprint_many true "\n" (C.print_wf None)) wfs ++
          d
      end P.nil
  | Some _ -> (* print solution *)
      me |> get_wfs
         |> Misc.map C.env_of_wf 
         |> Misc.flap C.bindings_of_env 
         |> CM.doc_of_formatter (Misc.pprint_many false "\n" (C.print_binding so))
         |> P.concat (P.text "Liquid Types:\n\n")

let config ts env ps a ds cs ws qs assm = 
  { Config.empty with 
      Config.a    = a
    ; Config.ts   = ts
    ; Config.uops = env
    ; Config.ps   = ps
    ; Config.ds   = ds
    ; Config.cs   = cs
    ; Config.ws   = ws
    ; Config.qs   = qs 
    ; Config.assm = assm
  }


type ('a, 'b, 'c, 'd, 'e) domain = 
  { create : 'a 
  ; save   : 'b
  ; solve  : 'c
  ; read : 'd
  ; read_bind   : 'e
  }


let d_predAbs   = { create = SPA.create
		  ; save   = SPA.save
		  ; solve  = SPA.solve
		  ; read   = SPA.read
		  ; read_bind = SPA.read_bind
		  }

let d_indexAbs   = { create = SIA.create
		   ; save   = SIA.save
		   ; solve  = SIA.solve
		   ; read   = SIA.read
		   ; read_bind = SIA.read_bind
		   }

let ac_solve dd me fn (ws, cs, ds) qs so =
  let env     = YM.map FixConstraint.sort_of_reft FA.builtinm in
  let assm    = match so with Some s0 -> s0 | _ -> C.empty_solution in
  let cfg     = config FA.sorts env FA.axioms 4 ds cs ws qs assm in
  let ctx, s  = BS.time "Qual Inst" dd.create cfg in
  let _       = Errormsg.log "DONE: qualifier instantiation \n" in
  let _       = Errormsg.log "DONE: solution strengthening \n" in
  let _       = BS.time "save in" (dd.save (fn^".in.fq") ctx) s in
  let _       = Errormsg.log "DONE: saving input constraints \n" in
  let s',cs'  = BS.time "Cons: Solve" (dd.solve ctx) s in 
  let _       = Errormsg.log "DONE: constraint solving \n" in
  let _       = BS.time "save out" (dd.save (fn^".out.fq") ctx) s' in
  let _       = Errormsg.log "DONE: saving output constraints \n" in
  s', cs'

let filter_cstrs dd s fp (ws, cs) = 
  let sol = dd.read s in
  let fr  = fun ((v,_,_) as r) -> r |> C.preds_of_reft sol |> Ast.pAnd |> fp v in
  (Misc.filter (fr <.> C.reft_of_wf) ws, Misc.filter (fr <.> C.rhs_of_t) cs)

let ac_scalar_solve dd me fn fp (ws, cs, ds) (eqs, bqs, mqs) =
  Misc.with_ref_at Constants.slice false begin fun () ->
    ac_solve dd me (fn^".eq")  (ws, cs, ds) eqs None |> fst
  end

let get_cstrs me = 
  (get_wfs me, get_cs me, get_deps me)
  (* >> (fun (ws, cs, ds) -> if ws = [] then failwith "NO WF CONSTRAINTS")
*)

(* API *)
let solve me fn qs =
(*  let _ = SM.map (List.map (Format.printf "Solve: me.cm: %a" (C.print_t None))) (me.cm) in *)
  let _ = Pretty.printf "***ac_solve for int starting***\n" in
  let _ = ac_solve d_indexAbs me fn (get_cstrs me) qs None
  |> fun (s, c) -> let _ = Pretty.printf "***ac_solve for int done***\n" in (s,c)
  |> (fun (s, c) -> let _ = AbstractDomain.dump s in (s, c))
  |> Misc.app_fst d_indexAbs.read
  |> fun (s', cs) -> match cs with
       | [] ->  let _ = Pretty.printf "woohoo\n!" in ()
       | cs' ->
	   let _ = List.map (Format.printf "Solve: unsat: %a" (C.print_t None)) cs' in
	     ()
  in
  ac_solve d_predAbs me fn (get_cstrs me) qs None 
  |> Misc.app_fst d_predAbs.read

let scalar_solve me fn fp qs =
  let index_of_refc s v cr =
    Ctypes.reft_of_refctype cr
  |> FixConstraint.ras_of_reft
  |> List.map (fun ra ->
		 match ra with
		   | FixConstraint.Conc p -> ScalarCtypes.index_of_var v (cr, p)
		   | FixConstraint.Kvar (subs, k) -> AbstractDomain.read_bind s k)
  |> List.fold_left Index.glb Index.top
  in
(*   foldl f . map g = foldl f.g ; or something        *)
  let qst = ScalarCtypes.partition_scalar_quals qs in
  let s   = ac_scalar_solve d_indexAbs me fn fp (get_cstrs me) qst in
  me.defm
  |> SM.map (List.map (fun (v, cr) -> (v, index_of_refc s v cr)))
  |> SM.map CM.vm_of_list
    
(* {{{
let solve (d_create, d_save, d_solve, d_read) me fn qs = 
  let ws     = get_wfs me in
  let cs     = get_cs me in
  let ds     = get_deps me in
  let env    = YM.map FixConstraint.sort_of_reft FA.builtinm in
  let cfg    = config FA.sorts env FA.axioms 4 ds cs ws qs in
  let ctx, s = BS.time "Qual Inst" d_create cfg in
  let _      = Errormsg.log "DONE: qualifier instantiation \n" in
  let _      = BS.time "save in" (d_save (fn^".in.fq") ctx) s in
  let s',cs' = BS.time "Cons: Solve" (d_solve ctx) s in 
  let _      = Errormsg.log "DONE: constraint solving \n" in
  let _      = BS.time "save out" (d_save (fn^".out.fq") ctx) s' in
  (d_read s'), cs'

(* API *)
let solve = solve d_predAbs 

}}} *)
