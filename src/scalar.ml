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

module A  = Ast

module CM = CilMisc
module CI = CilInterface
module VM = CM.VarMap
module Sy = A.Symbol
module Su = A.Subst
module FI = FixInterface
module SM = Misc.StringMap
module YM = A.Symbol.SMap
module ST = Ssa_transform
module Ct = Ctypes
module Ix = Ct.Index
module Co = Constants
module P  = A.Predicate 
module Q  = A.Qualifier

module Ci = Consindex
module E  = Errormsg
module Sc = ScalarCtypes

open Misc.Ops



(***************************************************************************)
(************************ Generate Scalar Constraints **********************)
(***************************************************************************)

let generate tgr gnv scim : Ci.t =
  ([], [], [], [])
  |> Ci.create  
  |> ConsVisitor.cons_of_scis tgr gnv Ct.refstore_empty scim None

(***************************************************************************)
(*************************** Solve Scalar Constraints **********************)
(***************************************************************************)

let solve cil ci = 
  (Sc.scalar_quals_of_file cil) 
  |> Ci.force ci (!Co.liquidc_file_prefix^".scalar")
  |> SM.map (VM.mapi Sc.index_of_pred)

(***************************************************************************)
(***** Close with Bindings for Params, Undef Vars Scalar Constraints *******)
(***************************************************************************)

let ix_binds_of_spec spec fn : (string * Ix.t) list =
  spec |> FI.cspec_of_refspec 
       |> Ct.I.Spec.get_fun fn
       |> fst
       |> (fun x -> x.Ct.args) 
       |> List.map (Misc.app_snd Ct.index_of_ctype)
 
let close_formals args (formals : Cil.varinfo list) : Ix.t VM.t -> Ix.t VM.t = 
  formals
  |> List.map (fun v -> (v, List.assoc v.Cil.vname args))
  |> CM.vm_of_list 
  |> VM.fold (fun k v acc -> VM.add k v acc)

let close_locals locals vm =
  locals 
  |> List.filter (fun v -> not (VM.mem v vm)) 
  |> List.fold_left (fun vm v -> if VM.mem v vm then vm else VM.add v Ix.top vm) vm

let close scim spec sim =
  SM.mapi begin fun fn vm ->
    if fn = Co.global_name then vm else
      let _    = asserti (SM.mem fn scim) "Scalar.close: function %s missing from scim" fn in
      let fdec = (SM.find fn scim).ST.fdec in
      let args = ix_binds_of_spec spec fn in
      (vm |> close_formals args fdec.Cil.sformals 
          |> close_locals fdec.Cil.slocals)
  end sim

(***************************************************************************)
(************************ Inject into Ctype ********************************)
(***************************************************************************)

let ctype_of_var_index v ix =
  let _ = Cil.currentLoc := v.Cil.vdecl in
  let t = Cil.unrollType v.Cil.vtype in
  match t with
  | Cil.TInt (ik, _)        -> Ct.Int (Cil.bytesSizeOfInt ik, ix)
  | Cil.TEnum (ei, _)       -> Ct.Int (Cil.bytesSizeOfInt ei.Cil.ekind, ix)
  | Cil.TFloat _            -> Ct.Int (CM.typ_width t, ix)
  | Cil.TVoid _             -> Ct.void_ctype
  | Cil.TPtr (Cil.TFun _ , _) -> Ct.Top ix
  | Cil.TPtr _ | Cil.TArray _ -> Ct.Ref (Sloc.none, ix)
  | _  when !Constants.safe -> halt <| Cil.error "Scalar.ctype_of_ciltype_index %s" v.Cil.vname
  | _                       -> (Cil.warn "Scalar.ctype_of_ciltype_index %s of tricky type %a@!@!" 
                                v.Cil.vname Cil.d_type t; Ct.Top ix)

(***************************************************************************)
(*********************************** API ***********************************)
(***************************************************************************)

let scalarinv_of_scim cil spec tgr gnv scim =
  scim 
  >> FI.annot_clear 
  |> generate tgr gnv 
  |> solve cil
  |> close scim spec
  |> SM.map (VM.mapi ctype_of_var_index)
  >> FI.annot_clear

(***************************************************************************)
(************************* TESTING SCALAR INVS *****************************)
(***************************************************************************)

type scalar_error = 
  | MissingFun of string 
  | MissingVar of string * Cil.varinfo * Ix.t 
  | DiffIndex of string * Cil.varinfo * Ix.t * Ix.t

let d_scalar_error () = function
  | MissingFun fn -> 
      Pretty.dprintf "[SCALAR ERROR in %s] Missing Function" fn
  | MissingVar (fn, v, ix) -> 
      Pretty.dprintf "[SCALAR ERROR in %s] Missing Variable %s: inferctypes=%a" fn 
      v.Cil.vname Ix.d_index ix
  | DiffIndex (fn, v, ix, ix') ->
      Pretty.dprintf "[SCALAR ERROR in %s] Different Index %s: inferctypes=%a vs scalar=%a" fn 
      v.Cil.vname Ix.d_index ix Ix.d_index ix' 

let check_index oc fn v ix ix' =
  Pretty.fprintf oc "%s : inferctypes = %a, scalar = %a \n" 
  v.Cil.vname Ix.d_index ix Ix.d_index ix';
  Ix.is_subindex ix' ix   (* ix' ==> ix *)

let check_scalar uvm shm sim = 
  let oc  = open_out (!Co.liquidc_file_prefix ^ ".scalarlog") in
  (SM.fold begin fun fn { Shape.vtyps = vcts } errs ->
    let vcts = List.filter (fun (v, _) -> VM.mem v uvm) vcts in
    if not (SM.mem fn sim) then (MissingFun fn :: errs) else 
      let im = SM.find fn sim in
      List.fold_left begin fun errs (v, ct) ->
        let ix = Ct.index_of_ctype ct in
        if ix = Ix.IBot then errs else
        if (not (VM.mem v im)) then (MissingVar (fn, v, ix) :: errs) else
          let ct'  = VM.find v im in
          let ix'  = Ct.index_of_ctype ct' in
          if check_index oc fn v ix ix' then errs else (DiffIndex (fn, v, ix, ix')) ::errs
      end errs vcts 
   end shm [])
  >> (fun _ -> close_out oc)

let dump_quals_to_file (fname: string) (qs: Q.t list) : unit = 
  let oc  = open_out fname in
  let ppf = Format.formatter_of_out_channel oc in
  Format.fprintf ppf "@[%a@]\n" (Misc.pprint_many true "\n" Q.print) qs;
  close_out oc

(* API *) 
let test cil spec tgr gnv scim shm = 
  let sim = scalarinv_of_scim cil spec tgr gnv scim in
  let uvm = ref VM.empty in
  let _   = CM.iterUsedVars cil (fun v -> uvm := VM.add v () !uvm) in
  check_scalar !uvm shm sim
  >> (List.iter (fun e -> E.warn "%a \n" d_scalar_error e |> ignore))
  >> (fun _ -> E.log "DONE: scalar testing \n")
  |> (function [] -> exit 0 | _ -> exit 1)
