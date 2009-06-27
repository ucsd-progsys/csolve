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


module  F = Format
module  C = FixConstraint
module YM = Ast.Symbol.SMap
module SM = Misc.StringMap

open Misc.Ops
open Cil

(****************************************************************)
(********************** Constraint Indexing *********************)
(****************************************************************)

type t = {
  scim : Ssa_transform.ssaCfgInfo SM.t;
  wfm  : C.wf list SM.t;
  cm   : C.t list SM.t;
}

(* API *)
let create ws cs = 
  { scim = SM.empty;
    wfm  = SM.empty |> SM.add Constants.global_name ws;
    cm   = SM.empty |> SM.add Constants.global_name cs }

(* API *)
let add me fn sci wfs cs =
  { scim = SM.add fn sci me.scim ;
    wfm  = SM.add fn wfs me.wfm ;
    cm   = SM.add fn cs  me.cm ;
  }

let find me fn = 
  (SM.find fn me.scim, 
   SM.find fn me.wfm,
   SM.find fn me.cm)

let iter me f = 
  SM.iter (fun fn _ -> f fn (find me fn)) me.scim

(* DOG SLOW *)
let env_of_ws ws =
  let bs = 
  ws |> Misc.map C.env_of_wf 
     |> Misc.flap C.bindings_of_env in
  let _ = Printf.printf "env_of_ws: BOO |ws| = %d, |bs| = %d \n" (List.length ws)
  (List.length bs) in 
   C.env_of_bindings bs  
(* API *)
let get_wfs = fun me -> SM.fold (fun _ wfs acc -> wfs ++ acc) me.wfm []

(* API *)
let get_cs  = fun me -> SM.fold (fun _ cs acc -> cs ++ acc) me.cm []

(* API *)
let print so ppf me = 
  match so with 
  | None -> (* print constraints *) 
      iter me begin 
        fun fn (_, wfs, cs) ->
          F.fprintf ppf "Ref-Constraints for %s \n %a" 
          fn (Misc.pprint_many true "\n" (C.print_t None)) cs;
          F.fprintf ppf "WF-Constraints for %s \n %a"
          fn (Misc.pprint_many true "\n" (C.print_wf None)) wfs
      end
  | Some _ -> (* print solution *)
      get_wfs me 
      |> env_of_ws 
      |> F.printf "Liquid Types: \n%a" (C.print_env so)


