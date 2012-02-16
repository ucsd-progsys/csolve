(*
 * Copyright © 1990-2011 The Regents of the University of California. All rights reserved. 
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


module Misc= FixMisc

(* Data Types and Printers for the JSON representation of Csolve results *)
module Co = Constants
module PP = Pretty
module SM = Misc.StringMap
module IM = Misc.IntMap

open Misc.Ops

type qdef  = string
type pred  = string 
type expr  = string
type act   = string

type error = 
  { line : int
  ; file : string 
  }

type qual  =
  { name : string
  ; args : expr list
  ; url  : act
  }

type annot = 
  { quals : qual list
  ; conc  : pred list 
  }

type json = 
  { errors   : error list
  ; qualDef  : qdef SM.t
  ; genAnnot : annot
  ; annot    : (annot IM.t) SM.t 
  } 

(*******************************************************************)
(************* Render JSON as Pretty.doc ***************************)
(*******************************************************************)

let d_many f () xs   = PP.seq (PP.text ", ") (f ()) xs
let d_kv f () (k, v) = PP.dprintf "%s : %a" k f v
let d_kvs f () kvs   = PP.dprintf "{ %a }" (d_many (d_kv f)) kvs 


(***************** Serializing Arrays and Maps **************************)

let d_array f () = PP.dprintf "[ %a ]" (d_many f) 
let d_sm f ()    = SM.to_list <+> d_kvs f ()
let d_im f ()    = IM.to_list <+> Misc.map (Misc.app_fst string_of_int) <+> d_kvs f ()

(***************** Serializing String Aliases ***************************)

let d_qdef () = PP.text
let d_expr () = PP.text
let d_act  () = PP.text 
let d_pred () = PP.text

(***************** Serializing Specialized Records **********************) 

let d_error () e = 
  PP.dprintf "{ line : %d }" e.line

let d_qual () q =
  PP.dprintf "{ name : %s, args : %a, url : %a }" 
    q.name 
    (d_array d_expr) q.args
    d_act q.url

let d_annot () a =
  PP.dprintf "{ quals : %a, conc : %a }"
    (d_array d_qual) a.quals
    (d_array d_pred) a.conc

let d_json () x = 
  PP.dprintf "{ errors : %a, qualDef : %a, genAnnot : %a, annot : %a }"
    (d_array d_error)     x.errors
    (d_sm d_qdef)         x.qualDef
    d_annot               x.genAnnot
    (d_sm (d_im d_annot)) x.annot

(*******************************************************************)
(************* Convert Bindings to JSON ****************************)
(*******************************************************************)

    (* HEREHEREHEREHERE
let error_of_constraint tgr c = 
  c |> FixConstraint.tag_of_t 
    |> CilTag.t_of_tag
    |> CilTag.loc_of_t tgr
    |> (fun l -> { line = l.Cil.line; file = l.Cil.file })

let mkErrors tgr = 
  List.map (error_of_constraint tgr)

let mkQualdef q = 
  failwith "TODO"
let qn = Sy.to_string <| Q.name_of_t q in
  let qd = P.to_string  <| Q.pred_of_t q in
  (qn, qd)

let mkQualdefm qs =
  SM.of_list <| List.map mkQualdef qs

let bindsToJson qs tgr s' cs' binds =
  { errors   = mkErrors tgr cs'
  ; qualDef  = mkQualdefm qs
  ; genAnnot = { quals = []; conc = [] }
  ; annot    = mkAnnot s' binds
  }
*)

let bindsToJson _ = failwith "TODO"
(*******************************************************************)
(************* API *************************************************)
(*******************************************************************)

(* API *)
let dump_annots qs tgr s' cs' binds : unit =
  let f = !Co.csolve_file_prefix^".json" in
  let d = d_json () <| bindsToJson qs tgr s' cs' binds in
  Misc.with_out_file f (fun oc -> Pretty.fprint ~width:80 oc d)

