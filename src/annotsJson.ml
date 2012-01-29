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


(* Data Types and Printers for the JSON representation of Csolve results *)
module Co = Constants
open FixMisc.Ops

type qdef  = string
type pred  = string 
type expr  = string
type act   = string


type error = 
  { line : int 
  }

type qual  =
  { name : string
  ; args : expr list
  ; url  : act
  }

type annot = 
  { quals : qual list
  ; conc  : pred 
  }

type json = 
  { errors   : error list
  ; qualDef  : qdef FixMisc.StringMap.t
  ; genAnnot : annot
  ; annot    : (annot FixMisc.IntMap.t) FixMisc.StringMap.t 
  } 

(*******************************************************************)
(************* Convert Bindings to JSON ****************************)
(*******************************************************************)

let bindsToJson qs binds so = failwith "TODO"

(*******************************************************************)
(************* Render JSON as Pretty.doc ***************************)
(*******************************************************************)

let d_json () (js: json) : Pretty.doc = failwith "TODO"

(*******************************************************************)
(************* API *************************************************)
(*******************************************************************)

(* API *)
let dump_annots qs binds so : unit =
  let f = !Co.csolve_file_prefix^".json" in
  let d = d_json () <| bindsToJson qs binds so in
  FixMisc.with_out_file f (fun oc -> Pretty.fprint ~width:80 oc d)



