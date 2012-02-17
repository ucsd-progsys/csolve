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
module Co  = Constants
module PP  = Pretty
module A   = Ast
module Sy  = A.Symbol
module SM  = Sy.SMap
module SSM = Misc.StringMap
module IM  = Misc.IntMap
module C   = FixConstraint
module Q   = Qualifier

open Misc.Ops

type qdef  = string
type pred  = string 
type expr  = string
type act   = string
type ctyp  = string

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
  { ctype : ctyp
  ; quals : qual list
  ; conc  : pred list 
  }

type json = 
  { errors   : error list
  ; qualDef  : qdef SSM.t
  ; genAnnot : annot
  ; annot    : (annot IM.t) SSM.t 
  } 

let junkUrl = ""

(*******************************************************************)
(************* Render JSON as Pretty.doc ***************************)
(*******************************************************************)

let d_str () s       = PP.dprintf "\"%s\"" s 
let d_many f () xs   = PP.d_list ", " f () xs
let d_kv f () (k, v) = PP.dprintf "%a : %a" d_str k f v


(***************** Serializing Maps and Arrays **************************)

let d_kvs f () kvs   = PP.dprintf "{ @[%a@] }" (d_many (d_kv f)) kvs 
let d_array f ()     = PP.dprintf "[ @[%a@] ]" (d_many f) 
let d_sm f ()        = SSM.to_list <+> d_kvs f ()
let d_im f ()        = IM.to_list <+> Misc.map (Misc.app_fst string_of_int) <+> d_kvs f ()

(***************** Serializing String Aliases ***************************)

let d_qdef  = d_str 
let d_expr  = d_str 
let d_act   = d_str
let d_pred  = d_str

(***************** Serializing Specialized Records **********************) 

let d_error () e = 
  PP.dprintf "{ line : %d }" e.line

let d_qual () q =
  PP.dprintf 
  "{ name : %a, 
     args : @[%a@], 
     url  : @[%a@] 
   }" 
    d_str q.name 
    (d_array d_expr) q.args
    d_act q.url

let d_annot () a =
  PP.dprintf 
  "{ quals : @[%a@]
   , conc  : @[%a@] 
   }"
    (d_array d_qual) a.quals
    (d_array d_pred) a.conc

let d_json () x = 
  PP.dprintf 
  "{ errors   : @[%a@]
   , qualDef  : @[%a@]
   , genAnnot : @[%a@]
   , annot    : @[%a@] 
   }"
    (d_array d_error)     x.errors
    (d_sm d_qdef)         x.qualDef
    d_annot               x.genAnnot
    (d_sm (d_im d_annot)) x.annot


(*******************************************************************)
(************* Build Map from var-line -> ssavar *******************)
(*******************************************************************)

let mkCtype = CilMisc.pretty_to_string CilMisc.d_type_noattrs
let mkPred  = Ast.Predicate.to_string
let mkQual  = fun (f,es) -> { name = Sy.to_string f
                            ; args = List.map A.Expression.to_string es
                            ; url = junkUrl }

let deconstruct_pApp = function
  | A.Bexp (A.App (f, es), _), _ -> Some (f, es)
  | _                            -> None

let annot_of_vinfo s ((cr : Ctypes.refctype), (ct : Cil.typ)) : annot =
  let cs, ks = cr |> Ctypes.reft_of_refctype 
                  |> thd3
                  |> Misc.either_partition (function C.Conc p -> Left p | C.Kvar (su,k) -> Right (su, k)) in
  let qs, cs'= ks |> Misc.flap (fun (su, k) -> (s k) |> List.map (Misc.flip A.substs_pred su))
                  |> Misc.either_partition (fun p -> match deconstruct_pApp p with Some z -> Left z | _ -> Right p) in
  { ctype = mkCtype ct 
  ; quals = List.map mkQual qs 
  ; conc  = List.map mkPred (cs ++ cs')      
  }

let mkVarLineSsavarMap bs : (Sy.t IM.t) SSM.t =
  let get x m     = if SSM.mem x m then (SSM.find x m) else IM.empty in 
  let put x n y m = SSM.add x (IM.add n y (get x m)) m               in
  bs |> Misc.flap begin function 
          | Annots.TSSA (fn, t) -> 
             t |>  Misc.hashtbl_to_list 
               |>: Misc.app_fst (Misc.app_fst3 (CilMisc.unrename_local fn))
          | _ -> []
        end
     |> List.fold_left begin fun m ((x, file, line), xssa) ->
          put x line (FixAstInterface.name_of_string xssa) m 
        end SSM.empty

let mkSyInfoMap bs =
  bs |> Misc.map_partial (function Annots.TVar (x, y) -> Some (x, y) | _ -> None)
     |> SM.of_list

(*******************************************************************)
(************* Convert Bindings to JSON ****************************)
(*******************************************************************)

let error_of_constraint tgr c = 
  c |> FixConstraint.tag_of_t 
    |> CilTag.t_of_tag
    |> CilTag.loc_of_t tgr
    |> (fun l -> { line = l.Cil.line; file = l.Cil.file })

let mkErrors tgr = 
  List.map (error_of_constraint tgr)

let mkQualdef q = 
  let qn = Sy.to_string <| Q.name_of_t q in
  let qd = A.Predicate.to_string  <| Q.pred_of_t q in
  (qn, qd)

let mkQualdefm qs =
  SSM.of_list <| List.map mkQualdef qs

  (*
let mkAnnot s bs =
  bs |> mkVarLineReftMap
     |> SSM.map (IM.map (annot_of_reft s))
 *)

let mkAnnot s bs =
  let xm = mkSyInfoMap bs in
  bs |> mkVarLineSsavarMap
     |> SSM.map (IM.map_partial (fun x ->
          Misc.maybe_map (annot_of_vinfo s) (SM.maybe_find x xm)
        ))


let bindsToJson qs tgr s' cs' binds =
  { errors   = mkErrors tgr cs'
  ; qualDef  = mkQualdefm qs
  ; genAnnot = { ctype = "(void *)"; quals = []; conc = [] }
  ; annot    = mkAnnot s' binds
  }

(*******************************************************************)
(************* API *************************************************)
(*******************************************************************)

(* API *)
let dump_annots qs tgr s' cs' binds : unit =
  let f = !Co.csolve_file_prefix^".json" in
  let d = d_json () <| bindsToJson qs tgr s' cs' binds in
  Misc.with_out_file f (fun oc -> Pretty.fprint ~width:80 oc d)

