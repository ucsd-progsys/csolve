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
module P = Pretty
module M = FixMisc
module S = Sloc
  
open M.Ops

type t = SVar of int
    
let d_svar () (SVar n) = P.text <| "h" ^ string_of_int n
    
let compare (SVar n) (SVar m) = compare n m
    
let (fresh_svar, reset_fresh_svar) = 
  let (fresh_svar_id, reset_fresh_svar_ids) = M.mk_int_factory ()
  in (fresh_svar_id <+> (fun i -> SVar i), reset_fresh_svar_ids) 
  
type svar = t
    
module ComparableSvar =
  struct
    type t = svar
    let compare = compare
    let print = CilMisc.pretty_to_format d_svar
  end
  
module SvarMap =
  M.EMap (ComparableSvar)
    
module SvarSet = 
  M.ESet (ComparableSvar)
    
module SSP = P.MakeSetPrinter(SvarSet)

let d_svarset () ss =
  SSP.d_set ", " d_svar () ss

let d_smap d_a () m = m
                   |> SvarMap.to_list
                   |> P.dprintf "[@[%a@]]"
                       (P.d_list ", " (fun () (v, a) -> P.dprintf "%a -> %a" d_svar v d_a a))
  
