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

module Sl = Sloc
module FC = FixConstraint
module SSS = Sloc.SlocSloc

type refVar   = string

type intr        = Sloc.t * Sloc.t

type 'a def      = { loc_params : Sloc.t list 
                   ; ref_params :     'a list 
                   ; unfolds    :   intr list
                   ; rhs        : 'a Ctypes.prestore
                   }

type ref_def  = FixConstraint.reft def
type var_def  = refVar def

module HfMap = FixMisc.StringMap
type   env   = var_def HfMap.t

let intr_is_conc (l1, l2) = Sloc.is_abstract l1 && Sloc.is_concrete l2
let intr_is_abs  (l1, l2) = Sloc.is_abstract l1 && Sloc.is_abstract l2

let test_env    = HfMap.add "list" def_of_list HfMap.empty

let def_of_list = ()

let d_ref_fapp = ()
let d_ref_def  = ()



