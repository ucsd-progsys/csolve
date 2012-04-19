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

type fname    = string
type refVar   = string

type 'a fapp  =
  fname * loc_param list * 'a list

type ref_fapp = FixConstraint.reft fapp

and loc_param = Sloc.t 

and 'a def    =
  loc_formal list * 'a list * 'a def_rhs

and loc_formal   = Sloc.t
and 'a def_rhs   = intr * ('a Ctypes.prestore)
and intr         = Sloc.t * Sloc.t

type ref_def  = FixConstraint.reft def
type var_def  = refVar def

module HfMap = FixMisc.StringMap
type   env   = var_def HfMap.t

let intr_is_conc (l1, l2) = Sloc.is_abstract l1 and Sloc.is_concrete l2
let intr_is_abs  (l1, l2) = Sloc.is_abstract l1 and Sloc.is_abstract l2

let test_env    = HfMap.add "list" def_of_list HfMap.empty

let d_ref_fapp = ()
let d_ref_def  = ()



