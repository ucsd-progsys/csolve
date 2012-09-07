
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

type intrs = Sloc.t list

type def

type env  = def FixMisc.StringMap.t

val flocs_of : def -> Sloc.t list
val frefs_of : def -> Ctypes.VarRefinement.t list
val unfs_of  : def -> intrs list
val rhs_of   : def -> Ctypes.VarRefinement.t Ctypes.prestore

val def_of_intlist : def
val test_env : env

val fresh_unfs_of_hf : Sloc.t -> string -> env -> intrs list

val fold_hf_on_sto   : Ctypes.hf_appl  ->
                       intrs list      ->
                       Ctypes.rvstore  ->
                       env             ->
                       Ctypes.refstore

val fold_hf_shapes_on_sto   : Ctypes.hf_appl ->
                              intrs list     ->
                              Ctypes.store   ->
                              env            ->
                              Ctypes.store



val gen : Ctypes.hf_appl     -> intrs list   ->
          Sloc.SlocSlocSet.t -> Ctypes.store ->
          env                -> Sloc.SlocSlocSet.t * Ctypes.store

val ins : Sloc.t -> Sloc.t list -> intrs list -> 
          Sloc.SlocSlocSet.t -> Ctypes.store -> env ->
          Sloc.SlocSlocSet.t  * Ctypes.store

val shape_in_env : string -> Sloc.t list ->
                   env    -> Ctypes.store
                   
val expand_sto_shape : env -> Ctypes.store -> Ctypes.hf_appl list * Ctypes.store

val expand_cspec_stores    : Ctypes.cspec -> env -> Ctypes.cspec

val contract_store_shapes  : Ctypes.store -> Ctypes.hf_appl list
                                        -> env -> Ctypes.store
