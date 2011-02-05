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

(* val stripcasts_of_expr: Cil.exp  -> Cil.exp
   val stripcasts_of_lval: Cil.lval -> Cil.lval *)
val purify           : Cil.file -> unit
val unfloat          : Cil.file -> unit
val is_pure_expr     : Cil.exp -> bool 
val is_local_expr    : Cil.exp -> bool
val is_null_expr     : Cil.exp -> bool

val doc_of_formatter : (Format.formatter -> 'a -> unit) -> 'a -> Pretty.doc

val bytesSizeOf      : Cil.typ -> int
val bytesSizeOfFloat : Cil.fkind -> int
val bytesOffset      : Cil.typ -> Cil.offset -> int
val ptrRefType       : Cil.typ -> Cil.typ
val isVararg         : Cil.typ -> bool
val typ_width        : Cil.typ -> int
val int_width        : int
val short_width      : int
val char_width       : int

val has_array_attr     : Cil.attributes -> bool
val has_pos_attr       : Cil.attributes -> bool
val has_unchecked_attr : Cil.attributes -> bool

val is_unchecked_ptr_type : Cil.typ -> bool

val bprintf : bool -> ('a, unit, Pretty.doc) format -> 'a

val definedHere : Cil.varinfo -> bool

val d_var       : unit -> Cil.varinfo -> Pretty.doc

type dec =
  | FunDec of string * Cil.location
  | VarDec of Cil.varinfo * Cil.location * Cil.init option

module VarMap: Map.S with type key = Cil.varinfo
module VarMapPrinter: sig
  val d_map : ?dmaplet:(Pretty.doc -> Pretty.doc -> Pretty.doc) ->
    string ->
    (unit -> VarMap.key -> Pretty.doc) ->
    (unit -> 'a -> Pretty.doc) -> unit -> 'a VarMap.t -> Pretty.doc
end
val assertLoc : Cil.location -> bool -> ('a, unit, Pretty.doc) format -> 'a
val vm_print_keys : unit -> 'a VarMap.t -> Pretty.doc
val vm_of_list    : (Cil.varinfo * 'a) list -> 'a VarMap.t
val vm_to_list    : 'a VarMap.t -> (Cil.varinfo * 'a) list
val vm_union      : 'a VarMap.t -> 'a VarMap.t -> 'a VarMap.t
(*
val sccs : Cil.file -> Cil.varinfo list list 
val reach: Cil.file -> Cil.varinfo -> Cil.varinfo list
*)
val reachable: Cil.file -> string -> bool

val iterExprs: Cil.file -> (Cil.exp -> bool) -> unit
(*
val iterConsts: Cil.file -> (Cil.constant -> unit) -> unit 
*)
val iterDefVars: Cil.file -> (Cil.varinfo -> unit) -> unit
val iterUsedVars: Cil.file -> (Cil.varinfo -> unit) -> unit
val iterExprVars: Cil.exp -> (Cil.varinfo -> unit) -> unit 

val g_error:    bool -> ('a, unit, Pretty.doc) format -> 'a
val g_errorLoc: bool -> Cil.location -> ('a, unit, Pretty.doc) format -> 'a
val g_halt:     bool -> 'a -> unit

val is_fun: Cil.varinfo    -> bool
val is_scalar: Cil.varinfo -> bool
val is_reference: Cil.typ  ->bool

module type Summarizer =
sig
  type summary =
    {has_prop: bool; metric: int}
  val build_summary: Cil.file -> summary
end

module FunPtrDetector : Summarizer

module type Visitor =
sig
  val doVisit: Cil.file -> unit 
end

module CopyGlobal: Visitor
module NameNullPtrs: Visitor
module Pheapify: Visitor


