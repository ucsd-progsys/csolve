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

val referenced_var_of_exp : Cil.exp -> Cil.varinfo

val fresh_arg_name   : unit -> string
val purify           : Cil.file -> unit
val unfloat          : Cil.file -> unit

type considerStringsPure =
  | StringsArePure
  | StringsAreNotPure

val is_pure_expr     : considerStringsPure -> Cil.exp -> bool
val is_null_expr     : Cil.exp -> bool

val d_formatter      : (Format.formatter -> 'a -> unit) -> unit -> 'a -> Pretty.doc
val doc_of_formatter : (Format.formatter -> 'a -> unit) -> 'a -> Pretty.doc
val pretty_to_string : (unit -> 'a -> Pretty.doc) -> 'a -> string  
val pretty_to_format : (unit -> 'a -> Pretty.doc) -> (Format.formatter -> 'a -> unit)

val concat_docs      : Pretty.doc list -> Pretty.doc
val d_many_parens    : bool -> (unit -> 'a -> Pretty.doc) -> unit -> 'a list -> Pretty.doc
val d_many_braces    : bool -> (unit -> 'a -> Pretty.doc) -> unit -> 'a list -> Pretty.doc
val d_many_brackets  : bool -> (unit -> 'a -> Pretty.doc) -> unit -> 'a list -> Pretty.doc
val d_opt            : (unit -> 'a -> Pretty.doc) -> unit -> 'a option -> Pretty.doc
val d_pair           : (unit -> 'a -> Pretty.doc) -> (unit -> 'b -> Pretty.doc) -> unit -> ('a * 'b)
-> Pretty.doc

val bytesSizeOf      : Cil.typ -> int
val bytesSizeOfFloat : Cil.fkind -> int
val bytesOffset      : Cil.typ -> Cil.offset -> int
val ptrRefType       : Cil.typ -> Cil.typ
val isVararg         : Cil.typ -> bool
val isCompoundType   : Cil.typ -> bool
val typeName         : Cil.typ -> string option
val typ_width        : Cil.typ -> int
val int_width        : int
val short_width      : int
val char_width       : int

val getAttr            : string -> Cil.attributes -> Cil.attribute
val getStringAttrs     : string -> Cil.attributes -> string list
val setStringAttr      : string -> string -> Cil.attributes -> Cil.attributes

val arrayAttribute          : string
val singleAttribute         : string
val finalAttribute          : string
val slocAttribute           : string
val globalAttribute         : string
val instantiateAttribute    : string
val predAttribute           : string
val roomForAttribute        : string
val nonnullRoomForAttribute : string
val externOkAttribute       : string
val checkTypeAttribute      : string
val layoutAttribute         : string
val ignoreIndexAttribute    : string
val useIndexAttribute       : string
val ignoreBoundAttribute    : string
val effectAttribute         : string
val anyRefAttribute         : string
val anyTypeAttribute        : string
val typeVarAttribute        : string
val instantiateTypeVarAttribute : string

val hasRoomAttribute        : string
val nonnullHasRoomAttribute : string

val has_array_attr          : Cil.attributes -> bool
val has_pos_attr            : Cil.attributes -> bool
val has_unchecked_attr      : Cil.attributes -> bool

val is_cobegin_block        : Cil.stmt -> bool
val is_foreach_block        : Cil.stmt -> bool
val is_foreach_iter_block   : Cil.stmt -> bool
val is_foreach_iter_block   : Cil.stmt -> bool
val block_has_fresh_effects : Cil.stmt -> bool
val coroutines_of_block     : Cil.stmt -> int list
val index_var_of_foreach    : Cil.stmt -> Cil.varinfo

val is_unchecked_ptr_type : Cil.typ -> bool

val bprintf : bool -> ('a, unit, Pretty.doc) format -> 'a

val definedHere : Cil.varinfo -> bool

val d_var                   : unit -> Cil.varinfo -> Pretty.doc

(* val d_type_noattrs          : unit -> Cil.typ -> Pretty.doc
 *)

type dec =
  | FunDec of string * Cil.fundec * Cil.location
  | VarDec of Cil.varinfo * Cil.location * Cil.init option

module VarSet: FixMisc.ESetType with type elt = Cil.varinfo
module VarMap: FixMisc.EMapType with type key = Cil.varinfo

module VarMapPrinter: sig
  val d_map : ?dmaplet:(Pretty.doc -> Pretty.doc -> Pretty.doc) ->
    string ->
    (unit -> VarMap.key -> Pretty.doc) ->
    (unit -> 'a -> Pretty.doc) -> unit -> 'a VarMap.t -> Pretty.doc
end
val assertLoc : Cil.location -> bool -> ('a, unit, Pretty.doc) format -> 'a

val vm_print_keys : unit -> 'a VarMap.t -> Pretty.doc
(* val vm_of_list    : (Cil.varinfo * 'a) list -> 'a VarMap.t
   val vm_to_list    : 'a VarMap.t -> (Cil.varinfo * 'a) list
   val vm_union      : 'a VarMap.t -> 'a VarMap.t -> 'a VarMap.t
*)
(*
val sccs : Cil.file -> Cil.varinfo list list 
val reach: Cil.file -> Cil.varinfo -> Cil.varinfo list
*)
val reachable     : Cil.file -> string -> bool
val source_files  : Cil.file -> string list

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
val is_funptr: Cil.varinfo    -> bool
val is_scalar: Cil.varinfo -> bool
val is_reference: Cil.typ  ->bool



val is_pure_function    : string -> bool
val is_cil_tempvar      : string -> bool
val rename_local        : string -> string -> string
val unrename_local      : (* string -> *) string -> string
val top_level_fn_assgns : Cil.fundec -> VarSet.t

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

val dec_of_global : Cil.global -> dec option

val noBlockAttrPrinter : Cil.cilPrinter

val typStripAttrs  : Cil.typ  -> Cil.typ
val exprStripAttrs : Cil.exp  -> Cil.exp
(* val varExprMap     : Cil.file -> Cil.exp VarMap.t *)

val varExprMap     : Cil.fundec list -> Cil.exp VarMap.t

val reSugar_lval  : Cil.exp VarMap.t -> Cil.lval  -> Cil.lval
val reSugar_exp   : Cil.exp VarMap.t -> Cil.exp   -> Cil.exp
val reSugar_instr : Cil.exp VarMap.t -> Cil.instr -> Cil.instr

(****************** Preserving Source Maps *************************)

type srcinfo

val d_srcinfo         : unit -> srcinfo -> Pretty.doc

val srcinfo_of_lval     : Cil.lval -> Cil.location option -> srcinfo
val srcinfo_of_type     : Cil.typ  -> Cil.location option -> srcinfo
val srcinfo_of_constant : Cil.constant -> Cil.location option -> srcinfo
val srcinfo_of_var      : Cil.varinfo -> Cil.location option -> srcinfo
val srcinfo_of_instr    : Cil.instr -> Cil.location option -> srcinfo
val srcinfo_of_string   : string -> srcinfo

val typ_of_srcinfos     : srcinfo list -> Cil.typ option
val exp_of_srcinfos     : srcinfo list -> Cil.exp option
val lval_of_srcinfos    : srcinfo list -> Cil.lval option

val setSrcLval          : Cil.location -> Cil.lval -> Cil.lval -> unit
val setSrcExpr          : Cil.location -> Cil.exp -> Cil.exp -> unit
val getSrcLval          : Cil.location -> Cil.lval -> Cil.lval option
val getSrcExpr          : Cil.location -> Cil.exp -> Cil.exp option

