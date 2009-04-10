(*
 * Copyright © 2009 The Regents of the University of California. All rights reserved. 
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

(**
 * This module implements a DAG representation for expressions and 
 * predicates: each sub-predicate or sub-expression is paired with
 * a unique int ID, which enables constant time hashing. 
 * However, one must take care when using DAGS:
 * (1) they can only be constructed using the appropriate functions
 * (2) when destructed via pattern-matching, one must discard the ID
 *)

(**********************************************************)
(********************** Base Logic  ***********************)
(**********************************************************)

module Symbol : 
  sig 
    type t = string
    module Map : Map.S with type key = t

    val to_string : t -> string 
    val print : Format.formatter -> t -> unit 
  end

module Constant :
  sig
    type t = Int of int		
    val to_string : t -> string
    val print : Format.formatter -> t -> unit
  end

module Sort :
  sig 
    type t = Int | Bool | Array of t * t | Unint of string | Func of t list
    val to_string : t -> string
    val print : Format.formatter -> t -> unit
  end

type tag  (* externally opaque *)

type brel = Eq | Ne | Gt | Ge | Lt | Le 

type bop  = Plus | Minus | Times | Div
    
type expr = expr_int * tag 
    
and expr_int =
  | Con of Constant.t
  | Var of Symbol.t
  | App of Symbol.t * expr list
  | Bin of expr * bop * expr  
  | Ite of pred * expr * expr
  | Fld of Symbol.t * expr             (* NOTE: Fld (s, e) == App ("field"^s,[e]) *) 
      
and pred = pred_int * tag

and pred_int =
  | True
  | False
  | Atom of expr * brel * expr 
  | And  of pred list
  | Or   of pred list
  | Not  of pred
  | Imp  of pred * pred
  | Bexp of expr
  | Forall of ((Symbol.t * Sort.t) list) * pred

(** HERE Wrap and unwrap -- should not be exposed 
val ewr : expr_int -> expr
val euw : expr -> expr_int
val pwr : pred_int -> pred
val puw : pred -> pred_int
*)

(* Constructors : expressions *)
val eCon : Constant.t -> expr
val eVar : Symbol.t -> expr
val eApp : Symbol.t * expr list -> expr
val eBin : expr * bop * expr -> expr 
val eIte : pred * expr * expr -> expr
val eFld : Symbol.t * expr -> expr

(* Constructors : predicates *)
val pTrue  : pred
val pFalse : pred
val pAtom  : expr * brel * expr -> pred
val pAnd   : pred list -> pred
val pOr    : pred list -> pred
val pNot   : pred -> pred
val pImp   : (pred * pred) -> pred
val pBexp  : expr -> pred
val pForall: ((Symbol.t * Sort. t) list) * pred -> pred

(* Misc *)
val print_stats : unit -> unit

module Expression : 
sig
  type t (* = expr *)
  module Hash : Hashtbl.S with type key = t
  
  val print     : Format.formatter -> t -> unit
  val show      : t -> unit
  val to_string : t -> string
  
  val support   : t -> Symbol.t list
  val subst     : t * Symbol.t * t -> t 
  val map       : (t -> t) -> t -> t 
end
  
module Predicate :
sig
  type t (* = pred *)
  module Hash : Hashtbl.S with type key = t 
 
  val print     : Format.formatter -> t -> unit
  val show      : t -> unit
  val to_string : t -> string
  
  val support   : t -> Symbol.t list
  val subst     : t * Symbol.t * t -> t 
  val map       : (t -> t) -> t -> t 
  
  val elim_ite  : t -> t 
  val size      : t -> int
end
