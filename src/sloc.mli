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

type slocinfo = CilMisc.srcinfo list

type t

module SlocSet: Set.S with type elt = t
module SlocMap: Misc.EMapType with type key = t

val none           : t
val canonical      : t -> t
(* val fresh_abstract : unit -> t *)
val fresh_abstract : slocinfo -> t

val fresh_concrete : t -> t
val is_abstract    : t -> bool
val compare        : t -> t -> int
val eq             : t -> t -> bool
val to_slocinfo    : t -> slocinfo
val to_string      : t -> string
val d_sloc         : unit -> t -> Pretty.doc
val d_sloc_info    : unit -> t -> Pretty.doc
val d_slocmap      : (unit -> 'a -> Pretty.doc) -> unit -> 'a SlocMap.t -> Pretty.doc
val d_slocset      : unit -> SlocSet.t -> Pretty.doc
val slm_bindings   : 'a SlocMap.t -> (t * 'a) list

type sloc = t

module Subst : sig
  type t = (sloc * sloc) list

  val empty        : t
  val apply        : t -> sloc -> sloc
  val extend       : sloc -> sloc -> t -> t
  val compose      : t -> t -> t
  val slocs        : t -> sloc list
  val avoid        : t -> sloc list -> t
  val d_subst      : unit -> t -> Pretty.doc
end
