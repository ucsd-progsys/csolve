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

open M.Ops

module type SubstType = sig
  type t
  val refresh : t -> t
  val d_t     : unit -> t -> Pretty.doc
end

module type S = sig
  type e 
  type t = (e * e) list

  val empty   : t
  val apply   : t -> e -> e
  val extend  : e -> e -> t -> t
  val compose : t -> t -> t
  val d_subst : unit -> t -> Pretty.doc
  val avoid   : t -> e list -> t
  val es      : t -> e list
end
  
module Make (E : SubstType) : S with type e = E.t = struct
  type e = E.t
  type t = (e * e) list

  let empty = []

  let d_subst (): t -> P.doc =
    P.dprintf "[@[%a@]]" (P.d_list ", " (fun () (sfrom, sto) -> 
      P.dprintf "%a -> %a" E.d_t sfrom E.d_t sto))

  let apply (sub: t) (s: e): e =
    try List.assoc s sub with Not_found -> s

  let extend (sfrom: e) (sto: e) (sub: t): t =
    let sub = List.map (apply [(sfrom, sto)] |> M.app_snd) sub in
      if not (List.mem_assoc sfrom sub) then
        (sfrom, sto) :: sub
      else
        sub

  let compose (sub1: t) (sub2: t): t =
    let sub = List.map (apply sub1 |> M.app_snd) sub2 in
      List.fold_left begin fun sub (sfrom, sto) ->
        if not (List.mem_assoc sfrom sub2) then
          (sfrom, sto) :: sub
        else
          sub
      end sub sub1

  let avoid sub ls =
       sub
    |> List.filter (fst <+> M.flip List.mem ls)
    |> List.map (fun (f, t) -> (f, if List.mem t ls then E.refresh t else t))

  let es (sub: t): e list =
    List.split sub |> M.uncurry (@) |> M.sort_and_compact
end
