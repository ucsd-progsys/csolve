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
module M = Misc

open Misc.Ops

type slocid = int

type t =
  | Abstract of slocid
  | Concrete of slocid * (* abstract counterpart *) slocid

let (fresh_slocid, reset_fresh_slocid) = Misc.mk_int_factory ()

let refresh = function
  | Abstract _        -> Abstract (fresh_slocid ())
  | Concrete (_, ida) -> Concrete (fresh_slocid (), ida)

let fresh_abstract () = 
  Abstract (fresh_slocid ())

let fresh_concrete abs =
  Concrete (fresh_slocid (), match abs with Abstract aid -> aid | _ -> assert false)

let none = Abstract (-1)

let canonical = function
  | Abstract _ as al  -> al
  | Concrete (_, aid) -> Abstract aid

let compare = compare

let eq l1 l2 = compare l1 l2 = 0

let is_abstract = function
  | Abstract _ -> true
  | Concrete _ -> false

let to_string = function
  | Abstract lid      -> "A" ^ string_of_int lid
  | Concrete (lid, _) -> "C" ^ string_of_int lid

let d_sloc () = function
  | Abstract lid        -> P.text <| "A" ^ string_of_int lid
  | Concrete (lid, aid) -> P.text <| "C" ^ string_of_int lid ^ "[A" ^ string_of_int aid ^ "]"

(******************************************************************************)
(******************************* Maps Over Slocs ******************************)
(******************************************************************************)

(* Avoiding cyclic types when making SlocSet and SlocMap *)
type sloc = t

module ComparableSloc =
  struct
    type t = sloc
    let compare = compare
  end

module SlocSet = Set.Make(ComparableSloc)

module SlocMap = Map.Make(ComparableSloc)

let slm_bindings = fun conc -> SlocMap.fold (fun k v acc -> (k,v)::acc) conc []

module SMP = P.MakeMapPrinter(SlocMap)

let d_slocmap d_value () slm =
  SMP.d_map "; " d_sloc d_value () slm

module SSP = P.MakeSetPrinter(SlocSet)

let d_slocset () ss =
  SSP.d_set ", " d_sloc () ss

(******************************************************************************)
(***************************** Slocs Substitutions ****************************)
(******************************************************************************)

module Subst = struct
  type t = (sloc * sloc) list

  let empty = []

  let d_subst (): t -> P.doc =
    P.dprintf "[@[%a@]]" (P.d_list ", " (fun () (sfrom, sto) -> P.dprintf "%a -> %a" d_sloc sfrom d_sloc sto))

  let apply (sub: t) (s: sloc): sloc =
    try List.assoc s sub with Not_found -> s

  let extend (sfrom: sloc) (sto: sloc) (sub: t): t =
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
    |> List.map (fun (f, t) -> (f, if List.mem t ls then refresh t else t))

  let slocs (sub: t): sloc list =
    List.split sub |> M.uncurry (@) |> M.sort_and_compact
end
