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

type slocid = int
type slocinfo = CilMisc.srcinfo list

type t =
  | Abstract of slocid (* * slocinfo *) 
  | Concrete of slocid * (* abstract counterpart *) slocid (* * slocinfo *)
  | AnyLoc 

let slocinfot : (int, slocinfo) Hashtbl.t = Hashtbl.create 39
let slocinfo i = try Hashtbl.find slocinfot i with Not_found -> assertf "Unknown Sloc %d" i

let to_slocinfo = function
  | Abstract i       
  | Concrete (i,_) -> slocinfo i 
  | AnyLoc         -> []

let to_ciltyp = to_slocinfo <+> CilMisc.typ_of_srcinfos

let to_string = function
  | Abstract (lid)    -> "A" ^ string_of_int lid
  | Concrete (lid, _) -> "C" ^ string_of_int lid
  | AnyLoc            -> "ANY"

let d_sloc () = function
  | Abstract lid        -> P.text <| "A" ^ string_of_int lid
  | Concrete (lid, aid) -> P.text <| "C" ^ string_of_int lid ^ "[A" ^ string_of_int aid ^ "]"
  | AnyLoc              -> P.text <| "ANY"

let d_slocinfo () z =
  CilMisc.d_many_brackets false CilMisc.d_srcinfo () z 

let d_sloc_info () x = 
  P.dprintf "%a %a" d_sloc x d_slocinfo (to_slocinfo x)

let fresh_slocid = 
  let (fresh, _) = M.mk_int_factory () in
  begin fun z -> 
    fresh () 
    >> (fun i -> Hashtbl.add slocinfot i z)
    (* >> (fun i -> ignore <| print_now (Printf.sprintf "fresh_slocid: %d --> %s\n" i (CilMisc.pretty_to_string d_slocinfo z))) *)
  end

let sloc_of_any = AnyLoc

let refresh = function
  | Abstract i        -> Abstract (fresh_slocid (slocinfo i))
  | Concrete (i, ida) -> Concrete (fresh_slocid (slocinfo i), ida)
  | AnyLoc            -> AnyLoc

(*
let refresh = function
  | Abstract (_, i)      -> Abstract (fresh_slocid (), i)
  | Concrete (_, ida, i) -> Concrete (fresh_slocid (), ida, i)
  | AnyLoc               -> AnyLoc
*)

let canonical = function
  | Abstract _ as al  -> al
  | Concrete (_, aid) -> Abstract aid
  | AnyLoc            -> AnyLoc

(*
let strip_info = function
  | Abstract (x,_)   -> Abstract (x, [])
  | Concrete (x,y,_) -> Concrete (x, y, [])
  | AnyLoc           -> AnyLoc

*)

let compare l1 l2 = compare l1 l2 (* (strip_info l1) (strip_info l2) *)

let eq l1 l2 = compare l1 l2 = 0

let is_abstract = function
  | Abstract _ -> true
  | Concrete _ -> false
  | AnyLoc     -> false

let is_concrete = function
  | Abstract _ -> false
  | Concrete _ -> true
  | AnyLoc     -> false

let is_any      = function
  | AnyLoc     -> true
  | Abstract _ -> false
  | Concrete _ -> false

(* API *)
let fresh_abstract i = Abstract (fresh_slocid [i])

(* API *)
let copy_concrete = function
  | AnyLoc     -> AnyLoc
  | Abstract i -> Concrete (fresh_slocid (slocinfo i), i)
  | _          -> assert false

(* API *)
let copy_abstract z' = function
  | AnyLoc          -> AnyLoc
  | Abstract i   
  | Concrete (i, _) -> Abstract (fresh_slocid (z' ++ (slocinfo i)))

(* API *)
let abstract_of_int i n = 
  Hashtbl.add slocinfot n [i]; Abstract n

let none = fresh_abstract (CilMisc.srcinfo_of_string "none")

(******************************************************************************)
(******************************* Maps Over Slocs ******************************)
(******************************************************************************)

(* Avoiding cyclic types when making SlocSet and SlocMap *)
type sloc = t

module ComparableSloc =
  struct
    type t = sloc
    let compare = compare
    let print = CilMisc.pretty_to_format d_sloc
  end

module SlocSet = Set.Make(ComparableSloc)

module SlocMap = M.EMap (ComparableSloc)

let slm_bindings = fun conc -> SlocMap.fold (fun k v acc -> (k, v) :: acc) conc []

module SMP = P.MakeMapPrinter(SlocMap)

let d_slocmap d_value () slm =
  SMP.d_map "; " d_sloc d_value () slm

module SSP = P.MakeSetPrinter(SlocSet)

let d_slocset () ss =
  SSP.d_set ", " d_sloc () ss

(******************************************************************************)
(***************************** Slocs Substitutions ****************************)
(******************************************************************************)

module SlocElt = struct
  type t = sloc
  let refresh  = refresh
  let d_t = d_sloc
end

module Subst = struct 
  include Substitution.Make(SlocElt)
  let slocs = es
end
(* module Subst = struct *)
(*   type t = (sloc * sloc) list *)

(*   let empty = [] *)

(*   let d_subst (): t -> P.doc = *)
(*     P.dprintf "[@[%a@]]" (P.d_list ", " (fun () (sfrom, sto) -> P.dprintf "%a -> %a" d_sloc sfrom d_sloc sto)) *)

(*   let apply (sub: t) (s: sloc): sloc = *)
(*     try List.assoc s sub with Not_found -> s *)

(*   let extend (sfrom: sloc) (sto: sloc) (sub: t): t = *)
(*     let sub = List.map (apply [(sfrom, sto)] |> M.app_snd) sub in *)
(*       if not (List.mem_assoc sfrom sub) then *)
(*         (sfrom, sto) :: sub *)
(*       else *)
(*         sub *)

(*   let compose (sub1: t) (sub2: t): t = *)
(*     let sub = List.map (apply sub1 |> M.app_snd) sub2 in *)
(*       List.fold_left begin fun sub (sfrom, sto) -> *)
(*         if not (List.mem_assoc sfrom sub2) then *)
(*           (sfrom, sto) :: sub *)
(*         else *)
(*           sub *)
(*       end sub sub1 *)

(*   let avoid sub ls = *)
(*        sub *)
(*     |> List.filter (fst <+> M.flip List.mem ls) *)
(*     |> List.map (fun (f, t) -> (f, if List.mem t ls then refresh t else t)) *)

(*   let slocs (sub: t): sloc list = *)
(*     List.split sub |> M.uncurry (@) |> M.sort_and_compact *)
(* end *)
