(*
 * Copyright ? 1990-2007 The Regents of the University of California. All rights reserved. 
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

(* $Id: misc.ml,v 1.14 2006/09/26 01:47:01 jhala Exp $
 *
 * This file is part of the SIMPLE Project.
 *)

(**
 * This module provides some miscellaneous useful helper functions.
 *)

(* Used without "Misc" prefix *)
module Ops = struct
  
  let (|>) x f = f x

  let (+=) x n = x := !x + n

  let (++) = List.rev_append 

  let id = fun x -> x

  let un = fun x -> ()

  let failure fmt = 
    Printf.ksprintf failwith fmt

  let asserts p fmt =
    Printf.ksprintf (fun x -> if not p then failwith x) fmt
  
  let pretty_string f x = 
    Pretty.dprintf "%a" f x |> Pretty.sprint ~width:80 

end

module IntMap = 
  Map.Make 
  (struct
    type t = int
    let compare i1 i2 = 
      compare i1 i2
  end)

module StringMap = 
  Map.Make 
  (struct
    type t = string 
    let compare i1 i2 = 
      compare i1 i2
  end)

open Ops

let get_option d = function  
  | Some x -> x 
  | None   -> d

let map_partial f xs =
  List.rev 
    (List.fold_left 
      (fun acc x -> 
        match f x with 
        | None   -> acc
        | Some z -> (z::acc)) [] xs)

let do_catch s f x =
  try f x with ex -> 
     (Printf.printf "%s hits exn: %s \n" s (Printexc.to_string ex); raise ex) 

let do_catch_ret s f x y = 
  try f x with ex -> 
     (Printf.printf "%s hits exn: %s \n" s (Printexc.to_string ex); y) 


let do_memo memo f args key = 
  try Hashtbl.find memo key with Not_found ->
    let rv = f args in
    let _ = Hashtbl.replace memo key rv in
    rv

let map_pair f (x1, x2) = (f x1, f x2)

let flap f xs = List.flatten (List.map f xs)

let tr_flatten xss =
  List.fold_left (fun acc xs -> List.rev_append xs acc) [] xss |>
  List.rev

let tr_flap f xs = 
  List.rev (tr_flatten (List.rev_map f xs))

let hashtbl_keys t = 
  Hashtbl.fold (fun x y l -> x::l) t []
 
let sort_and_compact ls =
  let rec _sorted_compact l = 
    match l with
	h1::h2::tl ->
	  let rest = _sorted_compact (h2::tl) in
	    if h1 = h2 then rest else h1::rest
      | tl -> tl
  in
    _sorted_compact (List.sort compare ls)   

(** repeats f: unit - > unit i times *)
let rec repeat_fn f i = 
  if i = 0 then ()
  else (f (); repeat_fn f (i-1))

let is_substring s subs = 
  let reg = Str.regexp subs in
  try ignore(Str.search_forward reg s 0); true
  with Not_found -> false

let iteri f xs =
  let rec _m i l = 
    match l with [] -> ()
    | h::t -> ((f i h);(_m (i+1) t)) in
  _m 0 xs

exception FalseException
let rec intmap_for_all f m =
  try 
    IntMap.iter (fun i v -> if not (f i v) then raise FalseException) m;
    true
  with FalseException -> false

let hashtbl_to_list t = 
   Hashtbl.fold (fun x y l -> (x,y)::l) t []

let rec clone x n = 
  if n <= 0 then [] else (x::(clone x (n-1)))

let distinct xs = 
  List.length (sort_and_compact xs) = List.length xs

let trunc i j = 
  let (ai,aj) = (abs i, abs j) in
  if aj <= ai then j else ai*j/aj 

let map_to_string f xs = 
  String.concat "," (List.map f xs)

(* [count_map xs] = fun x -> number of times x appears in xs if non-zero *)
let count_map rs =
  List.fold_left 
    (fun m r -> 
      let c = try IntMap.find r m with Not_found -> 0 in
      IntMap.add r (c+1) m)
    IntMap.empty rs

let o2s f = function
  | Some x -> "Some "^ (f x)
  | None   -> "None"

let rec fixpoint f x =
  let rec acf b x =
    let x', b' = f x in
    if b' then acf true x' else (x', b) in
  acf false x

let is_prefix p s = 
  let reg = Str.regexp p in
  Str.string_match reg s 0

let rec pprint_many brk s f ppf = function
  | []     -> ()
  | x::[]  -> Format.fprintf ppf "%a" f x
  | x::xs' -> ((if brk 
                then Format.fprintf ppf "%a %s@ " f x s 
                else Format.fprintf ppf "%a %s" f x s); 
                pprint_many brk s f ppf xs')

let pprint_str ppf s =
  F.fprintf ppf "%s" s


