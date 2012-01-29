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
module E = Errormsg

open Cil
module Misc = FixMisc open Misc.Ops

(* Assume: all guards are from "if" statements. *)
let mydebug = false

type ginfo = (exp * int option * int option) option

(****************************************************************)             
let print_ifs ifs = 
  if mydebug then 
    Array.iteri begin fun i -> function 
      | None -> 
          ()
      | Some (e,tbo,ebo) -> 
          let ts = Misc.o2s string_of_int tbo in 
          let es = Misc.o2s string_of_int ebo in
          ignore(E.log "block i = %d, grd = (%a), thn = (%s), els = (%s)\n" i d_exp e ts es)
  end ifs

let print_gdoms gdoms =
  if mydebug then
    Array.iteri begin fun i (j, bo) -> 
      Pretty.printf "block i = %d, idom = %d, guard = %s \n"
      i j (Misc.o2s string_of_bool bo) |> ignore
    end gdoms

let print_edoms edoms : unit =
  if mydebug then
    Hashtbl.iter begin fun (i, j) b ->
      ignore (Pretty.printf "edge dom i = %d, j = %d, b = %b \n" i j b)
    end edoms

(****************************************************************)             

let block_id b = 
  match b.bstmts with 
  | s::_ -> Some s.sid 
  | _    -> None 

class guardVisitor ifs = object(self)
  inherit nopCilVisitor
  method vstmt (s: stmt) : stmt visitAction =
    match s.skind with
    | If (e, tb, eb, _) ->
        Array.set ifs s.sid (Some (e, block_id tb, block_id eb));
        DoChildren
    | _                 -> 
        DoChildren
end

(** [mk_ifs n fdec] = block :-> ginfo option 
 * s.t.  i :-> None                     if i not an IF-STM
	 i :-> Some (e, tbo, ebo)       if i is  an IF-STM with:
                                        guard expr = e, 
                                        then block = tbo, 
                                        else block = ebo.       *)

let mk_ifs (n:int) (fdec:fundec) : ginfo array =
  let ifs = Array.make n None in
  let _   = visitCilFunction (new guardVisitor ifs) fdec in
  ifs

(** [dom_by_then ifs i j] = true iff i has idom j and is dom by "then" cond of j *)
let dom_by_then preds ifs i j =
  match ifs.(j) with
  | Some (_, Some i', _)        -> i = i' 
  | Some (_, None, (Some i'))   -> i != i' && preds.(i) = [j] 
  | _ 			        -> false

(** [dom_by_else ifs i j] = true iff i has idom j and is dom by "else" cond of j *)
let dom_by_else preds ifs i j = 
  match ifs.(j) with
  | Some (_, _, Some i')        -> i = i' 
  | Some (_, (Some i'), None)   -> i != i' && preds.(i) = [j] 
  | _			        -> false

(** [mk_gdoms ifs doms] = block :-> block * (bool option) 
    s.t. i :-> j, Some true	if j is idom i and i is dom by "then" cond 
	 i :-> j, Some false	if j is idom i and i is dom by "else" cond
	 i :-> j, None 		if j is idom i o.w. *)
let mk_gdoms preds ifs idom = 
  Array.mapi begin fun i j -> 
    if j < 0 then (j, None) else 
      let tb = dom_by_then preds ifs i j in
      let eb = dom_by_else preds ifs i j in
      let _  = asserts  (not (tb && eb)) "ERROR: then/else dom! i=%d, j=%d" i j in
      (j, if tb || eb then Some tb else None)
  end idom

let mk_edoms preds ifs idom = 
  let t = Hashtbl.create 17 in 
  Array.iteri begin fun i js ->
    List.iter begin fun j ->
      match ifs.(j) with
      | Some (p, Some i', None) when i != i' ->
          Hashtbl.add t (j, i) false
      | Some (p, None, Some i') when i != i' ->
          Hashtbl.add t (j, i) true 
      | _ -> ()
    end js
  end preds;
  t

