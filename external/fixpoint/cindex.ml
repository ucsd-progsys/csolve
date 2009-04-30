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

(***************************************************************)
(**** This module implements constraint indexing ***************)
(***************************************************************)

module F  = Format
module BS = Bstats
module Co = Constants
module C  = Constraint
module IM = Misc.IntMap
module SM = Ast.Symbol.SMap 
open Misc.Ops

type fc_id = int option 
type subref_id = int 

module WH = 
  Heap.Functional(struct 
      type t = subref_id * int * (int * bool)
      let compare (_,ts,(i,j)) (_,ts',(i',j')) =
        if i <> i' then compare i i' else
          if ts <> ts' then -(compare ts ts') else
            compare j j'
    end)

type wkl = WH.t

type t = 
  { cnst: Constraint.t IM.t;            (* id -> refinement_constraint *) 
    rank: (int * bool) IM.t;            (* id -> dependency rank *)
    depm: subref_id list IM.t;          (* id -> successor ids *)
    pend: (subref_id,unit) Hashtbl.t;   (* id -> is in wkl ? *)
  }

let get_ref_rank me c =
  Misc.do_catch "ERROR: Cindex.get_ref_rank" (IM.find (C.get_id c)) me.rank

let get_ref_constraint me i = 
  Misc.do_catch "ERROR: Cindex.get_ref_constraint" (IM.find i) me.cnst

let refa_ko = function C.Kvar (_,k) -> Some k | _ -> None

let reft_ks = function (_,_,ras) -> Misc.map_partial refa_ko ras

let lhs_ks (env ,_ , (r1:C.reft) ,_ ,_) = 
  reft_ks r1 |> 
  SM.fold (fun _ (r:C.reft) l -> (reft_ks r) ++ l) env

let rhs_ks (_,_,_,(r2:C.reft),_) =
  reft_ks r2

let print_scc_edge rm (u,v) = 
  let (scc_u,_,_) = IM.find u rm in
  let (scc_v,_,_) = IM.find v rm in
  let tag = if scc_u > scc_v then "entry" else "inner" in
  Co.cprintf Co.ol_solve "SCC edge %d (%s) %d ====> %d \n" scc_v tag u v

let make_rank_map () (cm : C.t IM.t) =
  let get km k = try SM.find k km with Not_found -> [] in
  let upd id km k = SM.add k (id::(get km k)) km in
  let km = 
    IM.fold 
      (fun id (c:Constraint.t) vm -> lhs_ks c |> List.fold_left (upd id) vm) 
      cm SM.empty in
  let (dm, deps) = 
    IM.fold
      (fun id c (dm, deps) ->
        rhs_ks c |> 
        List.fold_left 
          (fun (dm, deps) k -> 
            let kds = get km k in
            let deps' = List.map (fun id' -> (id,id')) (id::kds) in
            (IM.add id kds dm, (deps' ++ deps)))
          (dm, (id,id)::deps))
      cm (IM.empty,[]) in
  let rm = 
    let rank = Common.scc_rank string_of_int deps in
    List.fold_left
      (fun rm (id,r) -> 
        let b = (not !Co.psimple) || (C.is_simple (IM.find id cm)) in
        IM.add id (r,b) rm)
      IM.empty rank in
  (dm,rm)

(* API *)
let create cs = 
  let cm = List.fold_left 
             (fun cm c -> IM.add (C.get_id c) c cm) 
             IM.empty cs in
  let (dm,rm) = BS.time "make rank map" (make_rank_map ()) cm in
  {cnst = cm; rank = rm; depm = dm; pend = Hashtbl.create 17}

(* API *) 
let deps me c =
  let is' = try IM.find (C.get_id c) me.depm with Not_found -> [] in
  List.map (get_ref_constraint me) is'

(* API *)
let to_list me = 
  IM.fold (fun _ c cs -> c::cs) me.cnst [] 

(* API *)
let iter f me = 
  IM.iter (fun _ c -> f c) me.cnst

let sort_iter_ref_constraints me f = 
  let rids  = IM.fold (fun id (r,_) ac -> (id,r)::ac) me.rank [] in
  let rids' = List.sort (fun x y -> compare (snd x) (snd y)) rids in 
  List.iter (fun (id,_) -> f (IM.find id me.cnst)) rids' 

(* API *)
let wpush =
  let timestamp = ref 0 in
  fun me w cs ->
    incr timestamp;
    List.fold_left 
      (fun w c -> 
        let id = C.get_id c in
        if Hashtbl.mem me.pend id then w else 
          (Co.cprintf Co.ol_solve "Pushing %d at %d \n" id !timestamp; 
           Hashtbl.replace me.pend id (); 
           WH.add (id,!timestamp,get_ref_rank me c) w))
      w cs

(* API *)
let wpop me w =
  try 
    let (id,_,_) = WH.maximum w in
    let _        = Hashtbl.remove me.pend id in
    let c        = get_ref_constraint me id in
    let (r,b)    = get_ref_rank me c in
    let _        = Co.cprintf Co.ol_solve "popping %d in scc (%d,%b,%s) \n" id r b in 
    (Some c, WH.remove w)
  with Heap.EmptyHeap -> (None,w) 

(* API *)
let winit me =
  to_list me |> wpush me WH.empty  

(* API *) 
let print ppf me = 
  if !Co.dump_ref_constraints then begin
    Format.fprintf ppf "Refinement Constraints: \n";
    iter (Format.fprintf ppf "@[%a@.@]" (C.print None)) me;
    Format.fprintf ppf "\n SCC Ranked Refinement Constraints: \n";
    sort_iter_ref_constraints me (Format.fprintf ppf "@[%a@.@]" (C.print None)); 
  end



