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

module C = Ast.Constraint

(** indexing and dependencies *)
val create      : C.t list -> t 
val deps        : t -> C.t -> C.t list
val iter        : t -> (C.t -> unit) -> unit

(** worklist manipulation *)
val push        : t -> wkl -> C.t list -> wkl 
val pop         : t -> wkl -> (C.t option * wkl)
val init        : t -> wkl

module WH = 
  Heap.Functional(struct 
      type t = subref_id * int * (int * bool * fc_id)
      let compare (_,ts,(i,j,k)) (_,ts',(i',j',k')) =
        if i <> i' then compare i i' else
          if ts <> ts' then -(compare ts ts') else
            if j <> j' then compare j j' else 
              compare k' k
    end)

type t = 
  { orig: labeled_constraint SIM.t;     (* id -> orig *)
    cnst: refinement_constraint SIM.t;  (* id -> refinement_constraint *) 
    rank: (int * bool * fc_id) SIM.t;   (* id -> dependency rank *)
    depm: subref_id list SIM.t;         (* id -> successor ids *)
    pend: (subref_id,unit) Hashtbl.t;   (* id -> is in wkl ? *)
  }

let get_ref_id = 
  function WFRef (_,_,Some i) | SubRef (_,_,_,_,Some i) -> i | _ -> assert false

let get_ref_rank sri c = 
  try SIM.find (get_ref_id c) sri.rank with Not_found ->
    (printf "ERROR: @[No@ rank@ for:@ %a@\n@]" (pprint_ref None) c; 
     raise Not_found)

let get_ref_constraint sri i = 
  C.do_catch "ERROR: get_constraint" (SIM.find i) sri.cnst

let lhs_ks = function WFRef _ -> assert false | SubRef (env,_,r,_,_) ->
  Le.fold (fun _ f l -> F.refinement_qvars f @ l) env (F.refinement_qvars r)

let rhs_k = function
  | SubRef (_,_,_,(_, F.Qvar k),_) -> Some k
  | _ -> None

let wf_k = function
  | WFRef (_, (_, F.Qvar k), _) -> Some k
  | _ -> None

let ref_k c =
  match (rhs_k c, wf_k c) with
  | (Some k, None)
  | (None, Some k) -> Some k
  | _ -> None

let ref_id = function
  | WFRef (_, (_, _), Some id)
  | SubRef (_,_,_,(_, _), Some id) -> id
  | _ -> -1

let print_scc_edge rm (u,v) = 
  let (scc_u,_,_) = SIM.find u rm in
  let (scc_v,_,_) = SIM.find v rm in
  let tag = if scc_u > scc_v then "entry" else "inner" in
  C.cprintf C.ol_solve "@[SCC@ edge@ %d@ (%s)@ %d@ ====> %d@\n@]" scc_v tag u v

let make_rank_map om cm =
  let get vm k = try VM.find k vm with Not_found -> [] in
  let upd id vm k = VM.add k (id::(get vm k)) vm in
  let km = 
    BS.time "step 1"
    (SIM.fold 
      (fun id c vm -> match c with WFRef _ -> vm 
        | SubRef _ -> List.fold_left (upd id) vm (lhs_ks c))
      cm) VM.empty in
  let (dm,deps) = 
    BS.time "step 2"
    (SIM.fold
      (fun id c (dm,deps) -> 
        match (c, rhs_k c) with 
        | (WFRef _,_) -> (dm,deps) 
        | (_,None) -> (dm,(id,id)::deps) 
        | (_,Some k) -> 
          let kds = get km k in
          let deps' = List.map (fun id' -> (id,id')) (id::kds) in
          (SIM.add id kds dm, (List.rev_append deps' deps)))
      cm) (SIM.empty,[]) in
  let flabel i = C.io_to_string ((SIM.find i om).lc_id) in
  let rm = 
    let rank = BS.time "scc rank" (C.scc_rank flabel) deps in
    BS.time "step 2"
    (List.fold_left
      (fun rm (id,r) -> 
        let b = (not !Cf.psimple) || (is_simple_constraint (SIM.find id cm)) in
        let fci = (SIM.find id om).lc_id in
        SIM.add id (r,b,fci) rm)
      SIM.empty) rank in
  (dm,rm)

let fresh_refc = 
  let i = ref 0 in
  fun c -> 
    let i' = incr i; !i in
    match c with  
    | WFRef (env,r,None) -> WFRef (env,r,Some i')
    | SubRef (env,g,r1,r2,None) -> SubRef (env,g,r1,r2,Some i')
    | _ -> assert false

(* API *)
let create_t ocs = 
  let ics = List.map (fun (o,c) -> (o,fresh_refc c)) ocs in
  let (om,cm) = 
    List.fold_left 
      (fun (om,cm) (o,c) ->
        let i = get_ref_id c in 
        (SIM.add i o om, SIM.add i c cm))
      (SIM.empty, SIM.empty) ics in
  let (dm,rm) = BS.time "make rank map" (make_rank_map om) cm in
  {orig = om; cnst = cm; rank = rm; depm = dm; pend = Hashtbl.create 17}

let get_ref_orig sri c = 
  C.do_catch "ERROR: get_ref_orig" (SIM.find (get_ref_id c)) sri.orig

let get_ref_fenv sri c =
  (function SubFrame (a, _, _, _) | WFFrame (a, _) -> a) (get_ref_orig sri c).lc_cstr

(* API : deps *) 
let deps sri c =
  let is' = try SIM.find (get_ref_id c) sri.depm with Not_found -> [] in
  List.map (get_ref_constraint sri) is'

(* API : to_list *)
let to_list sri = 
  SIM.fold (fun _ c cs -> c::cs) sri.cnst [] 

(* API : iter *)
let iter sri f = 
  SIM.iter (fun _ c -> f c) sri.cnst

let iter_ref_origs sri f =
  SIM.iter (fun i c -> f i c) sri.orig

let sort_iter_ref_constraints sri f = 
  let rids  = SIM.fold (fun id (r,_,_) ac -> (id,r)::ac) sri.rank [] in
  let rids' = List.sort (fun x y -> compare (snd x) (snd y)) rids in 
  List.iter (fun (id,_) -> f (SIM.find id sri.cnst)) rids' 

(* API: push *)
let push =
  let timestamp = ref 0 in
  fun sri w cs ->
    incr timestamp;
    List.fold_left 
      (fun w c -> 
        let id = get_ref_id c in
        if Hashtbl.mem sri.pend id then w else 
          (C.cprintf C.ol_solve "@[Pushing@ %d at %d@\n@]" id !timestamp; 
           Hashtbl.replace sri.pend id (); 
           WH.add (id,!timestamp,get_ref_rank sri c) w))
      w cs

(* API *)
let pop sri w =
  try 
    let (id, _, _) = WH.maximum w in
    let _ = Hashtbl.remove sri.pend id in
    (Some (get_ref_constraint sri id), WH.remove w)
  with Heap.EmptyHeap -> (None,w) 

(* API *)
let create_wkl sri =
  let cs = List.filter is_subref_constraint (to_list sri) in
  push sri WH.empty cs 


