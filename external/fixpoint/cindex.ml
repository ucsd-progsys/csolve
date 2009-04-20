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

module WH = 
  Heap.Functional(struct 
      type t = subref_id * int * (int * bool * fc_id)
      let compare (_,ts,(i,j,k)) (_,ts',(i',j',k')) =
        if i <> i' then compare i i' else
          if ts <> ts' then -(compare ts ts') else
            if j <> j' then compare j j' else 
              compare k' k
    end)

type wkl = WH.t

type t = 
  { orig: labeled_constraint SIM.t;     (* id -> orig *)
    cnst: refinement_constraint SIM.t;  (* id -> refinement_constraint *) 
    rank: (int * bool * fc_id) SIM.t;   (* id -> dependency rank *)
    depm: subref_id list SIM.t;         (* id -> successor ids *)
    pend: (subref_id,unit) Hashtbl.t;   (* id -> is in wkl ? *)
  }

let get_ref_id = 
  function WFRef (_,_,Some i) | SubRef (_,_,_,_,Some i) -> i | _ -> assert false

let get_ref_rank me c = 
  try SIM.find (get_ref_id c) me.rank with Not_found ->
    (printf "ERROR: @[No@ rank@ for:@ %a@\n@]" (pprint_ref None) c; 
     raise Not_found)

let get_ref_constraint me i = 
  C.do_catch "ERROR: get_constraint" (SIM.find i) me.cnst

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
        let b = (not !Cf.psimple) || (C.is_simple (SIM.find id cm)) in
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

let get_ref_orig me c = 
  C.do_catch "ERROR: get_ref_orig" (SIM.find (get_ref_id c)) me.orig

let get_ref_fenv me c =
  (function SubFrame (a, _, _, _) | WFFrame (a, _) -> a) (get_ref_orig me c).lc_cstr

(* API : deps *) 
let deps me c =
  let is' = try SIM.find (get_ref_id c) me.depm with Not_found -> [] in
  List.map (get_ref_constraint me) is'

(* API : to_list *)
let to_list me = 
  SIM.fold (fun _ c cs -> c::cs) me.cnst [] 

(* API : iter *)
let iter me f = 
  SIM.iter (fun _ c -> f c) me.cnst

let iter_ref_origs me f =
  SIM.iter (fun i c -> f i c) me.orig

let sort_iter_ref_constraints me f = 
  let rids  = SIM.fold (fun id (r,_,_) ac -> (id,r)::ac) me.rank [] in
  let rids' = List.sort (fun x y -> compare (snd x) (snd y)) rids in 
  List.iter (fun (id,_) -> f (SIM.find id me.cnst)) rids' 

(* API *)
let wpush =
  let timestamp = ref 0 in
  fun me w cs ->
    incr timestamp;
    List.fold_left 
      (fun w c -> 
        let id = get_ref_id c in
        if Hashtbl.mem me.pend id then w else 
          (C.cprintf C.ol_solve "@[Pushing@ %d at %d@\n@]" id !timestamp; 
           Hashtbl.replace me.pend id (); 
           WH.add (id,!timestamp,get_ref_rank me c) w))
      w cs

(* API *)
let wpop me w =
  try 
    let (id, _, _) = WH.maximum w in
    let _          = Hashtbl.remove me.pend id in
    let c          = get_ref_constraint me id in
    let (r,b,fci)  = get_ref_rank me c in
    let _          = C.cprintf C.ol_solve "Popping %d at iter %d in scc (%d,%b,%s) \n"
                     (get_ref_id c) r b (Misc.io_to_string fci) in 
    (Some c, WH.remove w)
  with Heap.EmptyHeap -> (None,w) 

(* API *)
let winit me =
  to_list me |> wpush me WH.empty  

(* API *) 
let dump me = 
  if !Co.dump_ref_constraints then begin
    Format.printf "Refinement Constraints: \n" 
    iter me (Format.printf "@[%a@.@]" (C.print None));
    Format.printf "\n SCC Ranked Refinement Constraints: \n";
    sort_iter_ref_constraints me (Format.printf "@[%a@.@]" (C.print None)); 
  end



