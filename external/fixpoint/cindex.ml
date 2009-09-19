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
module H  = Hashtbl
module F  = Format
module BS = BNstats
module Co = Constants
module C  = FixConstraint
module IM = Misc.IntMap
module SM = Ast.Symbol.SMap 
open Misc.Ops

(***********************************************************************)
(***************** Index Data Structures and Accessors *****************)
(***********************************************************************)

type rank = {
  id    : C.id;
  scc   : int;
  simpl : bool;
  tag   : C.tag;
}

let string_of_tag t = 
  Printf.sprintf "[%s]" (Misc.map_to_string string_of_int t)

let pprint_rank ppf r = 
  Format.fprintf ppf "id=%d, scc=%d, tag=%s" 
    r.id r.scc (string_of_tag r.tag)

module WH = 
  Heaps.Functional(struct 
      type t = int * rank 
      let compare (ts,r) (ts',r') = 
        if r.scc <> r'.scc then compare r.scc r'.scc else
          if ts <> ts' then - (compare ts ts') else 
            if !Constants.ptag && r.tag <> r'.tag then compare r.tag r'.tag else
              compare r.simpl r'.simpl
    end)

type wkl = WH.t

type t = 
  { cnst : FixConstraint.t IM.t;   (* id   -> refinement_constraint *) 
    rnkm : rank IM.t;              (* id   -> dependency rank *)
    depm : C.id list IM.t;         (* id   -> successor ids *)
    pend : (C.id, unit) H.t;       (* id   -> is in wkl ? *)
    rtm  : unit IM.t;              (* rank -> unit, keys are "root" sccs *) 
    ds   : C.dep list;             (* add/del dep list *)
  }

let get_ref_rank me c =
  Misc.do_catch "ERROR: Cindex.get_ref_rank" (IM.find (C.id_of_t c)) me.rnkm

let get_ref_constraint me i = 
  Misc.do_catch "ERROR: Cindex.get_ref_constraint" (IM.find i) me.cnst

(***********************************************************************)
(******************** Building Real Dependencies ***********************)
(***********************************************************************)

let refa_ko = function C.Kvar (_,k) -> Some k | _ -> None

let reft_ks = function (_,_,ras) -> Misc.map_partial refa_ko ras

let lhs_ks c = 
  c |> C.lhs_of_t
    |> reft_ks 
    |> SM.fold (fun _ (r:C.reft) l -> (reft_ks r) ++ l) (C.env_of_t c)

let rhs_ks c =
  c |> C.rhs_of_t |> reft_ks 

let make_deps cm = 
  let get = fun km k -> try SM.find k km with Not_found -> [] in
  let upd = fun id km k -> SM.add k (id::(get km k)) km in
  let km  = IM.fold (fun id c vm -> List.fold_left (upd id) vm (lhs_ks c)) cm SM.empty in
  IM.fold begin fun id c (dm, deps) ->
    List.fold_left begin fun (dm, deps) k -> 
      let kds = get km k in
      let deps' = List.map (fun id' -> (id, id')) kds in
      (IM.add id kds dm, (deps' ++ deps)) 
    end (dm, deps) (rhs_ks c) 
  end cm (IM.empty,[])

(***********************************************************************)
(************* Adjusting Dependencies with Provided Tag-Deps ***********)
(***********************************************************************)

let delete_deps cm dds = 
  let delf = C.matches_deps dds in
  let tagf = fun x -> IM.find x cm |> C.tag_of_t in
  List.filter (not <.> delf <.> Misc.map_pair tagf)
  
let add_deps cm ads ijs = 
  let tt = H.create 37 in
  let _  = IM.iter (fun id c -> H.add tt (C.tag_of_t c) id) cm in
  ads |> Misc.map C.tags_of_dep
      |> Misc.map (Misc.map_pair (H.find_all tt))
      |> Misc.flap (Misc.uncurry Misc.cross_product)
      |> (++) ijs

let adjust_deps cm ds = 
  let ads, dds = List.partition C.pol_of_dep ds in
  add_deps cm ads <.> delete_deps cm dds 

(***********************************************************************)
(**************************** Dependency SCCs **************************)
(***********************************************************************)

let fid_of_cid cm (i,j) = 
  try 
    let [_;_;_;i'] = IM.find i cm |> C.tag_of_t in
    let [_;_;_;j'] = IM.find j cm |> C.tag_of_t in
    if i' <> j' then Some (i', j') else None 
  with _ -> None 

let string_of_cid cm id = 
  try 
    IM.find id cm 
    |> C.tag_of_t 
    |> string_of_tag 
    |> Printf.sprintf "%d: %s" id
  with _ -> assertf "string_of_cid: impossible" 

let make_rankm cm ranks = 
  List.fold_left begin fun rm (id, r) -> 
    let c = IM.find id cm in
    IM.add id {id    = id; 
               scc   = r; 
               simpl = (not !Co.psimple) || (C.is_simple c); 
               tag   = C.tag_of_t c;} rm
  end IM.empty ranks 

let make_rootm rankm ijs =
  let sccs = rankm |> Misc.intmap_bindings |> Misc.map (fun (_,r) -> r.scc) in 
  let sccm = List.fold_left (fun im scc -> IM.add scc () im) IM.empty sccs in
  List.fold_left begin fun sccm (i,j) ->
    let ir = (IM.find i rankm).scc in
    let jr = (IM.find j rankm).scc in
    if ir <> jr then IM.remove jr sccm else sccm
  end sccm ijs

let make_rank_map ds cm =
  let (dm, real_deps) = make_deps cm in
  let deps  = adjust_deps cm ds real_deps in
  let ids   = cm |> Misc.intmap_bindings |> Misc.map fst in
  let ranks = Fcommon.scc_rank "constraint" (string_of_cid cm) ids deps in
  let _     = deps |> Misc.map_partial (fid_of_cid cm) |> Fcommon.scc_rank "fconstraint" string_of_int [] in 
  let rankm = make_rankm cm ranks in
  let rootm = make_rootm rankm deps in
  (dm, rankm, rootm)

(***********************************************************************)
(**************************** API **************************************)
(***********************************************************************)

(* API *)
let create ds cs =
  let cm            = List.fold_left (fun cm c -> IM.add (C.id_of_t c) c cm) IM.empty cs in
  let (dm, rm, rtm) = BS.time "make rank map" (make_rank_map ds) cm in
  {cnst = cm; ds = ds; rnkm = rm; depm = dm; rtm = rtm; pend = H.create 17}

(* API *) 
let deps me c =
  (try IM.find (C.id_of_t c) me.depm with Not_found -> [])
  |> List.map (get_ref_constraint me)

(* API *)
let to_list me = 
  me.cnst |> Misc.intmap_bindings |> Misc.map snd

let sort_iter_ref_constraints me f = 
  me.rnkm |> Misc.intmap_bindings
          |> List.sort (fun (_,r) (_,r') -> compare r.tag r'.tag) 
          |> List.iter (fun (id,_) -> f (IM.find id me.cnst)) 

(* API *)
let wpush =
  let timestamp = ref 0 in
  fun me w cs ->
    incr timestamp;
    List.fold_left 
      (fun w c -> 
        let id = C.id_of_t c in
        if Hashtbl.mem me.pend id then w else 
          (Co.cprintf Co.ol_solve "Pushing %d at %d \n" id !timestamp; 
           Hashtbl.replace me.pend id (); 
           WH.add (!timestamp, get_ref_rank me c) w))
      w cs

let wstring w = 
  WH.fold (fun (_,r) acc -> r.id :: acc) w [] 
  |> List.sort compare
  |> Misc.map_to_string string_of_int

(* API *)
let wpop me w =
  try 
    let _, r = WH.maximum w in
    let _    = Hashtbl.remove me.pend r.id in
    let c    = get_ref_constraint me r.id in
    let _    = Co.cprintf Co.ol_solve "popping (%a) "pprint_rank r in
    let _    = Co.cprintf Co.ol_solve "from wkl = %s \n" (wstring w) in 
    (Some c, WH.remove w)
  with Heaps.EmptyHeap -> (None, w) 

let roots me =
  IM.fold begin fun id r sccm ->
   (*  if not (IM.mem r.scc me.rtm) then sccm else *)
      let rs = try IM.find r.scc sccm with Not_found -> [] in
      IM.add r.scc (r::rs) sccm
  end me.rnkm IM.empty
  |> IM.map (List.hd <.> List.sort compare)
  |> Misc.intmap_bindings 
  |> Misc.map (fun (_,r) -> get_ref_constraint me r.id) 

(* API *)
let winit me = 
  roots me |> wpush me WH.empty  

(* API *) 
let print ppf me =
  List.iter (Format.fprintf ppf "@[%a@] \n" C.print_dep) me.ds; 
  IM.iter (fun _ c -> Format.fprintf ppf "@[%a@] \n" (C.print_t None) c) me.cnst
  
(* iter (Format.fprintf ppf "@[%a@.@]" (C.print_t None)) me; *)
  (* if !Co.dump_ref_constraints then begin
    Format.fprintf ppf "Refinement Constraints: \n";
    iter (Format.fprintf ppf "@[%a@.@]" (C.print_t None)) me;
    Format.fprintf ppf "\n SCC Ranked Refinement Constraints: \n";
    sort_iter_ref_constraints me (Format.fprintf ppf "@[%a@.@]" (C.print_t None)); 
  end *)
