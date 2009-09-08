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
  { cnst: FixConstraint.t IM.t;         (* id -> refinement_constraint *) 
    rnkm: rank IM.t;                    (* id -> dependency rank *)
    depm: C.id list IM.t;               (* id -> successor ids *)
    pend: (C.id, unit) H.t;       (* id -> is in wkl ? *)
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
      let deps' = List.map (fun id' -> (id, id')) (id::kds) in
      (IM.add id kds dm, (deps' ++ deps)) 
    end (dm, deps) (rhs_ks c) 
  end cm (IM.empty,[])

(***********************************************************************)
(************* Adjusting Dependencies with Provided Tag-Deps ***********)
(***********************************************************************)

let delete_deps cm dds ijs = 
  let tt   = H.create 37 in
  let s_tt = H.create 37 in
  let t_tt = H.create 37 in
  List.iter begin fun d -> match C.src_of_dep d, C.dst_of_dep d with
    | Some st, Some st' -> H.add tt (st,st') () 
    | Some st, None     -> H.add s_tt st ()
    | None, Some st'    -> H.add t_tt st' ()
    | _, _              -> assertf "delete_dep: bad dep"
  end dds;
  List.filter begin fun (i, j) ->
    let it = IM.find i cm |> C.tag_of_t in
    let jt = IM.find j cm |> C.tag_of_t in
    not (H.mem tt (it, jt) || H.mem s_tt it || H.mem t_tt jt)
  end ijs
  
let add_deps cm ads ijs = 
  let tt = H.create 37 in
  let _  = IM.iter (fun id c -> H.add tt (C.tag_of_t c) id) cm in
  ads |> Misc.flap begin fun d -> match C.src_of_dep d, C.dst_of_dep d with
           | Some st, Some st' -> Misc.cross_product (H.find_all tt st) (H.find_all tt st')
           | _ -> assertf "add_dep: bad dep" 
         end  
      |> (++) ijs

let adjust_deps dds ads cm = add_deps cm ads <.> delete_deps cm dds 

(***********************************************************************)
(**************************** Dependency SCCs **************************)
(***********************************************************************)

let string_of_cid cm id = 
  try 
    IM.find id cm 
    |> C.tag_of_t 
    |> string_of_tag 
    |> Printf.sprintf "%d: %s" id
  with _ -> assertf "string_of_cid: impossible" 

let make_rank_map dds ads cm =
  let (dm, real_deps) = make_deps cm in
  let deps = adjust_deps dds ads cm real_deps in
  let ids  = cm |> Misc.intmap_bindings |> Misc.map fst in
  let rank = Fcommon.scc_rank (string_of_cid cm) ids deps in
  let rm   = List.fold_left begin fun rm (id, r) -> 
               let c = IM.find id cm in
               IM.add id {id    = id; 
                          scc   = r; 
                          simpl = (not !Co.psimple) || (C.is_simple c); 
                          tag   = C.tag_of_t c;} rm
             end IM.empty rank in
  (dm, rm)

(***********************************************************************)
(**************************** API **************************************)
(***********************************************************************)

(* API *)
let create dds ads cs = 
  let cm       = List.fold_left begin fun cm c -> 
                  IM.add (C.id_of_t c) c cm 
                end IM.empty cs in
  let (dm, rm) = BS.time "make rank map" (make_rank_map dds ads) cm in
  {cnst = cm; rnkm = rm; depm = dm; pend = H.create 17}

(* API *) 
let deps me c =
  (try IM.find (C.id_of_t c) me.depm with Not_found -> [])
  |> List.map (get_ref_constraint me)

(* API *)
let to_list me = 
  me.cnst |> Misc.intmap_bindings |> Misc.map snd

(* API *)
let iter f me = 
  IM.iter (fun _ c -> f c) me.cnst

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
    let rs = try IM.find r.scc sccm with Not_found -> [] in
    IM.add r.scc rs sccm
  end me.rnkm IM.empty
  |> IM.map (List.hd <.> List.sort compare)
  |> Misc.intmap_bindings 
  |> Misc.map (fun (_,r) -> get_ref_constraint me r.id) 

(* API *)
let winit me = 
  roots me |> wpush me WH.empty  

(* API *) 
let print ppf me = 
  if !Co.dump_ref_constraints then begin
    Format.fprintf ppf "Refinement Constraints: \n";
    iter (Format.fprintf ppf "@[%a@.@]" (C.print_t None)) me;
    Format.fprintf ppf "\n SCC Ranked Refinement Constraints: \n";
    sort_iter_ref_constraints me (Format.fprintf ppf "@[%a@.@]" (C.print_t None)); 
  end
