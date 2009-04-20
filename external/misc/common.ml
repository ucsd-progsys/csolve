(*
 * Copyright © 2008 The Regents of the University of California. All rights reserved.
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

module F = Format

(*
let rec maybe_list_from_singles = function
  | x :: xs -> (match x with [a] -> Some a |  _ -> None) :: (maybe_list_from_singles xs)
  | [] -> []

let sub_from_list subs s =
  try List.assoc s subs with Not_found -> s

let strip_meas s =
  let start = try 1 + (String.rindex s '.') with Not_found -> 0 in
  try 
    if String.sub s start 6 = "_meas_" then 
      let pre  = String.sub s 0 start in
      let post = String.sub s (start + 6) ((String.length s) - start - 6) in
      pre ^ post
    else s
  with Invalid_argument _ -> s


let sub_from s c =
  try 
    let x = String.rindex s c in
      String.sub s x ((String.length s) - x)
  with Not_found -> s

let sub_to_r s c =
  let x = String.rindex s c in
    String.sub s 0 x

let strip_meas_whole s =
  if !Clflags.dsmeasures then s else
  try 
    let start = try String.rindex s '.'
    
    if String.sub s 0 6 = "_meas_" then 
    String.sub s 6 (String.length s - 6) 
  else s with Invalid_argument _ -> s 

let rw_suff f s c =
  let suff = f (sub_from s c) in
    try (sub_to_r s c) ^ suff with Not_found -> suff

let strip_meas s =
  rw_suff strip_meas_whole s '.'
*)
(*
let l_to_s l = String.concat "." (Longident.flatten l)
let s_to_l s = Longident.parse s

let l_is_id id = function
  | Longident.Lident s -> s = id
  | _ -> false

let int_of_tag = function
    Cstr_constant n -> 2*n
  | Cstr_block n -> 2*n+1
  | Cstr_exception _-> assert false
                       
let tag_of_int n = 
  if 2*(n/2) = n then
    Cstr_constant (n/2)
  else
    Cstr_block ((n-1)/2)
*)


(****************************************************************)
(************* SCC Ranking **************************************)
(****************************************************************)

module Int : Graph.Sig.COMPARABLE with type t = int * string =
struct
   type t = int * string 
   let compare = compare
   let hash = Hashtbl.hash
   let equal = (=)
end

module G = Graph.Imperative.Digraph.Concrete(Int)

module SCC = Graph.Components.Make(G)    

 (* Use of Graphviz *)

let io_to_string = function 
  | Some i -> string_of_int i 
  | None -> "*"

let xs_to_string f xs =
  "["^(String.concat "," (List.map f xs))^"]"

module DotGraph =
struct
   type t = G.t
   module V = G.V
   module E = G.E
   let iter_vertex = G.iter_vertex
   let iter_edges_e = G.iter_edges_e
   let graph_attributes g = []
   let default_vertex_attributes g = [`Shape `Box]
   let vertex_name (i,s) = Printf.sprintf "V_%d_%s" i s 
   let vertex_attributes v = [`Label (vertex_name v)]
   let default_edge_attributes g = []
   let edge_attributes e = []
   let get_subgraph v = None
end

module Dot = Graph.Graphviz.Dot(DotGraph) 

let dump_graph g = 
  let oc = open_out "constraints.dot" in
  Dot.output_graph oc g; 
  close_out oc


(* Given list [(u,v)] returns a numbering [(ui,ri)] s.t. 
 * 1. if ui,uj in same SCC then ri = rj
 * 2. if ui -> uj then ui >= uj *)
let scc_rank f ijs = 
  let g = G.create () in
  let _ = Bstats.time "making graph" (List.iter (fun (i,j) -> G.add_edge g (i,(f i)) (j,(f j)))) ijs in
  let _ = if !Clflags.dump_graph then dump_graph g in
  let a = SCC.scc_array g in
  let _ = cprintf ol_scc "@[dep@ graph:@ vertices@ =@ @ %d,@ sccs@ =@ %d@ @\n@]" 
          (G.nb_vertex g) (Array.length a);
          cprintf ol_scc "@[scc@ sizes:@\n@]";
          let int_s_to_string (i,s) = Printf.sprintf "(%d,%s)" i s in
          Array.iteri 
            (fun i xs -> 
               cprintf ol_scc "@[%d@ :@ %s@ @\n@]" 
                 i (xs_to_string int_s_to_string xs)) a;
          cprintf ol_scc "@[@\n@]" in
  let sccs = array_to_index_list a in
  flap (fun (i,vs) -> List.map (fun (j,_) -> (j,i)) vs) sccs

(*
let g1 = [(1,2);(2,3);(3,1);(2,4);(3,4);(4,5)];;
let g2 = [(0,1);(1,2);(2,0);(1,3);(4,3);
          (5,6);(5,7);(6,9);(7,9);(7,8);(8,5)];;
let g3 = (6,2)::g2;;
let g4 = (2,6)::g2;;
  
let n1 = make_scc_num g1 ;;
let n2 = make_scc_num g2 ;;
let n3 = make_scc_num g3 ;;
let n4 = make_scc_num g4 ;; *)

