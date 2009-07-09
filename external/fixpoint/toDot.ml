module C = FixConstraint
module StrMap = Map.Make (struct type t = string let compare = compare end)
module StrSet = Set.Make (struct type t = string let compare = compare end)
open Misc.Ops


module V = struct
  type t = StrSet.t
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
end
module E = struct
  type t = string
  let compare = Pervasives.compare
  let default = ""
end


module G = Graph.Persistent.Digraph.ConcreteLabeled(V)(E)

module Display = struct
  include G
  let vertex_name v =
    if StrSet.is_empty v then
      "empty"
    else
      "\"" ^ String.escaped (StrSet.elements v |> String.concat ", ") ^ "\""
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let edge_attributes e = [`Label (G.E.label e)]
  let get_subgraph _ = None
end

module DotOutput = Graph.Graphviz.Dot(Display)


let t_to_edge t = 
  let env = C.env_of_t t in
    (*   let grd = C.grd_of_t t in *)
  let lhs = C.lhs_of_t t in
  let rhs = C.rhs_of_t t in
  let tag = try string_of_int (C.id_of_t t) with _ -> 
    failure "ERROR: t_to_edge: anonymous constraint %s" (C.to_string t) in
  let kvs_to_strset default kvs = 
    if kvs = [] then 
      StrSet.add default StrSet.empty 
    else
      List.fold_left 
	(fun s kv -> StrSet.add (snd kv |> Ast.Symbol.to_string) s) StrSet.empty kvs in
  let src =
    C.kvars_of_reft lhs :: List.map (fun b -> snd b |> C.kvars_of_reft) (C.bindings_of_env env) |> 
	List.flatten |> kvs_to_strset "start" in
  let dst = C.kvars_of_reft rhs |> kvs_to_strset "error" in
    G.E.create src tag dst

let to_dot oc ts =
  let g = 
    List.fold_left (fun g t -> t_to_edge t |> G.add_edge_e g) G.empty  ts in
    DotOutput.output_graph oc g
