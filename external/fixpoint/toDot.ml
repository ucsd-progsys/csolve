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


module G = Graph.Persistent.Graph.ConcreteLabeled(V)(E)

let x = ""

let t_to_edge t = 
  let env = C.env_of_t t in
  let grd = C.grd_of_t t in
  let lhs = C.lhs_of_t t in
  let rhs = C.rhs_of_t t in
  let tag = try string_of_int (C.id_of_t t) with _ -> 
    failure "ERROR: t_to_edge: anonymous constraint %s" (C.to_string t) in
  let from_kvs = "" in
    List.map (fun b -> snd b |> C.kvars_of_reft) (C.bindings_of_env env) |>
	List.flatten |> List.map (fun kv -> snd kv |> Ast.Symbol.to_string)



