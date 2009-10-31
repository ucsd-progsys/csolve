open Misc.Ops
open Cil

type t = varinfo list list

module ComparableVar =
  struct
    type t            = varinfo
    let compare v1 v2 = compare v1.vid v2.vid
    let equal v1 v2   = v1.vid = v2.vid
    let hash          = Hashtbl.hash
  end

module G   = Graph.Imperative.Digraph.Concrete(ComparableVar)
module SCC = Graph.Components.Make(G)
module TRV = Graph.Traverse.Dfs(G)

class bodyVisitor (cg: G.t) (caller: varinfo) = object
  inherit nopCilVisitor

  method vinst = function
    | Call (_, Lval (Var callee, NoOffset), _, _) -> G.add_edge cg caller callee; SkipChildren
    | Call (_, _, _, _)                           -> failwith "Can't generate callgraph for non-variable function"
    | _                                           -> SkipChildren
end

class callgraphVisitor (cg: G.t) = object
  inherit nopCilVisitor

  method vglob = function
    GFun (fundec, _) ->
      G.add_vertex cg fundec.svar;
      visitCilBlock (new bodyVisitor cg fundec.svar) fundec.sbody |> ignore;
      SkipChildren
  | _ -> DoChildren
end

let sccs (f: file): t =
  let cg = G.create () in
  let _  = visitCilFile (new callgraphVisitor cg) f in
    SCC.scc_list cg

let reach (f: file) (root : varinfo) =
  let cg = G.create () in
  let _  = visitCilFile (new callgraphVisitor cg) f in
  let rv = ref [] in
  let _  = TRV.prefix_component (fun v -> rv := v :: !rv) cg root in
  let _  = Printf.printf "Reachable funs: %s \n" (String.concat "," (List.map (fun v -> v.vname) !rv)) in
  let _  = exit 0 in
  !rv


