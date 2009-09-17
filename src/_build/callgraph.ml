open Misc.Ops
open Cil

type t = varinfo list list

module ComparableVar =
  struct
    type t      = varinfo
    let compare = compare
    let equal   = (=)
    let hash    = Hashtbl.hash
  end

module G   = Graph.Imperative.Digraph.Concrete(ComparableVar)
module SCC = Graph.Components.Make(G)

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
    GFun (fundec, _) -> visitCilBlock (new bodyVisitor cg fundec.svar) fundec.sbody |> ignore; SkipChildren
  | _                -> DoChildren
end

let sccs (f: file): t =
  let cg = G.create () in
  let _  = visitCilFile (new callgraphVisitor cg) f in
    SCC.scc_list cg
