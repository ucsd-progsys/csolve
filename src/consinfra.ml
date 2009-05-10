module F  = Format
module ST = Ssa_transform
module H  = Hashtbl

open Misc.Ops
open Cil

(* MOVE TO misc *)
let hashtbl_of_list xys = 
  let t = Hashtbl.create 37 in
  let _ = List.iter (fun x,y -> Hashtbl.add t x y) xys in
  t

(* MOVE TO misc *)
let array_flapi f a =
  Array.fold_left (fun (i, acc) x -> (i+1, (f i x) :: acc)) (0,[]) a
  |> snd 
  |> List.rev
  |> Misc.flatten


(*******************************************************************)
(************* Constraint Generation Infrastructure ****************)
(*******************************************************************)

type t = {
  sci     : ST.ssaCfgInfo;
  size    : int;                            (* number of blocks *)
  vart    : (string, Cil.varinfo) H.t;      (* string|-> var *) 
  expt    : (string, Cil.exp) H.t;          (* string|-> defining assignment *)
  blockt  : (string, int) H.t;              (* string|-> defining block *)
  defa    : string list array;              (* block |-> vars defined inside block *)
  doma    : (int * bool option) list array; (* block |-> (dom guard * bool) list *)
  phidefa : (string * string) list array;   (* block |-> (x, xi) list, 
                                                            st. xi defined in block,
                                                             x = phi(...xi...) *)
}

(*******************************************************************)
(*************  Visitor: Gather block-binding information  *********)
(*******************************************************************)

class consInfraVisitor size bindr = object(self) 
  inherit nopCilVisitor
  
  val sid = ref 0

  method vinst = function
    | Set (((Var v), NoOffset), e, _) ->
        bindr := (!sid, v.Cil.vname, e) :: !bindr;
        DoChildren 
    | _ -> 
        asserts false "TBD: consInfraVisitor vinst";
        assert false

  method vstmt s =
    asserts (0 <= s.Cil.sid && s.Cil.sid < size) "consInfraVisitor";
    sid := s.sid;
    DoChildren
end

let defa_of_bindings size binds = 
  let defa = Array.make size [] in
  List.iter (fun (i, v, _) -> defa.(i) <- v.Cil.vname :: defa.(i)) binds;
  defa

let var_expt_of_bindings binds = 
  let t = H.create 37 in
  let _ = List.iter 
            (fun (_, v, e) -> 
              let vn = v.Cil.vname in
              asserts (not (H.mem t vn)) "duplicate binding";
              H.add t vn e) 
            binds in
  t 

(*******************************************************************)
(********** Dom-Tree: Guards and Reaching Definitions **************) 
(*******************************************************************)

let dom_closure gdoma = 
  let n    = Array.length gdoma in
  let doma = Array.make n [] in 
  for i = 0 to n - 1 do 
    Array.set doma i (gdoma.(i) :: doma.(j))
  done;
  doma

let guard me = function 
  | (_, None)   -> 
      None 
  | (i, Some b) -> begin 
      match me.sci.ST.ifs.(i) with 
      | None _ -> 
          assertf "ERROR: expand_guard"
      | Some (e,_,_) -> 
          let p  = CilInterface.pred_of_cilexp e in
          Some (if b then p else (Ast.pNot p))
  end

(*****************************************************************)
(********** (Inverted) Phi-bindings  *****************************)
(********** block |-> (x, xi) list,  *****************************)
(********** st. xi defined in block, *****************************) 
(********** and x = phi(...xi...)    *****************************) 
(*****************************************************************)

let phidefa_of_phis phia =
  let a       = Array.make (Array.length phia) [] in
  let phidefs = Misc.array_flapi begin fun _ asgns -> 
                  Misc.flap begin fun (v, ivis) -> 
                    Misc.flap begin fun (i,vi) -> (i,v,vi)
                    end ivis
                  end asgns
                end phia in
  List.iter (fun (i, v, vi) -> a.(i) <- (v, vi) :: (a.(i))) phidefs;
  a

(*****************************************************************)
(************************ API ************************************)
(*****************************************************************)

(* API *)
let create sci =
  let n = Array.length sci.ST.gdoms in
  let bindsr = ref [] in
  let vis    = new consInfraVisitor size bindsr in
  let _      = Cil.visitCilFunction vis sci.ST.fdec in
  { sci     = sci;
    size    = n;
    doma    = dom_closure sci.ST.gdoms;
    defa    = defa_of_bindings size !bindsr; 
    expt    = var_expt_of_bindings !bindsr;
    phidefa = phidefa_of_phis sci.ST.phis;
    vart    = sci.ST.fdec.Cil.slocals 
              |> List.map (fun v -> (v.Cil.vname, v))
              |> Misc.hashtbl_of_list; 
  }
 
(* API *)
let location me i =
  Cil.get_stmtLoc me.sci.ST.cfg.Ssa.blocks.(i).Ssa.bstmt.skind

(* API *)
let ssa_srcs me i = 
  Misc.do_catch "ssa_srcs" (Array.get me.phidefa) i 
  |> List.map (Misc.map_pr (H.find me.vart))

(* API *)
let ssa_targs me i = 
  Misc.do_catch "ssa_targs" (Array.get me.sci.ST.phis) i
  |> List.map fst 

(* API *)
let var_exp me v = 
  Misc.do_catch "var_exp" (H.find me.expt) v.Cil.vname 

(* API *)
let def_vars me i = 
  Misc.do_catch "defs" (Array.get me.defa) i 

(* API *)
let reach_vars me i = 
  Misc.do_catch "reach" (Array.get me.doma) i 
  |> Misc.flap (defs me)

(* API *)
let guardp me i = 
  Misc.do_catch "guardp" (Array.get me.doma) i 
  |> Misc.map_partial (guard me) 
  |> Ast.pAnd
