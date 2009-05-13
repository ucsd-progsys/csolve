open Cil
open Misc.Ops

module E = Errormsg

(******************************************************************************)
(*************************** Function Instantiation ***************************)
(******************************************************************************)

let mkSetLval (lv: lval) (e: exp) (loc: location): stmt =
  mkStmt (Instr [Set (lv, e, loc)])

let renameLval (vmap: (int * varinfo) list): lval -> lval = function
  | (Var v, o) as lv ->
      begin try
        (Var (List.assoc v.vid vmap), o)
      with Not_found ->
        lv
      end
  | lv -> lv

class freshVisitor vmap rvo = object
  inherit nopCilVisitor

  method vlval (lv: lval): lval visitAction =
    ChangeDoChildrenPost (renameLval vmap lv, id)

  method vexpr: exp -> exp visitAction = function
    | Lval lv -> ChangeDoChildrenPost (Lval (renameLval vmap lv), id)
    | _       -> DoChildren

  method vstmt: stmt -> stmt visitAction = function
    | {skind = Return (Some e, loc)} ->
        begin match rvo with
          | Some rv -> ChangeDoChildrenPost (mkSetLval (Var rv, NoOffset) e loc, id)
          | None    -> E.s <| errorLoc loc "Attempting to return value from void function"
        end
    | {skind = Return (None, loc)} ->
        (* pmr: surely there's a better way to make a nop *)
        ChangeTo (mkStmt (Instr []))
    | _ -> DoChildren
end

let freshVars (fin: fundec) (vs: varinfo list): varinfo list =
  List.map (fun vf -> makeTempVar fin vf.vtype) vs

let vids (vs: varinfo list): int list =
  List.map (fun v -> v.vid) vs

let freshFunction (fin: fundec) (fd: fundec): varinfo list * varinfo list * varinfo option * block =
  let fd = copyFunction fd "copiedFun" in
    match fd.svar.vtype with
      | TFun (rt, _, _, _) ->
          let (formals, locals) = (freshVars fin fd.sformals, freshVars fin fd.slocals) in
          let vmap              = List.combine (vids fd.sformals @ vids fd.slocals) (formals @ locals)  in
          let rvo               = if isVoidType rt then None else Some (makeTempVar fin rt) in
            (formals, locals, rvo, visitCilBlock (new freshVisitor vmap rvo) fd.sbody)
      | _ -> assert false

(******************************************************************************)
(****************************** Function Inlining *****************************)
(******************************************************************************)

let rec splitCallInstrs: instr list -> instr list list = function
  | []                 -> [[]]
  | Call _ as ic :: is -> [ic] :: splitCallInstrs is
  | i :: is            ->
      match splitCallInstrs is with
        | []                     -> [[i]]
        | [Call _] as isc :: iss -> [i] :: isc :: iss
        | is :: iss              -> (i :: is) :: iss

let rec splitCallStmts: stmt list -> stmt list = function
  | []                       -> []
  | {skind = Instr is} :: ss -> (List.map (fun is -> mkStmt (Instr is)) <| splitCallInstrs is) @ splitCallStmts ss
  | s :: ss                  -> s :: splitCallStmts ss

let assertLoc p loc msg =
  if not p then E.s <| errorLoc loc msg

class inlineVisitor fds fi = object
  inherit nopCilVisitor

  method vstmt (s: stmt): stmt visitAction =
    match s.skind with
      | Instr [Call (_, Lval (Var {vname = "malloc"}, NoOffset), _, _)] ->
          DoChildren
      | Instr [Call (lvo, Lval (Var f, NoOffset), es, loc)] ->
          let (formals, locals, rvo, b) = freshFunction fi (List.assoc f.vid fds) in
          let _                         = assertLoc (List.length formals = List.length es) loc "Wrong number of parameters" in
          let set_formals               = List.map2 (fun f e -> mkSetLval (Var f, NoOffset) e loc) formals es in
          let b                         = {b with bstmts = set_formals @ b.bstmts} in
            (* pmr: patch up block structure afterward, i.e., merge blocks? *)
            begin match (lvo, rvo) with
              | (None, None)       -> ChangeTo (mkStmt <| Block b)
              | (Some lv, Some rv) -> ChangeTo (mkStmt <| Block ({b with bstmts = b.bstmts @ [mkSetLval lv (Lval (Var rv, NoOffset)) loc]}))
              | _                  -> E.s <| errorLoc loc "Assigning void return type to a variable"
            end
      | _ -> DoChildren

  method vblock (b: block): block visitAction =
    ChangeDoChildrenPost ({b with bstmts = splitCallStmts b.bstmts}, id)
end

let doGlobal fds = function
    GFun (fi, _) -> fi.sbody <- visitCilBlock (new inlineVisitor fds fi) fi.sbody
  | _            -> ()
