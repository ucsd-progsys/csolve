open Cil
open Misc.Ops

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

class freshVisitor vmap rv = object
  inherit nopCilVisitor

  method vlval (lv: lval): lval visitAction =
    ChangeTo (renameLval vmap lv)

  method vexpr: exp -> exp visitAction = function
    | Lval lv -> ChangeTo (Lval (renameLval vmap lv))
    | _       -> DoChildren

  method vstmt: stmt -> stmt visitAction = function
    | {skind = Return (Some e, loc)} -> ChangeDoChildrenPost (mkSetLval (Var rv, NoOffset) e loc, fun s -> s)
    | _                              -> DoChildren
end

let freshVars (fin: fundec) (vs: varinfo list): varinfo list =
  List.map (fun vf -> makeTempVar fin vf.vtype) vs

let vids (vs: varinfo list): int list =
  List.map (fun v -> v.vid) vs

let freshFunction (fin: fundec) (fd: fundec): varinfo list * varinfo list * varinfo * block =
  let fd = copyFunction fd "copiedFun" in
    match fd.svar.vtype with
      | TFun (rt, _, _, _) ->
          let (formals, locals) = (freshVars fin fd.sformals, freshVars fin fd.slocals) in
          let vmap              = List.combine (vids fd.sformals @ vids fd.slocals) (formals @ locals)  in
          let rv                = makeTempVar fin rt in
            (formals, locals, rv, visitCilBlock (new freshVisitor vmap rv) fd.sbody)
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

class inlineVisitor fds fi = object
  inherit nopCilVisitor

  method vstmt (s: stmt): stmt visitAction =
    match s.skind with
      | Instr [Call (lvo, Lval (Var f, NoOffset), es, loc)] ->
          let (formals, locals, rv, b) = freshFunction fi (List.assoc f.vid fds) in
          let set_formals              = List.map2 (fun f e -> mkSetLval (Var f, NoOffset) e loc) formals es in
          let b                        = {b with bstmts = set_formals @ b.bstmts} in
            (* pmr: patch up block structure afterward, i.e., merge blocks? *)
            begin match lvo with
              | None    -> ChangeTo (mkStmt <| Block b)
              | Some lv -> ChangeTo (mkStmt <| Block ({b with bstmts = b.bstmts @ [mkSetLval lv (Lval (Var rv, NoOffset)) loc]}))
            end
      | _ -> SkipChildren

  method vblock (b: block): block visitAction =
    ChangeDoChildrenPost ({b with bstmts = splitCallStmts b.bstmts}, fun a -> a)
end

let doGlobal fds = function
    GFun (fi, _) -> fi.sbody <- visitCilBlock (new inlineVisitor fds fi) fi.sbody
  | _            -> ()
