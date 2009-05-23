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
      | Instr [Call (lvo, Lval (Var f, NoOffset), es, loc)] when List.mem_assoc f.vid fds ->
          let (formals, locals, rvo, b) = freshFunction fi (List.assoc f.vid fds) in
          let _                         = assertLoc (List.length formals = List.length es) loc "Wrong number of parameters" in
          let set_formals               = List.map2 (fun f e -> mkSetLval (Var f, NoOffset) e loc) formals es in
          let b                         = {b with bstmts = set_formals @ b.bstmts} in
            (* pmr: patch up block structure afterward, i.e., merge blocks? *)
            begin match (lvo, rvo) with
              | (None, None)       -> ChangeDoChildrenPost (mkStmt <| Block b, id)
              | (Some lv, Some rv) -> ChangeDoChildrenPost (mkStmt <| Block ({b with bstmts = b.bstmts @ [mkSetLval lv (Lval (Var rv, NoOffset)) loc]}), id)
              | _                  -> E.s <| errorLoc loc "Assigning void return type to a variable"
            end
      | Instr [Call (_, _, _, loc)] ->
          warnLoc loc "Unsoundly dropping recursive or forward call:@!%a@!" d_stmt s |> ignore;
          ChangeDoChildrenPost (mkStmt (Instr []), id)
      | _ -> DoChildren

  method vblock (b: block): block visitAction =
    ChangeDoChildrenPost ({b with bstmts = splitCallStmts b.bstmts}, id)
end

let doInline (file: file): unit =
  begin
    foldGlobals file begin fun fds g ->
      match g with
        | GFun (fd, _) -> fd.sbody <- visitCilBlock (new inlineVisitor fds fd) fd.sbody; (fd.svar.vid, fd) :: fds
        | _            -> fds
    end []
  end |> ignore

(******************************************************************************)
(******************************* Global Lowering ******************************)
(******************************************************************************)

module GIM = Misc.IntMap

type globalinfo = varinfo * init option

let shouldLower (v: varinfo): bool =
  v.vglob && not (isFunctionType v.vtype)

class renameVisitor (fgm: varinfo GIM.t) = object
  inherit nopCilVisitor

  method vvrbl (v: varinfo): varinfo visitAction =
    if shouldLower v then ChangeTo (GIM.find v.vid fgm) else SkipChildren
end

let rec instrsOfInit (v: varinfo) (o: offset): init -> instr list = function
  | SingleInit e             -> [Set ((Var v, o), e, v.vdecl)]
  | CompoundInit (ct, inits) -> foldLeftCompound ~implicit:true ~doinit:(fun o i _ is -> instrsOfInit v o i @ is) ~ct:ct ~initl:inits ~acc:[]

let localizeFunction (fd: fundec) (gim: globalinfo GIM.t): unit =
  let fgm   = GIM.map (fun (v, _) -> makeLocalVar fd (v.vname ^ "#") v.vtype) gim in
  let inits = GIM.fold (fun _ (v, io) is -> match io with Some i -> instrsOfInit v NoOffset i @ is | None -> is) gim [] in
    fd.sbody <- {fd.sbody with bstmts = [mkStmt (Instr inits)] @ fd.sbody.bstmts};
    fd.sbody <- visitCilBlock (new renameVisitor fgm) fd.sbody

let doLocalize (file: file): unit =
  begin
    foldGlobals file begin fun gim g ->
      match g with
        | GFun (fd, _)                       -> localizeFunction fd gim; gim
        | GVarDecl (v, _) when shouldLower v -> if not (GIM.mem v.vid gim) then GIM.add v.vid (v, None) gim else gim
        | GVar (v, i, _) when shouldLower v  -> GIM.add v.vid (v, i.init) gim
        | _                                  -> gim
    end GIM.empty
  end |> ignore

let inline (file: file): unit =
  doInline file;
  doLocalize file
