open Cil
open Misc.Ops

module CM = CilMisc
module P  = Pretty

let globname = "__g__"

class bodyVisitor (gbc: compinfo) (glob: varinfo) = object(self)
  inherit nopCilVisitor

  method vlval = function
    | Var vi, ofs when vi.vglob && not (isFunctionType vi.vtype) ->
        let blobfield = getCompField gbc vi.vname in
        let fieldlv   = (Mem (Lval (var glob)), Field (blobfield, NoOffset)) in
          begin match unrollType vi.vtype with
            | TComp _  -> ChangeDoChildrenPost ((Mem (Lval fieldlv), ofs), id)
            | TArray _ ->
                begin match ofs with
                  | Index (e, o) -> ChangeDoChildrenPost ((Mem (BinOp (PlusPI, Lval fieldlv, e, blobfield.ftype)), o), id)
                  | NoOffset     -> ChangeDoChildrenPost (fieldlv, id)
                  | _            -> assert false
                end
            | _ -> ChangeDoChildrenPost (addOffsetLval ofs fieldlv, id)
          end
    | _ -> DoChildren

  method vinst = function
    | Call (lvo, ((Lval (Var vi, NoOffset)) as f), args, loc) when CM.definedHere vi && vi.vglob ->
        ChangeDoChildrenPost ([Call (lvo, f, Lval (var glob) :: args, loc)], id)
    | Call (_, Lval (Var vi, NoOffset), _, _) when vi.vglob ->
        SkipChildren
    | Call _ ->
        failwith "Can't handle non-var function pointers"
    | _ -> DoChildren
end

class fileVisitor (gbc: compinfo) = object(self)
  inherit nopCilVisitor

  method vglob = function
    | GFun (fd, loc) when not (fd.svar.vname = "main") ->
        let glob = makeFormalVar fd ~where:"^" globname (TPtr (TComp (gbc, []), [])) in
          ChangeTo [GFun (visitCilFunction (new bodyVisitor gbc glob) fd, loc)]
    | _ -> SkipChildren
end

let allocate (malloc: exp) (lv: lval) (t: typ) (count: exp) (loc: location): instr =
  Call (Some lv, malloc, [BinOp (Mult, integer <| CM.bytesSizeOf t, count, !typeOfSizeOf)], loc)

let rec instrsOfInit (vi: varinfo) (lv: lval): init -> instr list = function
  | SingleInit e            -> [Set (lv, e, vi.vdecl)]
  | CompoundInit (_, inits) -> Misc.flap (fun (off, init) -> instrsOfInit vi (addOffsetLval off lv) init) inits

let instrsOfGlobalInit (malloc: exp) (vi: varinfo) (lv: lval) (ini: init): instr list =
  match ini with
    | CompoundInit (TArray (t, Some elen, _), _) -> allocate malloc lv t elen vi.vdecl :: instrsOfInit vi lv ini
    | CompoundInit (TComp _ as t, _)             -> allocate malloc lv t one vi.vdecl :: instrsOfInit vi lv ini
    | _                                          -> instrsOfInit vi lv ini

(* Awful name! *)
let globalizeType: typ -> typ = function
  | TArray (t, _, attrs) -> TPtr (t, attrs)
  | TComp _ as t         -> TPtr (t, [])
  | t                    -> t

let globalBlobComp (f: file): compinfo =
  mkCompInfo true "__globblob__"
    (fun _ -> foldGlobals f (fun gs -> function GVar (vi, _, _) -> vi :: gs | _ -> gs) []
           |> List.map (fun vi -> (vi.vname, globalizeType vi.vtype, None, [], vi.vdecl)))
    []

let collectGlobalInits (malloc: exp) (gbc: compinfo) (gb: varinfo) (instrss: instr list list): global -> instr list list = function
  | GVar (vi, ({init = Some i}), _) ->
      let lv = mkMem (Lval (var gb)) (Field (getCompField gbc vi.vname, NoOffset)) in
        instrsOfGlobalInit malloc vi lv i :: instrss
  | _ -> instrss

let makeGlobalInit (f: file) (gbc: compinfo): fundec =
  let malloc = Lval (var <| findOrCreateFunc f "malloc" (TFun (voidPtrType, Some [("size", !typeOfSizeOf, [])], false, []))) in
  let gbtyp  = TComp (gbc, []) in
  let gbtptr = TPtr (gbtyp, []) in
  let gbinit = emptyFunction "__globinit__" in
  let gb     = makeLocalVar gbinit "g" gbtptr in
    setFunctionType gbinit (TFun (gbtptr, Some [], false, []));
    gbinit.sbody <-
      begin
        Instr begin
            allocate malloc (var gb) gbtyp one locUnknown
        :: (foldGlobals f (collectGlobalInits malloc gbc gb) [] |> List.concat)
        end |> mkStmt
      end
    :: [mkStmt (Return (Some (Lval (var gb)), locUnknown))]
    |> mkBlock;
    gbinit

let findMain (f: file): fundec =
  match foldGlobals f (fun mo -> function GFun (fd, _) when fd.svar.vname = "main" -> Some fd | _ -> mo) None with
    | Some fd -> fd
    | None    -> failwith "No main function found!"

let unglobal (f: file): unit =
  let gbc      = globalBlobComp f in
  let globinit = makeGlobalInit f gbc in
  let main     = findMain f in
  let mainglob = makeLocalVar main globname (TPtr (TComp (gbc, []), [])) in
    main.sbody.bstmts <- mkStmt (Instr [Call (Some (var mainglob), Lval (var globinit.svar), [], locUnknown)]) :: main.sbody.bstmts;
    visitCilFunction (new bodyVisitor gbc mainglob) main |> ignore;
    visitCilFile (new fileVisitor gbc) f;
    f.globals <- GCompTag (gbc, locUnknown)
              :: GFun (globinit, locUnknown)
              :: List.filter (function GVar _ -> false | _ -> true) f.globals

let main fname =
  let file = Frontc.parse fname () in
  let _    = lineDirectiveStyle := None in
    unglobal file;
    dumpFile defaultCilPrinter stdout "" file

let _ = Toplevel.mk_options "unglobal" () |> main
