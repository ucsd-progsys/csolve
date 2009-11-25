open Cil
open Misc.Ops

class funVisitor fd = object(self)
  inherit nopCilVisitor

  val shadows = Hashtbl.create 17

  method vvrbl v =
    if v.vglob && not (isFunctionType v.vtype) then
      ChangeTo (Misc.do_memo shadows (fun glob -> makeTempVar fd glob.vtype) v v)
    else
      SkipChildren

  method revertShadows =
       Hashtbl.fold (fun glob shadow is -> Set (var glob, Lval (var shadow), locUnknown) :: is) shadows []
    |> self#queueInstr

  method vstmt = function
    | {skind = Return _} -> self#revertShadows; DoChildren
    | _                  -> DoChildren

  method addShadows =
    Instr (Hashtbl.fold (fun glob shadow is -> Set (var shadow, Lval (var glob), locUnknown) :: is) shadows [])

  method vfunc fd =
    ChangeDoChildrenPost (fd, fun fd -> fd.sbody.bstmts <- (mkStmt self#addShadows) :: fd.sbody.bstmts; fd)
end

class globVisitor = object(self)
  inherit nopCilVisitor

  method vglob = function
    | GFun (fd, _) -> visitCilFunction (new funVisitor fd :> cilVisitor) fd |> ignore; SkipChildren
    | _            -> SkipChildren
end

let copyGlobal = visitCilFile (new globVisitor)
