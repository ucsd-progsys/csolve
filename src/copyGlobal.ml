open Cil
open Misc.Ops

class funVisitor fd = object(self)
  inherit nopCilVisitor

  val shadows = Hashtbl.create 17

  method shadowGlobal glob =
    let tmp = makeTempVar fd glob.vtype in
      self#queueInstr [Set (var tmp, Lval (var glob), locUnknown)];
      tmp

  method vvrbl v =
    if v.vglob && not (isFunctionType v.vtype) then
      ChangeTo (Misc.do_memo shadows self#shadowGlobal v v)
    else
      SkipChildren

  method revertShadows =
       Hashtbl.fold (fun glob shadow is -> Set (var glob, Lval (var shadow), locUnknown) :: is) shadows []
    |> self#queueInstr

  method vstmt = function
    | {skind = Return _} -> self#revertShadows; DoChildren
    | _                  -> DoChildren
end

class globVisitor = object(self)
  inherit nopCilVisitor

  method vglob = function
    | GFun (fd, _) -> visitCilFunction (new funVisitor fd :> cilVisitor) fd |> ignore; SkipChildren
    | _            -> SkipChildren
end

let copyGlobal = visitCilFile (new globVisitor)
