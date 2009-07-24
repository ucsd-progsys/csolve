(******************************************************************************)
(******************* Assign Return Values to Local Vars Only ******************)
(******************************************************************************)

open Cil
open Misc.Ops

class returnVisitor fd = object(self)
  inherit nopCilVisitor

  method vinst = function
    | Call (None, _, _, _)
    | Call (Some (Var _, NoOffset), _, _, _) -> SkipChildren
    | Call (Some lv, e, es, loc) ->
        let tlv = (Var (makeTempVar fd (typeOfLval lv)), NoOffset) in
          ChangeTo ([Call (Some tlv, e, es, loc); Set (lv, Lval tlv, loc)]);
    | _ -> SkipChildren
end

let simpleret (f: file) =
  iterGlobals f begin function
    | GFun (fd, _) -> visitCilFunction (new returnVisitor fd) fd |> ignore
    | _            -> ()
  end
