open Cil
open Misc.Ops

let fresh_ptr_name =
  let counter = ref 0 in
    fun () -> incr counter; "nullptr" ^ string_of_int !counter

class funVisitor fd = object(self)
  inherit nopCilVisitor

  method vexpr = function
    | CastE (TPtr (t, attrs), (Const (CInt64 (i, _, _)) as cz)) when i = Int64.zero ->
        let tptr = TPtr (t, addAttribute (Attr (fresh_ptr_name (), [])) attrs) in
          ChangeDoChildrenPost (CastE (tptr, cz), id)
    | _ -> DoChildren
end

class globVisitor = object(self)
  inherit nopCilVisitor

  method vglob = function
    | GFun (fd, _) -> visitCilFunction (new funVisitor fd :> cilVisitor) fd |> ignore; SkipChildren
    | _            -> SkipChildren
end

let nameNullPtrs = visitCilFile (new globVisitor)
