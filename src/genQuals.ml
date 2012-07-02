module IS = FixMisc.IntSet
module A  = Ast
module FA = FixAstInterface

open FixMisc.Ops

class pointerSizesVisitor = object (self)
  inherit Cil.nopCilVisitor

  val sizes = ref IS.empty

  method private addTypeSize t =
    sizes := IS.add (CilMisc.bytesSizeOf t) !sizes

  method vtype t = match Cil.unrollType t with
    | Cil.TPtr (t, _) -> self#addTypeSize t; Cil.DoChildren
    | Cil.TArray _    -> self#addTypeSize t; Cil.DoChildren
    | _               -> Cil.DoChildren

  method getSizes =
    !sizes
end

let sizePred evv sz =
  A.pImp (A.pAtom (evv, A.Ne, A.zero),
          A.pAtom (FA.eApp_bend evv, A.Ge, A.eBin (FA.eApp_bbegin evv, A.Plus, A.eInt sz)))

let pointerSizeQualifier sz =
  let name = A.Symbol.of_string ("SIZE" ^ string_of_int sz) in
  let vv   = A.Symbol.of_string "vv" in
  let sort = A.Sort.t_ptr (A.Sort.Lvar 0) in
       sz 
    |> sizePred (A.eVar vv)
    |> Qualifier.create name vv sort []

let quals_of_cil cil =
  let psv = new pointerSizesVisitor in
    Cil.visitCilFileSameGlobals (psv :> Cil.cilVisitor) cil;
          psv#getSizes
      |>  IS.elements
      |>  List.filter ((<) 1)
      |>: pointerSizeQualifier 
      |>  Qualifier.normalize
