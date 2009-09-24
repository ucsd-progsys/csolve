(* val stripcasts_of_expr: Cil.exp  -> Cil.exp
   val stripcasts_of_lval: Cil.lval -> Cil.lval *)
val purify: Cil.file -> unit
val check_pure_expr: Cil.exp -> unit
val doc_of_formatter: (Format.formatter -> 'a -> unit) -> 'a -> Pretty.doc

val bytesSizeOf : Cil.typ -> int
val ptrRefType  : Cil.typ -> Cil.typ
val typ_width   : Cil.typ -> int

val bprintf : bool -> ('a, unit, Pretty.doc) format -> 'a

val d_var       : unit -> Cil.varinfo -> Pretty.doc

module VarMap: Map.S with type key = Cil.varinfo
module VarMapPrinter: sig
  val d_map : ?dmaplet:(Pretty.doc -> Pretty.doc -> Pretty.doc) ->
    string ->
    (unit -> VarMap.key -> Pretty.doc) ->
    (unit -> 'a -> Pretty.doc) -> unit -> 'a VarMap.t -> Pretty.doc
end
