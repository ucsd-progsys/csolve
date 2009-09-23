(* val stripcasts_of_expr: Cil.exp  -> Cil.exp
   val stripcasts_of_lval: Cil.lval -> Cil.lval *)
val purify: Cil.file -> unit
val check_pure_expr: Cil.exp -> unit
val doc_of_formatter: (Format.formatter -> 'a -> unit) -> 'a -> Pretty.doc

val bytesSizeOf : Cil.typ -> int
val ptrRefType  : Cil.typ -> Cil.typ
val typ_width   : Cil.typ -> int

val bprintf : bool -> ('a, unit, Pretty.doc) format -> 'a

module VarMap: Map.S with type key = Cil.varinfo
