(* val stripcasts_of_expr: Cil.exp  -> Cil.exp
   val stripcasts_of_lval: Cil.lval -> Cil.lval *)
val purify: Cil.file -> unit
val check_pure_expr: Cil.exp -> unit
val doc_of_formatter: (Format.formatter -> 'a -> unit) -> 'a -> Pretty.doc

val bytesSizeOf : Cil.typ -> int
val ptrRefType  : Cil.typ -> Cil.typ

val bprintf : bool -> ('a, unit, Pretty.doc) format -> 'a
