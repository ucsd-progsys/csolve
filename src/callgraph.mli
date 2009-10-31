type t = Cil.varinfo list list

val sccs : Cil.file -> t
val reach: Cil.file -> Cil.varinfo -> Cil.varinfo list

