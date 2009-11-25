(* val stripcasts_of_expr: Cil.exp  -> Cil.exp
   val stripcasts_of_lval: Cil.lval -> Cil.lval *)
val purify: Cil.file -> unit
val unfloat: Cil.file -> unit
val check_pure_expr: Cil.exp -> unit
val doc_of_formatter: (Format.formatter -> 'a -> unit) -> 'a -> Pretty.doc

val bytesSizeOf      : Cil.typ -> int
val bytesSizeOfFloat : Cil.fkind -> int
val bytesOffset      : Cil.typ -> Cil.offset -> int
val ptrRefType       : Cil.typ -> Cil.typ
val isVararg         : Cil.typ -> bool
val typ_width        : Cil.typ -> int
val int_width        : int
val short_width      : int
val char_width       : int

val has_array_attr     : Cil.attributes -> bool
val has_pos_attr       : Cil.attributes -> bool
val has_unchecked_attr : Cil.attributes -> bool

val is_unchecked_ptr_type : Cil.typ -> bool

val id_of_ciltype    : Cil.typ -> int option -> string

val bprintf : bool -> ('a, unit, Pretty.doc) format -> 'a

val definedHere : Cil.varinfo -> bool

val d_var       : unit -> Cil.varinfo -> Pretty.doc

module VarMap: Map.S with type key = Cil.varinfo
module VarMapPrinter: sig
  val d_map : ?dmaplet:(Pretty.doc -> Pretty.doc -> Pretty.doc) ->
    string ->
    (unit -> VarMap.key -> Pretty.doc) ->
    (unit -> 'a -> Pretty.doc) -> unit -> 'a VarMap.t -> Pretty.doc
end
val assertLoc : Cil.location -> bool -> ('a, unit, Pretty.doc) format -> 'a
val vm_print_keys : unit -> 'a VarMap.t -> Pretty.doc
val vm_of_list    : (Cil.varinfo * 'a) list -> 'a VarMap.t
val vm_to_list    : 'a VarMap.t -> (Cil.varinfo * 'a) list
