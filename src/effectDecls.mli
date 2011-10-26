type t

val readEffect         : t
val writeEffect        : t

val addEffect          : string -> t
val getEffects         : unit -> t list

val addCommutativePair : t -> t -> unit
val effectsCommute     : t -> t -> bool

val nameOfEffect       : t -> FixAstInterface.name
