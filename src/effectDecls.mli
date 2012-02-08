type t
val readEffect         : t
val writeEffect        : t
val nameOfEffect       : t -> FixAstInterface.name
val getEffects         : unit -> t list
val effectsCommute     : t -> t -> bool
val parseEffectDecls   : Cil.file -> unit
