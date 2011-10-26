module FA = FixAstInterface

open Misc.Ops

type t = string

let effects = ref []

let addEffect eff =
  effects := eff :: !effects;
  eff

let getEffects () =
  !effects

let readEffect  = "EREAD"
let writeEffect = "EWRITE"

let _ = List.map addEffect [readEffect; writeEffect]

let commutativeEffects = ref []

let addCommutativePair ename1 ename2 =
  commutativeEffects := (ename1, ename2) :: !commutativeEffects

let effectsCommute ename1 ename2 =
  List.mem (ename1, ename2) !commutativeEffects

let _ = addCommutativePair readEffect readEffect

let nameOfEffect ename =
  FA.name_of_string ename
