module FA = FixAstInterface
module C  = Cil

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

(******************************************************************************)
(************************** Parsing Cil Declarations **************************)
(******************************************************************************)

let parsePragmaDecl loc = function
  | C.Attr ("lcc_effect_decl", [C.AStr ename]) ->
    ignore <| addEffect ename
  | C.Attr ("lcc_effects_commute", [C.AStr ename1; C.AStr ename2]) ->
    addCommutativePair ename1 ename2
  | C.Attr ("lcc_effect_decl", _ ) ->
    Errormsg.s <| C.errorLoc loc "Malformed effect declaration@!"
  | C.Attr ("lcc_effects_commute", _) ->
    Errormsg.s <| C.errorLoc loc "Malformed effect commutation declaration@!"
  | _ -> ()

let parseEffectDecls cil =
  C.iterGlobals cil begin function
    | C.GPragma (ats, loc) -> parsePragmaDecl loc ats
    | _                    -> ()
  end
