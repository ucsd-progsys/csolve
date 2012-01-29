module FA = FixAstInterface
module C  = Cil

module SS = Ast.Symbol.SSet

module Misc = FixMisc open Misc.Ops

type t = FA.name 

(* API *)
let readEffect  = FA.eff_read
let writeEffect = FA.eff_write


let effects = ref (SS.of_list [readEffect; writeEffect])
let commutativeEffects = ref [(readEffect, readEffect)]

let addEffect x =
  x |> FA.name_of_string 
    >> (fun e -> effects := SS.add e !effects)

let canonize e1 e2 = 
  if e1 < e2 then (e1, e2) else (e2, e1)

let addCommutativePair e1 e2 =
  commutativeEffects := (canonize e1 e2) :: !commutativeEffects


(* API *)
let nameOfEffect x = x 
let getEffects ()  = SS.elements !effects
let effectsCommute = fun e1 e2 -> List.mem (canonize e1 e2) !commutativeEffects


(******************************************************************************)
(************************** Parsing Cil Declarations **************************)
(******************************************************************************)

let parsePragmaDecl loc = function
  | C.Attr ("csolve_effect_decl", [C.AStr ename]) ->
    ignore <| addEffect ename
  | C.Attr ("csolve_effects_commute", [C.AStr ename1; C.AStr ename2]) ->
    addCommutativePair (addEffect ename1) (addEffect ename2)
  | C.Attr ("csolve_effect_decl", _ ) ->
    Errormsg.s <| C.errorLoc loc "Malformed effect declaration@!"
  | C.Attr ("csolve_effects_commute", _) ->
    Errormsg.s <| C.errorLoc loc "Malformed effect commutation declaration@!"
  | _ -> ()

(* API *)
let parseEffectDecls cil =
  C.iterGlobals cil begin function
    | C.GPragma (ats, loc) -> parsePragmaDecl loc ats
    | _                    -> ()
  end
