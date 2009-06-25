module F = Format

open Misc.Ops
open Cil

exception NoSpec

let mk_type_string loc = function
  | TVoid _           -> "int (0, true, {v | true})"
  | TInt (ik, _)      -> F.sprintf "int (%d, true, {v | true})" (bytesSizeOfInt ik)
  | TPtr _ | TArray _ -> warnLoc loc "Can't create spec for function over pointers" |> ignore; raise NoSpec
  | _                 -> warnLoc loc "Can't create spec for exotic type" |> ignore; raise NoSpec

let mk_spec fname =
  let cil = Frontc.parse fname () in
    Cil.iterGlobals cil begin function
      | GFun (fd, _) ->
          let loc = fd.svar.vdecl in
            begin try

              let args            = List.map (fun f -> f.vname ^ ": " ^ mk_type_string loc f.vtype) fd.sformals in
              let (ret, _, _, _)  = splitFunctionType fd.svar.vtype in
                F.printf
                  "%s ::@.  forall []@.  arg (%a)@.  ret %s@.  store_in []@.  store_out []@.@."
                  fd.svar.vname
                  (Misc.pprint_many false ", " (fun ppf arg -> F.fprintf ppf "%s" arg)) args
                  (mk_type_string loc ret)
            with NoSpec ->
              warnLoc loc "Skipping spec for %s\n\n" fd.svar.vname |> ignore
            end
      | _ -> ()
    end

let mk_options () =
  let fs = ref [] in
  let us = "Usage: genspec <options> [source-file] \n options are:" in
  let _  = Arg.parse Constants.arg_spec (fun s -> fs := s::!fs) us in
  match !fs with
  | [fn] -> fn
  | []   -> assertf "Bug: No input file specified!"
  | _    -> assertf "Bug: More than one input file specified!"

let main () =
  mk_spec <| mk_options ()

let _ = main ()
