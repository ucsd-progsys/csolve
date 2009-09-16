module F = Format

open Misc.Ops
open Cil

exception NoSpec

let mk_type_string loc = function
  | TVoid _           -> "int (0, true, {v | true})"
  | TInt (ik, _)      -> F.sprintf "int (%d, true, {v | true})" (bytesSizeOfInt ik)
  | TPtr _ | TArray _ -> warnLoc loc "Can't create spec for function over pointers" |> ignore; raise NoSpec
  | _                 -> warnLoc loc "Can't create spec for exotic type" |> ignore; raise NoSpec

let ctype_of_ciltype (c: Cil.typ) loc : Ctypes.ctype = failwith "TBD"

let cfun_of_fdec (fd: Cil.fdec) loc : Ctypes.cfun = failwith "TBD"

let specs_of_file cil =
  Cil.foldGlobals cil begin fun acc -> function
    | GFun (fd, loc) -> 
        try (fd.svar.vname, cfun_of_fdec fd loc) :: acc with NoSpec -> 
          let _ = warnLoc loc "Skipping spec for %s\n\n" fd.svar.vname in 
          acc
    | _ -> acc
  end [] 

let mk_spec fname = 
  let oc = open_out (fname^".spec") in
  Frontc.parse fname ()
  |> specs_of_file
  |> List.iter (fun (fn, cf) -> Pretty.fprintf oc "%s ::@. %a @.@." fn Ctypes.d_cfun cf)
  |> fun _ -> close_out oc 

let _ = Toplevel.main "genspec.opt" mk_spec
