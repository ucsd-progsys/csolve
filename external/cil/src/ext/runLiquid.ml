open Cil

let doLiquidC  = ref false
let liquidopts = ref ""

let (|>) x f = f x

let liquiddir = Sys.executable_name
             |> Filename.dirname
             |> Filename.dirname
             |> Filename.dirname
             |> Filename.dirname
             |> Filename.dirname

let liquid = Filename.concat (Filename.concat liquiddir "src") "main.native"

let feature : featureDescr = 
  { fd_name = "liquidc";
    fd_enabled = doLiquidC;
    fd_description = "LiquidC type inference";
    fd_extraopt = [("--liquidopts",
                    Arg.String (fun s -> liquidopts := s),
                    " Additional options for LiquidC checker")];
    fd_doit = 
    (function (f: file) ->
       let oc  = open_out "liquid.c" in
       let _   = dumpFile defaultCilPrinter oc "liquid.c" f in
       let _   = close_out oc in
       let ret = Sys.command (String.concat " " [liquid; !liquidopts; "liquid.c"]) in
         if ret = 0 then () else exit ret);
    fd_post_check = false;
  } 
