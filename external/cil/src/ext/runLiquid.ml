open Cil

let doLiquidC = ref false

let feature : featureDescr = 
  { fd_name = "liquidc";
    fd_enabled = doLiquidC;
    fd_description = "LiquidC type inference";
    fd_extraopt = [];
    fd_doit = 
    (function (f: file) ->
       let oc  = open_out "liquid.c" in
       let _   = dumpFile defaultCilPrinter oc "liquid.c" f in
       let _   = close_out oc in
       let ret = Sys.command "/home/pmr/src/liquidc/src/main.native liquid.c" in
         if ret = 0 then () else exit ret);
    fd_post_check = false;
  } 
