module SM = Misc.StringMap
module VM = CilMisc.VarMap
module ST = Ssa_transform
module FI = FixInterface
module P  = Pretty

open Cil
open Misc.Ops

let infer_spec fn =
  let cil  = Toplevel.cil_of_file fn in
  (* pmr: this is not meant to handle any specs for functions actually present in the file! *)
  (* pmr: consider the above extra credit... *)
  let spec = Toplevel.spec_of_file fn in
       Inferctypes.specs_of_file spec cil
    |> P.printf "SPEC:\n\n%a" (P.d_list "\n\n" (fun () (fn, cf) -> P.dprintf "%s ::\n%a" fn Ctypes.d_cfun cf))

let _ = Toplevel.main "ispec" infer_spec
