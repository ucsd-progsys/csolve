module SM = Misc.StringMap
module VM = CilMisc.VarMap
module ST = Ssa_transform
module FI = FixInterface

open Cil
open Misc.Ops

let infer_spec fn =
  let cil  = Toplevel.cil_of_file fn in
  let cg   = Callgraph.sccs cil in
  let scis = cil |> ST.scis_of_file |> List.fold_left (fun scim sci -> VM.add sci.ST.fdec.svar sci scim) VM.empty in
  (* pmr: this is not meant to handle any specs for functions actually present in the file! *)
  (* pmr: consider the above extra credit... *)
  let spec = Toplevel.spec_of_file fn in
  let env  = SM.fold (fun fn (rf, _) vm -> VM.add (findOrCreateFunc cil fn voidType) (FI.cfun_of_refcfun rf) vm) spec VM.empty in
    Inferctypes.annot_shapes env cg scis

let _ = Toplevel.main "ispec" infer_spec
