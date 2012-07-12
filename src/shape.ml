module M = FixMisc

open Cil
open M.Ops

type final_fields = Index.IndexSet.t Sloc.SlocMap.t

type final_fields_annot = Index.IndexSet.t Sloc.SlocMap.t * final_fields list

type t =
  {vtyps   : (Cil.varinfo * Ctypes.ctype) list;
   etypm   : Ctypes.ctemap;
   store   : Ctypes.store;
   anna    : Refanno.block_annotation array;
(*   conca   : (Refanno.cncm * Refanno.cncm) array;
   theta   : Refanno.ctab;
   nasa    : NotAliased.NASet.t list array;
   ffmsa   : final_fields_annot array*)}

let create sci vtyps etypm sto =
  let cfg     = sci.Ssa_transform.cfg in
  let nblocks = Array.length cfg.Ssa.blocks in
  let res     = {vtyps = vtyps;
                 etypm = etypm;
                 store = sto;
                 anna  = Array.create nblocks [];} in
  let _ = Array.iteri begin fun i _ ->
    match cfg.Ssa.blocks.(i).Ssa.bstmt.skind with
    | Instr is -> List.length is
               |> fun x -> res.anna.(i) <- M.list_make x []
    |  _       -> () end res.anna in
  res
 


