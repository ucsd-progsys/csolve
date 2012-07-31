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

let create sci vtyps etypm sto bas =
  let cfg     = sci.Ssa_transform.cfg in
  let nblocks = Array.length cfg.Ssa.blocks in
  {  vtyps = vtyps;
     etypm = etypm;
     store = sto;
     anna  = bas; }
                   
let anns_of_shp t = Array.to_list t.anna
                 |> List.concat
                 |> List.concat
