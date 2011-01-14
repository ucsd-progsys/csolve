type final_fields = Ctypes.IndexSet.t Sloc.SlocMap.t

type final_fields_annot = Ctypes.IndexSet.t Sloc.SlocMap.t * final_fields list

type t =
  {vtyps   : (Cil.varinfo * Ctypes.ctype) list;
   etypm   : Ctypes.ctemap;
   store   : Ctypes.store;
   anna    : Refanno.block_annotation array;
   bdcks   : Inferindices.block_dchecks array;
   conca   : (Refanno.cncm * Refanno.cncm) array;
   theta   : Refanno.ctab;
   nasa    : NotAliased.NASet.t list array;
   ffmsa   : final_fields_annot array}
