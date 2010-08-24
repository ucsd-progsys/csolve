type final_fields_annot = Ctypes.PlocSet.t Sloc.SlocMap.t list

type t =
  {vtyps   : (Cil.varinfo * Ctypes.ctype) list;
   etypm   : Ctypes.ctemap;
   store   : Ctypes.store;
   anna    : Refanno.block_annotation array;
   conca   : (Refanno.cncm * Refanno.cncm) array;
   theta   : Refanno.ctab;
   nasa    : NotAliased.NASet.t list array;
   ffmsa   : final_fields_annot array;
   dchecks : Inferindices.dcheck list}
