type t =
  {vtyps   : (Cil.varinfo * Ctypes.ctype) list;
   etypm   : Ctypes.ctemap;
   store   : Ctypes.store;
   anna    : Refanno.block_annotation array;
   conca   : (Refanno.cncm * Refanno.cncm) array;
   theta   : Refanno.ctab;
   nasa    : NotAliased.NASet.t list array;
   dchecks : Inferindices.dcheck list}
