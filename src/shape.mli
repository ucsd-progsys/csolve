type final_fields_annot = Index.IndexSet.t Sloc.SlocMap.t * Index.IndexSet.t Sloc.SlocMap.t list

type t =
  {vtyps   : (Cil.varinfo * Ctypes.ctype) list;    (* variable ctypes *)
   etypm   : Ctypes.ctemap;                        (* expression ctype map *)
   store   : Ctypes.store;                         (* store ctypes *)
   anna    : Refanno.block_annotation array;
   conca   : (Refanno.cncm * Refanno.cncm) array;
   theta   : Refanno.ctab;
   nasa    : NotAliased.NASet.t list array;
   ffmsa   : final_fields_annot array}
