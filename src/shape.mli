type final_fields_annot = Index.IndexSet.t Sloc.SlocMap.t * Index.IndexSet.t Sloc.SlocMap.t list

type t =
  {vtyps   : (Cil.varinfo * Ctypes.ctype) list;    (* variable ctypes *)
   etypm   : Ctypes.ctemap;                        (* expression ctype map *)
   store   : Ctypes.store;                         (* store ctypes *)
   anna    : Refanno.block_annotation array;
   (* next three lines do not have long to live *)
  (* conca   : (Refanno.cncm * Refanno.cncm) array;
   theta   : Refanno.ctab;
   nasa    : NotAliased.NASet.t list array;
   ffmsa   : final_fields_annot array*)}

val create : Ssa_transform.t ->
             (Cil.varinfo * Ctypes.ctype) list ->
             Ctypes.ctemap ->
             Ctypes.store ->
             Refanno.block_annotation array ->
             t
            
val anns_of_shp : t -> Refanno.annotation list
