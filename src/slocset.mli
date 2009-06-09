type t

type slocindex

type sloctype = Abstract | Concrete

val empty           : t
val fresh_slocindex : sloctype -> t -> slocindex * t
val find            : slocindex -> t -> slocindex * sloctype * t
val unify           : slocindex -> slocindex -> t -> t
