type name
type cilenv

type cilreft = Base of C.reft T.prectype
             | Fun  of (name * cilreft) list * cilreft  


val name_of_varinfo: Cil.varinfo -> name
val nextname_of_varinfo: Cil.varinfo -> name

val ce_empty:   cilenv
val ce_add:     name 
             -> cilreft 
             -> cilenv 
             -> cilenv
val ce_find:    name 
             -> cilenv 
             -> cilreft
val ce_project: cilenv 
             -> cilenv 
             -> name list 
             -> cilenv
val ce_unroll:  name
             -> cilenv
             -> (cilenv * cilreft)

val print_ce:   Constraint.soln option 
             -> Format.formatter 
             -> cilenv 
             -> unit

val t_fresh: Cil.typ -> cilreft
val t_true: Cil.typ -> cilreft
val t_exp: cilenv -> Cil.exp -> cilreft
val t_name: cilenv -> name -> cilreft
val t_subs_exps: (name * Cil.exp) list -> cilreft -> cilreft
val t_subs_names: (name * name) list -> cilreft -> cilreft

val make_cs: cilenv 
           -> Ast.pred 
           -> cilreft 
           -> cilreft 
           -> Cil.location 
           -> Constraint.t list

val make_wfs: cilenv 
           -> cilreft 
           -> Cil.location 
           -> Constraint.wf list
