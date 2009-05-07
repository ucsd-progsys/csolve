val sort_of_typ: Cil.typ -> Ast.Sort.t
val expr_of_var: Cil.varinfo -> Ast.expr
val expr_of_lval: Cil.lval -> Ast.expr
val expr_of_cilexp: Cil.exp -> Ast.expr
val pred_of_cilexp: Cil.exp -> Ast.pred
