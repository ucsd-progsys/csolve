
%{
module A  = Ast
module So = A.Sort
module Sy = A.Symbol
module E  = A.Expression
module P  = A.Predicate
module C  = Constraint

let parse_error msg =
  Errormsg.error (symbol_start ()) msg

%}

%token <string> Id
%token <int> Num
%token LPAREN  RPAREN LB RB LC RC
%token EQ NE GT GE LT LE
%token AND OR NOT IMPL FORALL SEMI COLON
%token TRUE FALSE
%token EOF
%token PLUS
%token MINUS
%token TIMES 
%token DIV 
%token QM DOT ASGN
%token INT BOOL UNINT FUNC
%token SRT AXM CST SOL
%token ENV GRD LHS RHS

%start defs 

%type <Constraint.deft list>    defs 
%type <Constraint.deft>         def
%type <So.t list>               sorts 
%type <So.t>                    sort
%type <(Sy.t * So.t) list>      binds 
%type <Sy.t * So.t>             bind 
%type <Sy.t * (So.t * C.reft)>  rbind 
%type <A.pred list>             preds
%type <A.pred>                  pred
%type <A.expr list>             exprs
%type <A.expr>                  expr
%type <A.brel>                  brel 
%type <A.bop>                   bop 
%type <C.t>                     cstr
%type <C.envt>                  env
%type <C.reft>                  reft
%type <C.refa list>             refas
%type <C.refa>                  refa
%type <C.subs>                  subs


%%
defs:
                                        { [] } 
  | def DOT defs                        { $1 :: $3 }
  ;

def:
    SRT sort                            { C.Srt $2 }
  | AXM pred                            { C.Axm $2 }
  | CST cstr                            { C.Cst $2 }
  | SOL Id ASGN preds                   { C.Sol ((Sy.of_string $2), $4) }
  ;

sorts:
    sort SEMI sorts                     { $1 :: $3 }
  | sort                                { [$1] }
  ;

sort:
    INT                                 { So.Int }
  | BOOL                                { So.Bool }
  | UNINT Id                            { So.Unint ($2) }
  | FUNC sorts                          { So.Func ($2) }
  ;

binds: 
    bind SEMI binds                     { $1 :: $3 }
  | bind                                { [$1] }
  ;

bind:
  Id COLON sort                         { ((Sy.of_string $1), $3) }
  ;

rbind:
  Id COLON LC sort COLON reft RC        { ((Sy.of_string $1), ($4, $6)) } 
  ;

preds:
	pred SEMI preds                 { $1 :: $3 }
    |   pred                         	{ [ $1 ] }
;

pred:
    TRUE				{ A.pTrue }
  | FALSE				{ A.pFalse }
  | AND LB preds RB 			{ A.pAnd ($3) }
  | OR LB preds RB			{ A.pOr  ($3) }
  | NOT pred				{ A.pNot ($2) }
  | pred IMPL pred			{ A.pImp ($1, $3) }
  | LPAREN pred RPAREN			{ $2 }
  | LPAREN expr RPAREN                  { A.pBexp ($2) }
  | expr brel expr                      { A.pAtom ($1, $2, $3) }
  | FORALL binds DOT pred               { A.pForall ($2, $4) }
;

exprs:
    expr SEMI exprs                     { $1 :: $3 }
  | expr                                { [ $1 ] }
;

expr:
    Id				        { A.eVar (Sy.of_string $1) }
  | Num 				{ A.eCon (A.Constant.Int $1) }
  | expr bop expr                       { A.eBin ($1, $2, $3) }
  | Id LPAREN exprs RPAREN		{ A.eApp ((Sy.of_string $1), $3) }
  | pred QM expr COLON expr             { A.eIte ($1,$3,$5) }
  | expr DOT Id                         { A.eFld ((Sy.of_string $3), $1) }
  | LPAREN expr RPAREN                  { $2 }
  ;

brel:
    EQ                                  { A.Eq }
  | NE                                  { A.Ne }
  | GT                                  { A.Gt }
  | GE                                  { A.Ge }
  | LT                                  { A.Lt }
  | LE                                  { A.Le }
  ;

bop:
    PLUS                                { A.Plus }
  | MINUS                               { A.Minus }
  | TIMES                               { A.Times }
  | DIV                                 { A.Div }
  ;

cstr:
    ENV env GRD pred LHS reft RHS reft  { ($2, $4, $6, $8, None) }
  ;


env:
                                        { Sy.SMap.empty }
  | rbind SEMI env                      { Sy.SMap.add (fst $1) (snd $1) $3 }
  ;

reft: 
  LPAREN Id COLON refas RPAREN          { ((Sy.of_string $2), $4) }
  ;

refas:
                                        { [] }
  | refa SEMI refas                     { $1 :: $3 }
  ;
  
refa:
    pred                                { C.Conc $1 }
  | Id subs                             { C.Kvar ($2, (Sy.of_string $1)) }
  ;

subs:
                                        { [] }
  | LB Id ASGN expr RB subs             { ((Sy.of_string $2), $4) :: $6 } 


