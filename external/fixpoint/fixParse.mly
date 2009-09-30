
%{
module A  = Ast
module So = A.Sort
module Sy = A.Symbol
module E  = A.Expression
module P  = A.Predicate
module C  = FixConstraint

let parse_error msg =
  Errorline.error (symbol_start ()) msg

%}

%token <string> Id
%token <int> Num
%token TAG ID 
%token BEXP
%token TRUE FALSE
%token LPAREN  RPAREN LB RB LC RC
%token EQ NE GT GE LT LE
%token AND OR NOT IMPL FORALL SEMI COLON MID
%token EOF
%token PLUS
%token MINUS
%token TIMES 
%token DIV 
%token QM DOT ASGN
%token INT PTR BOOL UNINT FUNC
%token SRT AXM CST WF SOL QUL ADP DDP
%token ENV GRD LHS RHS REF

%start defs 

%type <FixConstraint.deft list>    defs
%type <FixConstraint.deft>         def
%type <So.t list>               sorts, sortsne 
%type <So.t>                    sort
%type <(Sy.t * So.t) list>      binds, bindsne 
%type <A.pred list>             preds, predsne
%type <A.pred>                  pred
%type <A.expr list>             exprs, exprsne
%type <A.expr>                  expr
%type <A.brel>                  brel 
%type <A.bop>                   bop 
%type <C.t>                     cstr
%type <C.envt>                  env
%type <C.reft>                  reft
%type <C.refa list>             refas, refasne
%type <C.refa>                  refa
%type <C.subs>                  subs

%%
defs:
                                        { [] } 
  | def defs                            { $1 :: $2 }
  ;

def:
    SRT COLON sort                      { C.Srt $3 }
  | AXM COLON pred                      { C.Axm $3 }
  | CST COLON cstr                      { C.Cst $3 }
  | WF  COLON wf                        { C.Wfc $3 }
  | SOL COLON Id ASGN preds             { C.Sol ((Sy.of_string $3), $5) }
  | QUL Id LPAREN Id COLON sort RPAREN COLON pred  
                                        { let v = Sy.of_string $4 in
                                          C.Qul (A.Qualifier.create (Some v) $6 $9) }
  | dep                                 { C.Dep $1 }
  ;

sorts:
    LB RB                               { [] }
  | LB sortsne RB                       { $2 }
  ;

sortsne:
    sort                                { [$1] }
  | sort SEMI sortsne                   { $1 :: $3 }
  ;


sort:
    INT                                 { So.Int }
  | PTR                                 { So.Ptr }
  | BOOL                                { So.Bool }
  | UNINT Id                            { So.Unint ($2) }
  | FUNC LPAREN sorts RPAREN            { So.Func ($3) }
  ;


binds:
    LB RB                               { [] }
  | LB bindsne RB                       { $2 }
  ;

bindsne:
    bind                                { [$1] }
  | bind SEMI bindsne                   { $1::$3 }
  ;


bind:
  Id COLON sort                         { ((Sy.of_string $1), $3) }
  ;

preds:
    LB RB                               { [] }
  | LB predsne RB                       { $2 }
  ;

predsne:
    pred                                { [$1] }
  | pred SEMI predsne                   { $1 :: $3 }
;

pred:
    TRUE				{ A.pTrue }
  | FALSE				{ A.pFalse }
  | BEXP expr                           { A.pBexp $2 }
  | AND preds   			{ A.pAnd ($2) }
  | OR  preds 	        		{ A.pOr  ($2) }
  | NOT pred				{ A.pNot ($2) }
  | pred IMPL pred			{ A.pImp ($1, $3) }
  | expr brel expr                      { A.pAtom ($1, $2, $3) }
  | FORALL binds DOT pred               { A.pForall ($2, $4) }
  | LPAREN pred RPAREN			{ $2 }
  ;

exprs:
    LB RB                               { [] }
  | LB exprsne RB                       { $2 }
  ;

exprsne:
    expr                                { [$1] }
  | expr SEMI exprsne                   { $1 :: $3 }
  ;

expr:
    Id				        { A.eVar (Sy.of_string $1) }
  | Num 				{ A.eCon (A.Constant.Int $1) }
  | MINUS Num 				{ A.eCon (A.Constant.Int (-1 * $2)) }
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
  
wf:
    ENV env REF reft                              { C.make_wf $2 $4 None }
  | ENV env REF reft ID Num                       { C.make_wf $2 $4 (Some $6) }
  ;

tagsne:
  Num                                             { [$1] }
  | Num SEMI tagsne                               { $1 :: $3 }
  ;

dep:
  | ADP COLON LB tagsne RB IMPL LB tagsne RB            {C.make_dep true (Some $4) (Some $8) }
  | DDP COLON LB tagsne RB IMPL LB tagsne RB            {C.make_dep false (Some $4) (Some $8) }
  | DDP COLON TIMES IMPL LB tagsne RB                   {C.make_dep false None (Some $6) }
  | DDP COLON LB tagsne RB IMPL TIMES                   {C.make_dep false (Some $4) None }
  ;

info:
  ID Num                                          { ((Some $2), []) }
  | TAG LB tagsne RB                              { (None, $3)} 
  | ID Num TAG LB tagsne RB                       { ((Some $2), $5) }
  ;

cstr:
    ENV env GRD pred LHS reft RHS reft          { C.make_t $2 $4 $6 $8 None [] }
  | ENV env GRD pred LHS reft RHS reft info     { C.make_t $2 $4 $6 $8 (fst $9) (snd $9)}
  ;


env:
  LB RB                                 { C.env_of_bindings [] }
  | LB envne RB                         { C.env_of_bindings $2 }
  ;

envne:
    rbind                               { [$1] }
  | rbind SEMI envne                    { $1 :: $3 }
  ;

rbind: 
  Id COLON reft                         { (Sy.of_string $1, $3) }
  ;

reft: 
  LC Id COLON sort MID refas RC         { ((Sy.of_string $2), $4, $6) }
  ;

refas:
    LB RB                               { [] }
  | LB refasne RB                       { $2 }
  
refasne:
    refa                                { [$1] }
  | refa SEMI refasne                   { $1 :: $3 }
  ;
  
refa:
    pred                                { C.Conc $1 }
  | Id subs                             { C.Kvar ($2, (Sy.of_string $1)) }
  ;

subs:
                                        { [] }
  | LB Id ASGN expr RB subs             { ((Sy.of_string $2), $4) :: $6 } 


