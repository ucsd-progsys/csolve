%{
module A  = Ast
module Sy = A.Symbol
module SM = Misc.StringMap
module Ct = Ctypes
module FI = FixInterface

let parse_error msg =
  Errorline.error (symbol_start ()) msg

let store_of_slocbinds sbs = 
  List.fold_left (fun slm (x,y) -> Ct.SLM.add x y slm) Ct.SLM.empty sbs

let ldesc_of_plocbinds pbs = 
  List.fold_left (fun ld (x,y) -> Ct.LDesc.add x y ld) Ct.LDesc.empty pbs

let mk_cfun qslocs args reto inst outst = 
  { Ct.qlocs   = qslocs; 
    Ct.args    = args;
    Ct.ret     = reto;
    Ct.abs_in  = inst;
    Ct.abs_out = outst;
    Ct.con_in  = Ct.SLM.empty;
    Ct.con_out = Ct.SLM.empty;
  }

%}

%token <string> Id
%token <int> Num
%token LPAREN  RPAREN LB RB LC RC
%token EQ NE GT GE LT LE
%token AND OR NOT IMPL FORALL COMMA SEMI COLON DCOLON MAPSTO MID
%token ARG RET INST OUTST
%token TRUE FALSE
%token EOF
%token PLUS
%token MINUS
%token TIMES 
%token DIV 
%token QM DOT ASGN
%token INT BOOL UNINT FUNC
%token SRT AXM CST WF SOL QUL
%token ENV GRD LHS RHS REF

%start specs 

%type <(string * FixInterface.refcfun) list>    specs 

%%
specs:
                                        { [] }
  | spec specs                          { $1 :: $2 }
  ;

spec:
    Id DCOLON 
    FORALL slocs
    ARG    binds 
    RET    reftype
    INST   refstore
    OUTST  refstore                     { ($1, (mk_cfun $4 $6 (Some $8) $10 $12)) }
  
  | Id DCOLON 
    FORALL slocs
    ARG    binds 
    INST   refstore
    OUTST  refstore                     { ($1, (mk_cfun $4 $6 None $8 $10)) }
    ;

slocs:
    LB RB                               { [] }
  | LB slocsne RB                       { $2 }
  ;

slocsne:
    sloc                                { $1 :: [] }
  | sloc SEMI slocsne                   { $1 :: $3 }
  ;

sloc:
  Num                                   { Ct.ALoc $1 }
  ;

refstore:
    LB RB                               { Ct.SLM.empty }
  | LB slocbindsne RB                   { store_of_slocbinds $2 }
  ;

slocbindsne:
    slocbind                            { [$1] } 
  | slocbind SEMI slocbindsne           { $1 :: $3 }
  ;

slocbind:
  sloc MAPSTO indbinds                  { ($1, Ct.LDesc.create $3) } 
  ;

indbinds:
    LB RB                               { [] }
  | LB indbindsne RB                    { $2 }
  ;

indbindsne:
    indbind                             { [$1] }
  | indbind SEMI indbindsne             { $1 :: $3 }
  ;

indbind:
    index COLON reftype                 { ($1, $3) }
  ;

reftype: 
  LC Id COLON ctype MID pred RC         { FI.t_pred (Sy.of_string $2) $4 $6 }
  ;

ctype:
    INT LPAREN Num COMMA index RPAREN   { Ct.CTInt ($3, $5) }
  | REF LPAREN sloc COMMA index RPAREN  { Ct.CTRef ($3, $5) }
  ;

index:
    Num                                 { Ct.IInt $1 }
  | Num LB Num RB                       { Ct.ISeq ($1, $3) }
  | TRUE                                { Ct.ITop }
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
  Id COLON reftype                      { ((Sy.of_string $1), $3) }
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
