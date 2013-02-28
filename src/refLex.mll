(*
 * Copyright © 1990-2002 The Regents of the University of California. All rights reserved. 
 *
 * Permission is hereby granted, without written agreement and without 
 * license or royalty fees, to use, copy, modify, and distribute this 
 * software and its documentation for any purpose, provided that the 
 * above copyright notice and the following two paragraphs appear in 
 * all copies of this software. 
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY 
 * FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES 
 * ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN 
 * IF THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY 
 * OF SUCH DAMAGE. 
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES, 
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY 
 * AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED HEREUNDER IS 
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION 
 * d 
 * TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 *
 *)

{
  module E = Errorline
  open E
  open RefParse 
	      
  open FixMisc.Ops

  let lexerror msg lexbuf = 
    E.error (Lexing.lexeme_start lexbuf) msg
     
}

let digit    = ['0'-'9']
let letdig   = ['0'-'9' 'a'-'z' 'A'-'Z' '_' '@' '#']
let alphlet  = ['A'-'Z' 'a'-'z' '~' '_' ]
let capital  = ['A'-'Z']
let small    = ['a'-'z' '$' '_']
let ws       = [' ' '\009' '\012']
let pathname = ['a'-'z' 'A'-'Z' '0'-'9' '.' '/' '\\' '-']

rule token = parse
    ['\r''\t'' ']       { token lexbuf}
  | '\n'		{ begin
			    E.startNewline (Lexing.lexeme_end lexbuf);
                            Lexing.new_line lexbuf;
			    token lexbuf 
			  end }
  | "/*"[^'/']"*/"      { token lexbuf }
  | "//"[^'!''\n']*'\n'
                        { begin
                            E.startNewline (Lexing.lexeme_end lexbuf);
			    token lexbuf
                          end }
  | '['                 { LB }
  | ']'                 { RB }
  | '('			{ LPAREN }
  | ')'			{ RPAREN }
  | '{'			{ LC }
  | '}'			{ RC }
  | '~'                 { NOT }
  | ';'                 { SEMI }
  | ','                 { COMMA }
  | ':'                 { COLON }
  | '|'                 { MID }
  | '+'                 { PLUS }
  | '-'                 { MINUS }
  | "+/-"               { PLUSMINUS }
  | '*'                 { TIMES }
  | "/"                 { DIV }
  | '?'                 { QM }
  | '.'                 { DOT }
  | "false"             { FALSE }
  | "true"              { TRUE }
  | "|->"               { MAPSTO }
  | "<:"                { PCOLON }
  | "::"                { DCOLON }
  | "|-"                { HASTYPE }
  | ":="                { ASGN }
  | "&&"                { AND }
  | "||"                { OR  }
  | "!="		{ NE }
  | "="		        { EQ }
  | "<="		{ LE }
  | "<"		        { LT }
  | ">="		{ GE }
  | ">"		        { GT }
  | "=>"                { IMPL }
  | "A"(digit)+	        { let str = Lexing.lexeme lexbuf in 
                          ABS (FixMisc.suffix_of_string str 1 |> int_of_string) } 
  | "C"(digit)+	        { let str = Lexing.lexeme lexbuf in 
                          CONC (FixMisc.suffix_of_string str 1 |> int_of_string) }
  | "mod"               { MOD }
  | "int"               { INT }
  | "ptr"               { PTR }
  | "<fun>"              { LFUN }
  | "bool"              { BOOL }
  | "final"             { FINAL }
  | "func"              { FUNC }
  | "global"            { GLOBAL }
  | "sort"              { SRT }
  | "axiom"             { AXM }
  | "constraint"        { CST }
  | "wf"                { WF }
  | "solution"          { SOL }
  | "qualif"            { QUL }
  | "arg"               { ARG }
  | "ret"               { RET }
  | "store"             { ST }
  | "store_in"          { INST }
  | "store_out"         { OUTST }
  | "grd"               { GRD }
  | "lhs"               { LHS }
  | "rhs"               { RHS }
  | "ref"               { REF }
  | "fref"              { FREF }
  | "loc"               { LOCATION }
  | "write*"            { WRITE }
  | "read*"             { READ }
  | "effects"           { EFFECTS }
  | '-'? (digit)+	{ Num (int_of_string (Lexing.lexeme lexbuf)) }
  | (alphlet)letdig*	{ Id    (Lexing.lexeme lexbuf) }
  | eof			{ EOF }
  | _			{ 
                          begin
                            lexerror ("Illegal Character '" ^ 
                                      (Lexing.lexeme lexbuf) ^ "'") lexbuf;
			    token lexbuf
			  end }
