

%%
(* required declarations *)
%name a2

%term
   IMPLIES | RPAREN | LPAREN | EOF| TERM |CONST |NOT 
  | AND | OR |XOR |EQUALS| IF| THEN| ELSE|ID of string

%nonterm formula of AST.fo| program of AST.fo option|statement of AST.fo|START of AST.fo |seq of AST.fo

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)

(*%left SUB PLUS*)

%right  IF THEN ELSE IMPLIES 
%left AND OR XOR EQUALS
%right NOT
  (* %nonassoc*)
%start START

%verbose

%%

  START:  program (TextIO.output(TextIO.stdOut,"START");valOf program)

      
  program: seq ( SOME seq)
  | (NONE)
  
  seq: statement program (if isSome program then (statement; valOf(program)) else (statement))
  
  formula: ID (TextIO.output(TextIO.stdOut," formula => ID ");AST.Id(ID))
  |LPAREN formula RPAREN (TextIO.output(TextIO.stdOut," formula => LPAREN formula RPAREN ");formula)
  |CONST (TextIO.output(TextIO.stdOut," formula => CONST ");AST.CONST)
  |NOT formula (TextIO.output(TextIO.stdOut," formula => NOT formula ");AST.Unexp(AST.NOT,formula))
  |formula OR formula (TextIO.output(TextIO.stdOut," formula => formula OR formula ");AST.BinExp(AST.OR, formula1,  formula2))
  |formula XOR formula (TextIO.output(TextIO.stdOut," formula => formula XOR formula ");AST.BinExp(AST.XOR, formula1,  formula2))
  |formula AND formula (TextIO.output(TextIO.stdOut," formula => formula AND formula ");AST.BinExp(AST.AND, formula1,  formula2))
  |formula EQUALS formula (TextIO.output(TextIO.stdOut," formula => formula EQUALS formula ");AST.BinExp(AST.EQUALS, formula1,  formula2))
  |formula IMPLIES formula (TextIO.output(TextIO.stdOut," formula => formula IMPLIES formula" );AST.BinExp(AST.IMPLIES, formula1,  formula2))
  |IF formula THEN formula ELSE formula (TextIO.output(TextIO.stdOut," formula => IIF formula THEN formula ELSE formula ");AST.Triexp(AST.IF,formula1,AST.THEN, formula2,AST.ELSE,formula3))
  
  statement: formula TERM (TextIO.output(TextIO.stdOut," statement => formula  ");formula)

 
  
  
  
  
  
  
  
  
