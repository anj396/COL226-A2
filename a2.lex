structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val pos = ref 0
  val eof = fn () => Tokens.EOF(!pos, !pos)
  val error = fn (e, l:int, _) => TextIO.output(TextIO.stdOut,"line " ^ (Int.toString l) ^ ": " ^ e ^ "\n")

  fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))
  
%%
%header (functor a2LexFun(structure Tokens:a2_TOKENS));

alpha=[A-Za-z];
ws = [\ \t];
%%
\n       => (pos := (!pos) + 1; lex());
{ws}+    => (lex());

	      
"("      => (TextIO.output(TextIO.stdOut, " LPAREN ( ");Tokens.LPAREN(!pos,!pos));
")"      => (TextIO.output(TextIO.stdOut, " RPAREN ) ");Tokens.RPAREN(!pos,!pos));
";"      => (TextIO.output(TextIO.stdOut, " TERM  ; ");Tokens.TERM(!pos,!pos));
"TRUE"   => (TextIO.output(TextIO.stdOut, " CONST  TRUE ");Tokens.CONST(!pos,!pos));
"FALSE"   => (TextIO.output(TextIO.stdOut, " CONST FALSE ");Tokens.CONST(!pos,!pos));
"IF"     => (TextIO.output(TextIO.stdOut, " IF IF ");Tokens.IF(!pos,!pos));
"THEN"   => (TextIO.output(TextIO.stdOut, " THEN THEN ");Tokens.THEN(!pos,!pos));
"ELSE"   => (TextIO.output(TextIO.stdOut, " ELSE ELSE ");Tokens.ELSE(!pos,!pos));
"NOT"    => (TextIO.output(TextIO.stdOut, " NOT NOT ");Tokens.NOT(!pos,!pos));
"OR"     => (TextIO.output(TextIO.stdOut, " OR OR ");Tokens.OR(!pos,!pos));
"AND"    => (TextIO.output(TextIO.stdOut, " AND AND ");Tokens.AND(!pos,!pos));
"XOR"    => (TextIO.output(TextIO.stdOut, " XOR XOR ");Tokens.XOR(!pos,!pos));
"EQUALS" => (TextIO.output(TextIO.stdOut, " EQUALS EQUALS ");Tokens.EQUALS(!pos,!pos));
"IMPLIES"=> (TextIO.output(TextIO.stdOut, " IMPLIES IMPLIES ");Tokens.IMPLIES(!pos,!pos));
{alpha}+ => (TextIO.output(TextIO.stdOut, "ID"^yytext);Tokens.ID(yytext,!pos,!pos));



"."      => (error ("ERROR Unknown Token "^(Int.toString (!pos))^yytext,!pos,!pos);
             lex());

