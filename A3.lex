structure Tokens = Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val pos = ref 0
  val eof = fn () => Tokens.EOF(!pos, !pos)
  exception error;
  
  fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))


%%

%header (functor A3LexFun(structure Tokens:A3_TOKENS));

alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];

%%


\n       => (pos := (!pos) + 1; lex());
{ws}+    => (lex());
{digit}+ => (Tokens.NUM(List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext), !pos, !pos));

"="      => (Tokens.EQ(!pos,!pos));
";"      => (Tokens.TERM(!pos,!pos));
"("      => (Tokens.LPAREN(!pos,!pos));
")"      => (Tokens.RPAREN(!pos,!pos));
":"		 => (Tokens.COLON(!pos, !pos));
"->"	 => (Tokens.SARROW(!pos, !pos));
"=>"  	 => (Tokens.DARROW(!pos, !pos));
"," 	 => (Tokens.COMMA(!pos, !pos));


{alpha}+{digit}* => (if yytext = "TRUE" then Tokens.CONST(true, !pos, !pos)
			 else if yytext = "FALSE" then Tokens.CONST(false, !pos, !pos)
			 else if yytext = "OR" then Tokens.OR(!pos, !pos)
			 else if yytext = "AND" then Tokens.AND(!pos, !pos)
			 else if yytext = "XOR" then Tokens.XOR(!pos, !pos)
			 else if yytext = "NOT" then Tokens.NOT(!pos, !pos)
			 else if yytext = "EQUALS" then Tokens.EQUALS(!pos, !pos)
			 else if yytext = "IMPLIES" then Tokens.IMPLIES(!pos, !pos)
			 else if yytext = "PLUS" then Tokens.PLUS(!pos, !pos)
			 else if yytext = "MINUS" then Tokens.MINUS(!pos, !pos)
			 else if yytext = "TIMES" then Tokens.TIMES(!pos, !pos)
			 else if yytext = "NEGATE" then Tokens.NEGATE(!pos, !pos)
			 else if yytext = "LESSTHAN" then Tokens.LESSERTHAN(!pos, !pos)
			 else if yytext = "GREATERTHAN" then Tokens.GREATERTHAN(!pos, !pos)
			 else if yytext = "if" then Tokens.IF(!pos, !pos)
			 else if yytext = "then" then Tokens.THEN(!pos, !pos)
			 else if yytext = "else" then Tokens.ELSE(!pos, !pos)
			 else if yytext = "fi" then Tokens.FI(!pos, !pos)
			 else if yytext = "let" then Tokens.LET(!pos, !pos)
			 else if yytext = "in" then Tokens.IN(!pos, !pos)
			 else if yytext = "end" then Tokens.END(!pos, !pos)
			 else if yytext = "val" then Tokens.VAL(!pos, !pos)
			 else if yytext = "fun" then Tokens.FUN(!pos, !pos)
			 else if yytext = "fn" then Tokens.FN(!pos, !pos)
			 else if yytext = "bool" then Tokens.BOOL(!pos, !pos)
			 else if yytext = "int" then Tokens.INT(!pos, !pos)
			 else if yytext = "string" then Tokens.STR(!pos, !pos)
			 	
			 else Tokens.ID(yytext, !pos, !pos));
			
"."      => (raise error; lex());
		
	



