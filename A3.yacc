%%

(*user declarations*)
	 
%name A3

%term
	CONST of bool
	|ID of string
	|NUM of int
	|TERM|COLON|COMMA|EOF
	|FUN|FN|DARROW|SARROW
	|IF|THEN|ELSE|FI
	|AND|OR|XOR|NOT|EQUALS|IMPLIES
	|LPAREN|RPAREN
	|PLUS|MINUS|TIMES|NEGATE|EQ|LESSERTHAN|GREATERTHAN
	|LET|IN|END|VAL
	|BOOL|INT|STR

%nonterm exp of AST.exp | program of AST.exp list | decl of AST.decl | Type of AST.typ | statement of AST.exp | START of AST.exp list

%pos int

(*optional declarations*)

%eop EOF
%noshift EOF

(*header*)

%nonassoc DARROW
%right SARROW
%right IF THEN ELSE
%right IMPLIES
%left LESSERTHAN GREATERTHAN
%left EQ
%left PLUS MINUS
%left TIMES
%left AND OR XOR EQUALS
%right NOT NEGATE

%start START

%verbose

%%
	START:program(program)
	program: statement TERM program([statement]@program)
			| statement([statement])

	statement: exp(exp)
	decl: ID EQ exp (AST.ValDecl(ID, exp))
	
	
	Type:
		Type SARROW Type (AST.FnTyp(Type1, Type2))
		|LPAREN Type SARROW Type RPAREN(AST.FnTyp(Type1, Type2))
		|INT (AST.IntTyp)
		|BOOL (AST.BoolTyp)
		|STR (AST.StrTyp)
		
	exp:
		LET decl IN exp END (AST.LetExp(decl, exp))
		|IF exp THEN exp ELSE exp FI (AST.IfExp(exp1, exp2, exp3))
		
		|FN LPAREN ID COLON Type RPAREN COLON Type DARROW exp (AST.FnExp(ID, Type1, Type2, exp))
		|FUN ID LPAREN ID COLON Type RPAREN COLON Type DARROW exp (AST.FunExp(ID1, ID2, Type1, Type2, exp))
		
		|LPAREN exp RPAREN (AST.ParExp(AST.Lparen, exp, AST.Rparen))
		|LPAREN exp exp RPAREN (AST.AppExp(exp1, exp2))
		
		|CONST (AST.ConstExp(CONST))
		|ID (AST.VarExp(ID))
		|NUM (AST.NumExp(NUM))
		
		|exp PLUS exp (AST.BinaryExp(AST.Add, exp1, exp2))
		|exp MINUS exp (AST.BinaryExp(AST.Subtract, exp1, exp2))
		|exp TIMES exp (AST.BinaryExp(AST.Multiply, exp1, exp2))
		
		|NEGATE exp (AST.UnaryExp(AST.Negate, exp))
		
		|exp AND exp (AST.BooleanExp(AST.And, exp1, exp2))
		|exp OR exp (AST.BooleanExp(AST.Or, exp1, exp2))
		|exp XOR exp (AST.BooleanExp(AST.Xor, exp1, exp2))
		|exp IMPLIES exp (AST.BooleanExp(AST.Implies, exp1, exp2))
		|exp EQUALS exp (AST.BooleanExp(AST.Equals, exp1, exp2))
		|exp GREATERTHAN exp (AST.BooleanExp(AST.Greater, exp1, exp2))
		|exp LESSERTHAN exp (AST.BooleanExp(AST.Lesser, exp1, exp2))


		|NOT exp (AST.BoolunaryExp(AST.Not, exp))
	