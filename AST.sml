structure AST =

struct

type id = string

datatype binary = Add|Subtract|Multiply

datatype unary = Negate

datatype boolean = And|Or|Xor|Implies|Equals|Greater|Lesser

datatype boolunary = Not

datatype par = Lparen|Rparen

datatype temp = ifDecl of bool

datatype typ = IntTyp
			  |BoolTyp
			  |StrTyp
			  |FnTyp of typ * typ

and exp = ConstExp of bool 
		|NumExp of int
    	|StrExp of string
    	|VarExp of id
		|BinaryExp of binary * exp * exp
		|LetExp of decl * exp
        |BooleanExp of boolean * exp * exp
		|IfExp of exp * exp * exp
		|AppExp of exp * exp
		|ParExp of par * exp * par
		|FnExp of id * typ * typ * exp
		|FunExp of id * id * typ * typ * exp
		|UnaryExp of unary * exp
		|BoolunaryExp of boolunary * exp
		
and decl = ValDecl of id * exp
				       
datatype value = IntVal of int
               |StrVal of string
			   |BoolVal of bool
			   |FnVal of id * exp * (id * value) list ref

type environment = (id * value) list

fun envAdd (var:id, v:value, env:environment ref) =
   (env:= (var,v)::(!env); env)

fun envLookup (var:id, env:environment ref) =
    case List.find(fn (x, _) => x = var) (!env) of
				       SOME (x, v)   => v
				    |   NONE => raise Fail "Environment lookup error"
				    							    
end


