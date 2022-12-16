structure evaluate = 

struct

open AST

val brokenTypes = Fail "Error in evaluation!"

fun evalExp(e:exp, env:environment) : value =
	case e of
		ConstExp b => BoolVal b
		
		|NumExp n => IntVal n
		
		|VarExp x => envLookup(x, env)
		
		|BinaryExp(bn, e1, e2) => evalBinExp(e1, bn, e2, env)
		
		|BooleanExp(bl, e1, e2) => evalBoolExp(e1, bl, e2, env)
		
		|UnaryExp(u, e) => evalUnaryExp(u, e, env)
		
		|BoolunaryExp(bu, e) => evalBoolunaryExp(bu, e, env)
		
		|LetExp(ValDecl(x, e1), e2) =>
			(
				let
					val t = evalExp(e1, env)
				in
					evalExp(e2, envAdd(x, t, env))
				end
			)
			
		|IfExp(e1, e2, e3) =>
			(case evalExp(e1, env) of
				BoolVal b => if b then evalExp(e2, env) else evalExp(e3, env)
					| _ => raise brokenTypes)
		
		|ParExp(p1, e, p2) =>
			if p1 = Lparen andalso p2 = Rparen then evalExp(e, env)
			else raise brokenTypes

and

	evalBinExp(e1:exp, bn:binary, e2:exp, env:environment) : value =
		case (evalExp(e1, env), bn, evalExp(e2, env)) of
			(IntVal n1, Add, IntVal n2) => IntVal(n1 + n2)
			|(IntVal n1, Subtract, IntVal n2) => IntVal(n1 - n2)
			|(IntVal n1, Multiply, IntVal n2) => IntVal(n1 * n2)
			| _ => raise brokenTypes
			
and
		
	evalBoolExp(e1:exp, bl:boolean, e2:exp, env:environment) : value =
		case (evalExp(e1, env), bl, evalExp(e2, env)) of
			(BoolVal b1, And, BoolVal b2) => BoolVal(b1 andalso b2)
			|(BoolVal b1, Or, BoolVal b2) => BoolVal(b1 orelse b2)
			|(BoolVal b1, Xor, BoolVal b2) => BoolVal((b1 andalso (not b2)) orelse ((not b1) andalso b2))
			|(BoolVal b1, Implies, BoolVal b2) => BoolVal((not b1) orelse b2) 
			|(IntVal n1, Equals, IntVal n2) => BoolVal(n1 = n2)
			|(IntVal n1, Greater, IntVal n2) => BoolVal(n1 > n2)
			|(IntVal n1, Lesser, IntVal n2) => BoolVal(n1 < n2)
			| _ => raise brokenTypes 
		
and

	evalUnaryExp(u:unary, e:exp, env:environment) : value =
		case (evalExp(e, env), u) of
			(IntVal n, Negate) => IntVal(~n)
			| _ => raise brokenTypes
		
and

	evalBoolunaryExp(bu:boolunary, e:exp, env:environment) : value =
		case (evalExp(e, env), bu) of
			(BoolVal b, Not) => BoolVal(not b)
			| _ => raise brokenTypes
			

	fun checkeval(array:exp list, env:environment) = 
		let
			val f = fn x => evalExp(x, env)
		in
			List.map f array
		end
		
end

		
		
		