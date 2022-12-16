structure typing = 

struct

open AST		

type typeEnv = (id * typ) list 

fun typeEnvLookup (var : id, env:typeEnv ref) : typ =        
    case List.find(fn (x,_) => x = var) (!env) of        
            SOME (x,v) => v          
        |   NONE => raise Fail ("error in environment lookup")                 


fun typeEnvAdd (var:id , t:typ, env:typeEnv ref)  =           
    (env := (var,t)::(!env); env)

fun remove (item, list) =SOME(List.filter(fn x => x <> item) list) 
	
fun typeEnvRemove (var:id , t:typ, env:typeEnv ref)  =            
    case remove((var,t), (!env)) of           
        SOME array => array                  
      | NONE => raise Fail "cannot remove type from environment." 
	
fun getType (e:exp, env:typeEnv ref) : typ =
	case e of 
		ConstExp _ => BoolTyp
		
		|NumExp _ => IntTyp
		
		|StrExp _ => StrTyp
		
		|VarExp x => typeEnvLookup(x, env)
		
		|AppExp(e1, e2) => 
			(case(getType(e1, env), getType(e2, env)) of
				(FnTyp(t1, t2), t3) => 
				if (t1 = t3)
				then t2
				else raise Fail "Application argument type mismatch"
				| (_, _) => raise Fail "Function was expected"
			)
			
		
		|IfExp (e1, e2, e3) =>
		(
			let
				val t1 = getType(e1, env)
				val t2 = getType(e2, env)
				val t3 = getType(e3, env)
			in
				if t1 <> BoolTyp
				then raise Fail "Condition of if command is not of BoolTyp"
				else 
					if t2 <> t3 
					then raise Fail "Type error: Branches have different types"
					else t2
			end
		)
		
		
		|BooleanExp (bl, e1, e2) =>
		(
			let 
				val e1Typ = getType(e1, env)
				val e2Typ = getType(e2, env)
			in
				if bl = Equals
				then BoolTyp
				else 
					if bl = Greater
					then BoolTyp
					else 
						if bl = Lesser
						then BoolTyp
						else
							if e1Typ = BoolTyp andalso e2Typ = BoolTyp
							then e1Typ
							else raise Fail "Mismatch in declared type and expected type"
			end
		)
		
		|BinaryExp (bn, e1, e2) =>
		(
			let 
				val e1Typ = getType(e1, env)
				val e2Typ = getType(e2, env)
			in
				if e1Typ = IntTyp andalso e1Typ = IntTyp
				then e1Typ
				else raise Fail "Mismatch in declared type and expected type"
			end
		)
		
		|UnaryExp (bn, e) =>
		( 
			let 
				val eTyp = getType(e, env)
			in
				if eTyp = IntTyp
				then eTyp
				else raise Fail "Mismatch in declared type and expected type"
			end
		)
		
		|BoolunaryExp (bl, e) =>
		( 
			let 
				val eTyp = getType(e, env)
			in
				if eTyp = BoolTyp
				then eTyp
				else raise Fail "Mismatch in declared type and expected type"
			end
		)
		
		|ParExp (p1, e, p2) =>
		( 
			let 
				val eTyp = getType(e, env)
			in
				if p1 = Lparen andalso p2 = Rparen 
				then eTyp
				else raise Fail "Invalid bracket matching"
			end
		)

		|FnExp (x,t1,t2,e) =>           
		(
            let       
				val t3 = getType(e,typeEnvAdd(x,t1,env))      
            in       
                typeEnvRemove(x,t1,env);         
                if (t2=t3)
				then FnTyp(t1,t2)             
                else raise Fail "Mismatch in declared type and actual type."     
            end      
        )
		
		|FunExp (k, x, t1, t2, e) =>        
        (
            let             
                val evalTyp = getType (e, typeEnvAdd(k, t2, typeEnvAdd(x,t1,env)))      
            in           
                typeEnvRemove(x,t1,env);          
                if (evalTyp = t2)         
                then FnTyp(t1,t2)        
                else raise Fail "Mismatch in declared type and actual type."         
            end          
        )        
		
		|LetExp (ValDecl(x,e1),e2)  =>            
        ( 
            let       
                val t1 = getType(e1,env)            
                val t2 = getType(e2,typeEnvAdd(x,t1,env))        
            in
                typeEnvRemove(x,t1,env);        
                t2      
            end
        )
		
	fun checktyp(array:exp list, t:typeEnv ref) = 
		let
			val f = fn x => getType(x, t)
		in
			List.map f array
		end

end
		
		
		
		
			
