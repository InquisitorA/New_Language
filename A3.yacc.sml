functor A3LrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : A3_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\015\000\002\000\014\000\003\000\013\000\008\000\012\000\
\\009\000\011\000\012\000\010\000\016\000\026\000\017\000\025\000\
\\018\000\024\000\019\000\009\000\020\000\023\000\021\000\022\000\
\\022\000\008\000\023\000\049\000\024\000\021\000\025\000\020\000\
\\026\000\019\000\027\000\007\000\029\000\018\000\030\000\017\000\
\\031\000\006\000\000\000\
\\001\000\001\000\015\000\002\000\014\000\003\000\013\000\008\000\012\000\
\\009\000\011\000\012\000\010\000\019\000\009\000\022\000\008\000\
\\027\000\007\000\031\000\006\000\000\000\
\\001\000\002\000\028\000\000\000\
\\001\000\002\000\034\000\000\000\
\\001\000\002\000\051\000\000\000\
\\001\000\002\000\058\000\000\000\
\\001\000\005\000\057\000\000\000\
\\001\000\005\000\066\000\000\000\
\\001\000\005\000\073\000\000\000\
\\001\000\005\000\079\000\000\000\
\\001\000\007\000\000\000\000\000\
\\001\000\010\000\080\000\011\000\069\000\000\000\
\\001\000\010\000\084\000\011\000\069\000\000\000\
\\001\000\011\000\069\000\023\000\068\000\000\000\
\\001\000\011\000\069\000\023\000\076\000\000\000\
\\001\000\011\000\069\000\023\000\081\000\000\000\
\\001\000\011\000\075\000\000\000\
\\001\000\013\000\050\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\020\000\023\000\021\000\022\000\024\000\021\000\025\000\020\000\
\\026\000\019\000\029\000\018\000\030\000\017\000\000\000\
\\001\000\014\000\060\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\020\000\023\000\021\000\022\000\024\000\021\000\025\000\020\000\
\\026\000\019\000\029\000\018\000\030\000\017\000\000\000\
\\001\000\015\000\072\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\020\000\023\000\021\000\022\000\024\000\021\000\025\000\020\000\
\\026\000\019\000\029\000\018\000\030\000\017\000\000\000\
\\001\000\016\000\026\000\017\000\025\000\018\000\024\000\020\000\023\000\
\\021\000\022\000\023\000\055\000\024\000\021\000\025\000\020\000\
\\026\000\019\000\029\000\018\000\030\000\017\000\000\000\
\\001\000\016\000\026\000\017\000\025\000\018\000\024\000\020\000\023\000\
\\021\000\022\000\024\000\021\000\025\000\020\000\026\000\019\000\
\\029\000\018\000\030\000\017\000\033\000\059\000\000\000\
\\001\000\022\000\033\000\000\000\
\\001\000\022\000\052\000\000\000\
\\001\000\022\000\065\000\035\000\064\000\036\000\063\000\037\000\062\000\000\000\
\\001\000\028\000\047\000\000\000\
\\001\000\032\000\046\000\000\000\
\\087\000\000\000\
\\088\000\000\000\
\\089\000\004\000\016\000\000\000\
\\090\000\016\000\026\000\017\000\025\000\018\000\024\000\020\000\023\000\
\\021\000\022\000\024\000\021\000\025\000\020\000\026\000\019\000\
\\029\000\018\000\030\000\017\000\000\000\
\\091\000\016\000\026\000\017\000\025\000\018\000\024\000\020\000\023\000\
\\021\000\022\000\024\000\021\000\025\000\020\000\026\000\019\000\
\\029\000\018\000\030\000\017\000\000\000\
\\092\000\011\000\069\000\000\000\
\\093\000\000\000\
\\094\000\000\000\
\\095\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\016\000\026\000\017\000\025\000\018\000\024\000\020\000\023\000\
\\021\000\022\000\024\000\021\000\025\000\020\000\026\000\019\000\
\\029\000\018\000\030\000\017\000\000\000\
\\100\000\016\000\026\000\017\000\025\000\018\000\024\000\020\000\023\000\
\\021\000\022\000\024\000\021\000\025\000\020\000\026\000\019\000\
\\029\000\018\000\030\000\017\000\000\000\
\\101\000\000\000\
\\102\000\000\000\
\\103\000\000\000\
\\104\000\000\000\
\\105\000\000\000\
\\106\000\016\000\026\000\017\000\025\000\018\000\024\000\020\000\023\000\
\\026\000\019\000\000\000\
\\107\000\016\000\026\000\017\000\025\000\018\000\024\000\020\000\023\000\
\\026\000\019\000\000\000\
\\108\000\016\000\026\000\017\000\025\000\018\000\024\000\020\000\023\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\\111\000\000\000\
\\112\000\000\000\
\\113\000\016\000\026\000\017\000\025\000\018\000\024\000\020\000\023\000\
\\021\000\022\000\024\000\021\000\025\000\020\000\026\000\019\000\
\\029\000\018\000\030\000\017\000\000\000\
\\114\000\000\000\
\\115\000\016\000\026\000\017\000\025\000\018\000\024\000\020\000\023\000\
\\024\000\021\000\025\000\020\000\026\000\019\000\000\000\
\\116\000\016\000\026\000\017\000\025\000\018\000\024\000\020\000\023\000\
\\024\000\021\000\025\000\020\000\026\000\019\000\000\000\
\\117\000\000\000\
\"
val actionRowNumbers =
"\001\000\029\000\027\000\030\000\
\\002\000\001\000\001\000\001\000\
\\001\000\022\000\003\000\045\000\
\\044\000\043\000\001\000\001\000\
\\001\000\001\000\001\000\001\000\
\\001\000\001\000\001\000\001\000\
\\001\000\026\000\025\000\049\000\
\\000\000\057\000\017\000\004\000\
\\023\000\028\000\055\000\056\000\
\\048\000\047\000\046\000\053\000\
\\054\000\052\000\051\000\050\000\
\\001\000\001\000\020\000\041\000\
\\001\000\006\000\005\000\021\000\
\\031\000\042\000\018\000\024\000\
\\007\000\037\000\001\000\013\000\
\\036\000\034\000\035\000\024\000\
\\024\000\019\000\008\000\024\000\
\\016\000\014\000\038\000\024\000\
\\032\000\024\000\009\000\011\000\
\\015\000\024\000\001\000\033\000\
\\012\000\039\000\001\000\040\000\
\\010\000"
val gotoT =
"\
\\001\000\003\000\002\000\002\000\005\000\001\000\006\000\084\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\025\000\000\000\
\\001\000\027\000\000\000\
\\001\000\028\000\000\000\
\\001\000\029\000\000\000\
\\001\000\030\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\003\000\002\000\033\000\005\000\001\000\000\000\
\\001\000\034\000\000\000\
\\001\000\035\000\000\000\
\\001\000\036\000\000\000\
\\001\000\037\000\000\000\
\\001\000\038\000\000\000\
\\001\000\039\000\000\000\
\\001\000\040\000\000\000\
\\001\000\041\000\000\000\
\\001\000\042\000\000\000\
\\001\000\043\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\046\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\051\000\000\000\
\\001\000\052\000\000\000\
\\000\000\
\\000\000\
\\001\000\054\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\059\000\000\000\
\\000\000\
\\000\000\
\\001\000\065\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\068\000\000\000\
\\004\000\069\000\000\000\
\\000\000\
\\000\000\
\\004\000\072\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\075\000\000\000\
\\000\000\
\\004\000\076\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\080\000\000\000\
\\001\000\081\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\083\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 85
val numrules = 31
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | NUM of unit ->  (int) | ID of unit ->  (string)
 | CONST of unit ->  (bool) | START of unit ->  (AST.exp list)
 | statement of unit ->  (AST.exp) | Type of unit ->  (AST.typ)
 | decl of unit ->  (AST.decl) | program of unit ->  (AST.exp list)
 | exp of unit ->  (AST.exp)
end
type svalue = MlyValue.svalue
type result = AST.exp list
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 6) => true | _ => false
val showTerminal =
fn (T 0) => "CONST"
  | (T 1) => "ID"
  | (T 2) => "NUM"
  | (T 3) => "TERM"
  | (T 4) => "COLON"
  | (T 5) => "COMMA"
  | (T 6) => "EOF"
  | (T 7) => "FUN"
  | (T 8) => "FN"
  | (T 9) => "DARROW"
  | (T 10) => "SARROW"
  | (T 11) => "IF"
  | (T 12) => "THEN"
  | (T 13) => "ELSE"
  | (T 14) => "FI"
  | (T 15) => "AND"
  | (T 16) => "OR"
  | (T 17) => "XOR"
  | (T 18) => "NOT"
  | (T 19) => "EQUALS"
  | (T 20) => "IMPLIES"
  | (T 21) => "LPAREN"
  | (T 22) => "RPAREN"
  | (T 23) => "PLUS"
  | (T 24) => "MINUS"
  | (T 25) => "TIMES"
  | (T 26) => "NEGATE"
  | (T 27) => "EQ"
  | (T 28) => "LESSERTHAN"
  | (T 29) => "GREATERTHAN"
  | (T 30) => "LET"
  | (T 31) => "IN"
  | (T 32) => "END"
  | (T 33) => "VAL"
  | (T 34) => "BOOL"
  | (T 35) => "INT"
  | (T 36) => "STR"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30)
 $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23)
 $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16)
 $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9)
 $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.program program1, program1left, 
program1right)) :: rest671)) => let val  result = MlyValue.START (fn _
 => let val  (program as program1) = program1 ()
 in (program)
end)
 in ( LrTable.NT 5, ( result, program1left, program1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.program program1, _, program1right)) :: _ ::
 ( _, ( MlyValue.statement statement1, statement1left, _)) :: rest671)
) => let val  result = MlyValue.program (fn _ => let val  (statement
 as statement1) = statement1 ()
 val  (program as program1) = program1 ()
 in ([statement]@program)
end)
 in ( LrTable.NT 1, ( result, statement1left, program1right), rest671)

end
|  ( 2, ( ( _, ( MlyValue.statement statement1, statement1left, 
statement1right)) :: rest671)) => let val  result = MlyValue.program
 (fn _ => let val  (statement as statement1) = statement1 ()
 in ([statement])
end)
 in ( LrTable.NT 1, ( result, statement1left, statement1right), 
rest671)
end
|  ( 3, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671))
 => let val  result = MlyValue.statement (fn _ => let val  (exp as 
exp1) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 4, ( result, exp1left, exp1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.decl (fn _ => let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in (AST.ValDecl(ID, exp))
end)
 in ( LrTable.NT 2, ( result, ID1left, exp1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.Type Type2, _, Type2right)) :: _ :: ( _, ( 
MlyValue.Type Type1, Type1left, _)) :: rest671)) => let val  result = 
MlyValue.Type (fn _ => let val  Type1 = Type1 ()
 val  Type2 = Type2 ()
 in (AST.FnTyp(Type1, Type2))
end)
 in ( LrTable.NT 3, ( result, Type1left, Type2right), rest671)
end
|  ( 6, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.Type Type2, _
, _)) :: _ :: ( _, ( MlyValue.Type Type1, _, _)) :: ( _, ( _, 
LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.Type (fn _
 => let val  Type1 = Type1 ()
 val  Type2 = Type2 ()
 in (AST.FnTyp(Type1, Type2))
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 7, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.Type (fn _ => (AST.IntTyp))
 in ( LrTable.NT 3, ( result, INT1left, INT1right), rest671)
end
|  ( 8, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.Type (fn _ => (AST.BoolTyp))
 in ( LrTable.NT 3, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 9, ( ( _, ( _, STR1left, STR1right)) :: rest671)) => let val  
result = MlyValue.Type (fn _ => (AST.StrTyp))
 in ( LrTable.NT 3, ( result, STR1left, STR1right), rest671)
end
|  ( 10, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.exp exp1, _, _)
) :: _ :: ( _, ( MlyValue.decl decl1, _, _)) :: ( _, ( _, LET1left, _)
) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (
decl as decl1) = decl1 ()
 val  (exp as exp1) = exp1 ()
 in (AST.LetExp(decl, exp))
end)
 in ( LrTable.NT 0, ( result, LET1left, END1right), rest671)
end
|  ( 11, ( ( _, ( _, _, FI1right)) :: ( _, ( MlyValue.exp exp3, _, _))
 :: _ :: ( _, ( MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp 
exp1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result
 = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (AST.IfExp(exp1, exp2, exp3))
end)
 in ( LrTable.NT 0, ( result, IF1left, FI1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.Type Type2, _, _)) :: _ :: _ :: ( _, ( MlyValue.Type Type1, _
, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: _ :: ( _, ( _, FN1left
, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val 
 (ID as ID1) = ID1 ()
 val  Type1 = Type1 ()
 val  Type2 = Type2 ()
 val  (exp as exp1) = exp1 ()
 in (AST.FnExp(ID, Type1, Type2, exp))
end)
 in ( LrTable.NT 0, ( result, FN1left, exp1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.Type Type2, _, _)) :: _ :: _ :: ( _, ( MlyValue.Type Type1, _
, _)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, FUN1left, _)) :: rest671)) => let
 val  result = MlyValue.exp (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  Type1 = Type1 ()
 val  Type2 = Type2 ()
 val  (exp as exp1) = exp1 ()
 in (AST.FunExp(ID1, ID2, Type1, Type2, exp))
end)
 in ( LrTable.NT 0, ( result, FUN1left, exp1right), rest671)
end
|  ( 14, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (AST.ParExp(AST.Lparen, exp, AST.Rparen))
end)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 15, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp2, _,
 _)) :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _, LPAREN1left, _))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1
 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.AppExp(exp1, exp2))
end)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.CONST CONST1, CONST1left, CONST1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (CONST
 as CONST1) = CONST1 ()
 in (AST.ConstExp(CONST))
end)
 in ( LrTable.NT 0, ( result, CONST1left, CONST1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  (ID as ID1) = ID1 ()
 in (AST.VarExp(ID))
end)
 in ( LrTable.NT 0, ( result, ID1left, ID1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.exp (fn _ => let val  (NUM as NUM1) = 
NUM1 ()
 in (AST.NumExp(NUM))
end)
 in ( LrTable.NT 0, ( result, NUM1left, NUM1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.BinaryExp(AST.Add, exp1, exp2))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.BinaryExp(AST.Subtract, exp1, exp2))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.BinaryExp(AST.Multiply, exp1, exp2))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
NEGATE1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (exp as exp1) = exp1 ()
 in (AST.UnaryExp(AST.Negate, exp))
end)
 in ( LrTable.NT 0, ( result, NEGATE1left, exp1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.BooleanExp(AST.And, exp1, exp2))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.BooleanExp(AST.Or, exp1, exp2))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.BooleanExp(AST.Xor, exp1, exp2))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.BooleanExp(AST.Implies, exp1, exp2))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.BooleanExp(AST.Equals, exp1, exp2))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.BooleanExp(AST.Greater, exp1, exp2))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.BooleanExp(AST.Lesser, exp1, exp2))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
NOT1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ =>
 let val  (exp as exp1) = exp1 ()
 in (AST.BoolunaryExp(AST.Not, exp))
end)
 in ( LrTable.NT 0, ( result, NOT1left, exp1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : A3_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.CONST (fn () => i),p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun FN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun DARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun SARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun XOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun IMPLIES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun NEGATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSERTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATERTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun VAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun STR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
end
end
