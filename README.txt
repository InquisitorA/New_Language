Commands required for running the program
enter
ml-lex A3.lex			//to compile the lex file
enter
ml-yacc A3.yacc			//to compile the yacc file
enter
use "loader.sml";		//to run all the components
enter
open typing;			//to open typing environment
enter
open evaluate;			//to open evaluate environment
enter
parsefile "filename/directory";	//to print AST
enter
checktyp(it, []);		//to do type checking
enter
checkeval(it, []);		//to do evaluation
 