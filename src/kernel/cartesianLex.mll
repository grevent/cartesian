{ 
  open CartesianSyntax;;
  
  exception Eof;;
  exception LexicalError;;
}

let hexadecimal = ['0'-'9''a'-'f''A'-'F']
let unicode = "\\u" hexadecimal hexadecimal hexadecimal hexadecimal
let escaped = '\\' ('n' | 'r' | 't' | '\"' | '\\')
let char = [^'\"''\\']
let allchar = escaped | unicode | char
  
  rule lexer = 		    parse 
     [' ' '\t'] { Debug.lexDebug "SPACE -> Ignoring"; lexer lexbuf } 
	| ['\r' '\n'] { Debug.lexDebug "Return -> Ignoring"; lexer lexbuf }
	| "define" { Debug.lexDebug "DEFINE"; CartesianSyntax.DEFINE }
	| "+" { Debug.lexDebug "PLUS"; CartesianSyntax.PLUS }
	| "-" { Debug.lexDebug "MINUS"; CartesianSyntax.MINUS }
	| "*" { Debug.lexDebug "MUL"; CartesianSyntax.MUL }
	| "/" { Debug.lexDebug "DIV"; CartesianSyntax.DIV }
	| "^" { Debug.lexDebug "PUISS"; CartesianSyntax.PUISS }
	| "<" { Debug.lexDebug "INF"; CartesianSyntax.INF }
	| ">" { Debug.lexDebug "SUP"; CartesianSyntax.SUP }
    | "==" { Debug.lexDebug "EGALEGAL"; CartesianSyntax.EGALEGAL }
    | "!=" { Debug.lexDebug "NOTEGAL"; CartesianSyntax.NOTEGAL }
    | "<=" { Debug.lexDebug "INFEGAL"; CartesianSyntax.INFEGAL }
    | ">=" { Debug.lexDebug "INFEGAL"; CartesianSyntax.SUPEGAL }
    | "[" { Debug.lexDebug "CROO"; CartesianSyntax.CROO }
    | "]" { Debug.lexDebug "CROF"; CartesianSyntax.CROF }
    | "[|" { Debug.lexDebug "CROOPIPE"; CartesianSyntax.CROOPIPE }
    | "|]" { Debug.lexDebug "PIPECROF"; CartesianSyntax.PIPECROF }
    | "|" { Debug.lexDebug "PIPE"; CartesianSyntax.PIPE }
    | "{" { Debug.lexDebug "ACOO"; CartesianSyntax.ACOO }
    | "}" { Debug.lexDebug "ACOF"; CartesianSyntax.ACOF }
    | "lambda" { Debug.lexDebug "LAMBDA"; CartesianSyntax.LAMBDA }
    | "->" { Debug.lexDebug "FLECHD"; CartesianSyntax.FLECHD }
    | "<-" { Debug.lexDebug "FLECHG"; CartesianSyntax.FLECHG }
    | "=>" { Debug.lexDebug "IMPLY"; CartesianSyntax.IMPLY }
    | "let" { Debug.lexDebug "LET"; CartesianSyntax.LET }
    | "in" { Debug.lexDebug "IN"; CartesianSyntax.IN }
    | "::" { Debug.lexDebug "DEUXDEUXPOINTS"; CartesianSyntax.DEUXDEUXPOINTS }
    | "=" { Debug.lexDebug "EGAL"; CartesianSyntax.EGAL }
    | ":" { Debug.lexDebug "DEUXPOINTS"; CartesianSyntax.DEUXPOINTS }
    | "." { Debug.lexDebug "PT"; CartesianSyntax.PT }
    | ".." { Debug.lexDebug "PTPT"; CartesianSyntax.PTPT }
	| "," { Debug.lexDebug "VIRG"; CartesianSyntax.VIRG }
    | "'" { Debug.lexDebug "QUOTE"; CartesianSyntax.QUOTE }
    | "(" { Debug.lexDebug "PARO"; CartesianSyntax.PARO }
    | ")" { Debug.lexDebug "PARF"; CartesianSyntax.PARF }
    | "all" { Debug.lexDebug "ALL"; CartesianSyntax.ALL }
    | "match" { Debug.lexDebug "MATCH"; CartesianSyntax.MATCH }
    | "with" { Debug.lexDebug "WITH"; CartesianSyntax.WITH }
    | "and" { Debug.lexDebug "AND"; CartesianSyntax.AND }
    | "mod" { Debug.lexDebug "MOD"; CartesianSyntax.MOD }
	| "do" { Debug.lexDebug "DO"; CartesianSyntax.DO }
	| "interface" { Debug.lexDebug "INTERFACE"; CartesianSyntax.INTERFACE }
	| "now" { Debug.lexDebug "NOW"; CartesianSyntax.NOW }
	| "{~" { Debug.lexDebug "ACOOTILDE"; CartesianSyntax.ACOOTILDE }
	| "~}" { Debug.lexDebug "TILDEACOF"; CartesianSyntax.TILDEACOF }
	| "{<" { Debug.lexDebug "ACOOINF"; CartesianSyntax.ACOOINF }
	| ">}" { Debug.lexDebug "SUPACOF"; CartesianSyntax.SUPACOF }
    | "not" { Debug.lexDebug "NOT"; CartesianSyntax.NOT }
    | "||" { Debug.lexDebug "LOGICALOR"; CartesianSyntax.LOGICALOR }
    | "&&" { Debug.lexDebug "LOGICALAND"; CartesianSyntax.LOGICALAND }
    | ";" { Debug.lexDebug "PTVIRG"; CartesianSyntax.PTVIRG }
    | "," { Debug.lexDebug "VIRG"; CartesianSyntax.VIRG }
    | "_" { Debug.lexDebug "SOULIGNE"; CartesianSyntax.SOULIGNE }
    | "as" { Debug.lexDebug "AS"; CartesianSyntax.AS }
    | "nod" { Debug.lexDebug "NOD"; CartesianSyntax.NOD }
    | "if" { Debug.lexDebug "IF"; CartesianSyntax.IF }
    | "then" { Debug.lexDebug "THEN"; CartesianSyntax.THEN }
    | "else" { Debug.lexDebug "ELSE"; CartesianSyntax.ELSE }
    | ('+'|'-')?['0'-'9']+ as integer { Debug.lexDebug (Printf.sprintf "INTVALUE='%s'" (Lexing.lexeme lexbuf)); CartesianSyntax.INTVALUE (int_of_string integer) }
    | ('+'|'-')?['0'-'9']+'.'['0'-'9']*(('e'|'E')['0'-'9']+)? as floating { Debug.lexDebug (Printf.sprintf "FLOATVALUE='%s'" (Lexing.lexeme lexbuf));  CartesianSyntax.FLOATVALUE (float_of_string floating) }
    | ('+'|'-')?['0'-'9']*'.'['0'-'9']+(('e'|'E')['0'-'9']+)? as floating { Debug.lexDebug (Printf.sprintf "FLOATVALUE='%s'" (Lexing.lexeme lexbuf));  CartesianSyntax.FLOATVALUE (float_of_string floating) }
    | "\"" allchar* "\"" as str { 
      let stripped_str = (String.sub str 1 (String.length str - 2)) in
      Debug.lexDebug (Printf.sprintf "STRINGVALUE='%s'" (Lexing.lexeme lexbuf));  CartesianSyntax.STRINGVALUE stripped_str }
    | "true" { Debug.lexDebug "BOOLVALUE=true"; CartesianSyntax.BOOLVALUE true }
    | "false" { Debug.lexDebug "BOOLVALUE=false"; CartesianSyntax.BOOLVALUE false }
    | ('_'('+'|'-'|'*'|'/'|'^'|'@'|':'':'|'|''|'|('&''&')|'~'|'=''='|'!''='|'<'|'>'|'<''='|'>''='|':'':')) as pIdent { Debug.lexDebug (Printf.sprintf "ID='%s'" (Lexing.lexeme lexbuf)); CartesianSyntax.ID pIdent }
    | "_" { Debug.lexDebug "SOULIGNE";  CartesianSyntax.SOULIGNE }
    | ['a'-'z''_''?']['a'-'z''A'-'Z''_''0'-'9''?']* as ident { Debug.lexDebug (Printf.sprintf "ID='%s'" (Lexing.lexeme lexbuf)); CartesianSyntax.ID ident }
    | ['A'-'Z''`']['a'-'z''A'-'Z''_''0'-'9''?']* as ident { Debug.lexDebug (Printf.sprintf "CAPID='%s'" (Lexing.lexeme lexbuf)); CartesianSyntax.CAPID ident }
    | ";;"  { Debug.lexDebug (Printf.sprintf "COMMANDEND"); CartesianSyntax.COMMANDEND } 
    | _ as c { Debug.lexDebug (Printf.sprintf "Error reading '%c'" c); raise LexicalError }

