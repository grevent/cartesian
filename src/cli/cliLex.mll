{ 
  open CliSyntax;;
}

let hexadecimal = ['0'-'9''a'-'f''A'-'F']
let unicode = "\\u" hexadecimal hexadecimal hexadecimal hexadecimal
let escaped = '\\' ('n' | 'r' | 't' | '\"' | '\\')
let char = [^'\"''\\']

let allchar = escaped | unicode | char
    
rule lexem = 
  parse 
    [' ' '\t'] { lexem lexbuf }  | 
    ['\r' '\n'] { lexem lexbuf } |
    "list" { Debug.lexDebug "LIST";  CliSyntax.LIST } |
	"objects" { Debug.lexDebug "OBJECTS"; CliSyntax.OBJECTS } |
	"object" { Debug.lexDebug "OBJECT"; CliSyntax.OBJECT } | 
    "edit" { Debug.lexDebug "EDIT"; CliSyntax.EDIT } |
    "exit" { Debug.lexDebug "EXIT"; CliSyntax.EXIT } |
    "exec" { Debug.lexDebug "EXEC"; CliSyntax.EXEC } |
    "new" { Debug.lexDebug "NEW"; CliSyntax.NEW } |
	('+'|'-')?['0'-'9']+ as integer { Debug.lexDebug (Printf.sprintf "INTVALUE='%s'" (Lexing.lexeme lexbuf)); CliSyntax.INTVALUE (int_of_string integer) } |
    ('_'('+'|'-'|'*'|'/'|'^'|'@'|':'':'|'|''|'|('&''&')|'~'|'=''='|'!''='|'<'|'>'|'<''='|'>''='|':'':')) as pIdent { Debug.lexDebug (Printf.sprintf "ID='%s'" (Lexing.lexeme lexbuf)); CliSyntax.ID pIdent } |
	['a'-'z''A'-'Z''_''?']['a'-'z''A'-'Z''_''0'-'9''?']* as ident { Debug.lexDebug (Printf.sprintf "ID='%s'" (Lexing.lexeme lexbuf)); CliSyntax.ID ident }

