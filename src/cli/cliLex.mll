{ 
  open CliSyntax;;
  
  exception Eof;;
  exception LexicalError;;
}

let hexadecimal = ['0'-'9''a'-'f''A'-'F']
let unicode = "\\u" hexadecimal hexadecimal hexadecimal hexadecimal
let escaped = '\\' ('n' | 'r' | 't' | '\"' | '\\')
let char = [^'\"''\\']

let allchar = escaped | unicode | char
    
rule lexem = 
  parse 
    [' ' '\t'] { lexem lexbuf }  | 
    ['\r' '\n'] { CliSyntax.EOL } |
    "list" { CliSyntax.LIST } |
    "edit" { CliSyntax.EDIT } |
	('+'|'-')?['0'-'9']+ as integer { Debug.lexDebug (Printf.sprintf "INTVALUE='%s'" (Lexing.lexeme lexbuf)); CliSyntax.INTVALUE (int_of_string integer) } |
    ('_'('+'|'-'|'*'|'/'|'^'|'@'|':'':'|'|''|'|('&''&')|'~'|'=''='|'!''='|'<'|'>'|'<''='|'>''='|':'':')) as pIdent { Debug.lexDebug (Printf.sprintf "ID='%s'" (Lexing.lexeme lexbuf)); CliSyntax.ID pIdent }
 
