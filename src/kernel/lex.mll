{ 
  open Syntax;;
  
  exception Eof;;
  exception LexicalError;;
}

let hexadecimal = ['0'-'9''a'-'f''A'-'F']
let unicode = "\\u" hexadecimal hexadecimal hexadecimal hexadecimal
let escaped = '\\' ('n' | 'r' | 't' | '\"' | '\\')
let char = [^'\"''\\']

let allchar = escaped | unicode | char
    
rule lexer = 
  parse 
    [' ' '\t'] { Debug.lexDebug "SPACE -> Ignoring"; lexer lexbuf } 
| ['\r' '\n'] { Debug.lexDebug "EOL"; Syntax.EOL }
    | "+" { Debug.lexDebug "PLUS"; Syntax.PLUS }
    | "-" { Debug.lexDebug "MINUS"; Syntax.MINUS }
    | "*" { Debug.lexDebug "MUL"; Syntax.MUL }
    | "/" { Debug.lexDebug "DIV"; Syntax.DIV }
    | "^" { Debug.lexDebug "PUISS"; Syntax.PUISS }
    | "<" { Debug.lexDebug "INF"; Syntax.INF }
    | ">" { Debug.lexDebug "SUP"; Syntax.SUP }
    | "==" { Debug.lexDebug "EGALEGAL"; Syntax.EGALEGAL }
    | "!=" { Debug.lexDebug "NOTEGAL"; Syntax.NOTEGAL }
    | "<=" { Debug.lexDebug "INFEGAL"; Syntax.INFEGAL }
    | ">=" { Debug.lexDebug "INFEGAL"; Syntax.SUPEGAL }
    | "[" { Debug.lexDebug "CROO"; Syntax.CROO }
    | "]" { Debug.lexDebug "CROF"; Syntax.CROF }
    | "[|" { Debug.lexDebug "CROOPIPE"; Syntax.CROOPIPE }
    | "|]" { Debug.lexDebug "PIPECROF"; Syntax.PIPECROF }
    | "|" { Debug.lexDebug "PIPE"; Syntax.PIPE }
    | "{" { Debug.lexDebug "ACOO"; Syntax.ACOO }
    | "}" { Debug.lexDebug "ACOF"; Syntax.ACOF }
    | "lambda" { Debug.lexDebug "LAMBDA"; Syntax.LAMBDA }
    | "->" { Debug.lexDebug "FLECHD"; Syntax.FLECHD }
    | "<-" { Debug.lexDebug "FLECHG"; Syntax.FLECHG }
    | "==>" { Debug.lexDebug "SEND"; Syntax.SEND }
    | "!=>" { Debug.lexDebug "STARTSEND"; Syntax.STARTSEND }
    | "<==" { Debug.lexDebug "RECEIVE"; Syntax.RECEIVE }
    | "let" { Debug.lexDebug "LET"; Syntax.LET }
    | "in" { Debug.lexDebug "IN"; Syntax.IN }
    | "::" { Debug.lexDebug "DEUXDEUXPOINTS"; Syntax.DEUXDEUXPOINTS }
    | "=" { Debug.lexDebug "EGAL"; Syntax.EGAL }
    | ":" { Debug.lexDebug "DEUXPOINTS"; Syntax.DEUXPOINTS }
    | "." { Debug.lexDebug "PT"; Syntax.PT }
    | "'" { Debug.lexDebug "QUOTE"; Syntax.QUOTE }
    | "(" { Debug.lexDebug "PARO"; Syntax.PARO }
    | ")" { Debug.lexDebug "PARF"; Syntax.PARF }
    | "match" { Debug.lexDebug "MATCH"; Syntax.MATCH }
    | "with" { Debug.lexDebug "WITH"; Syntax.WITH }
    | "and" { Debug.lexDebug "AND"; Syntax.AND }
    | "mod" { Debug.lexDebug "MOD"; Syntax.MOD }
    | "while" { Debug.lexDebug "WHILE"; Syntax.WHILE }
    | "do" { Debug.lexDebug "DO"; Syntax.DO }
    | "for" { Debug.lexDebug "FOR"; Syntax.FOR }
    | "not" { Debug.lexDebug "NOT"; Syntax.NOT }
    | "context" { Debug.lexDebug "CONTEXT"; Syntax.CONTEXT }
    | "||" { Debug.lexDebug "LOGICALOR"; Syntax.LOGICALOR }
    | "&&" { Debug.lexDebug "LOGICALAND"; Syntax.LOGICALAND }
    | ";" { Debug.lexDebug "PTVIRG"; Syntax.PTVIRG }
    | "," { Debug.lexDebug "VIRG"; Syntax.VIRG }
    | "_" { Debug.lexDebug "SOULIGNE"; Syntax.SOULIGNE }
    | "as" { Debug.lexDebug "AS"; Syntax.AS }
    | "nod" { Debug.lexDebug "NOD"; Syntax.NOD }
    | "if" { Debug.lexDebug "IF"; Syntax.IF }
    | "then" { Debug.lexDebug "IF"; Syntax.THEN }
    | "else" { Debug.lexDebug "ELSE"; Syntax.ELSE }
    | "raise" { Debug.lexDebug "RAISE"; Syntax.RAISE }
    | "try" { Debug.lexDebug "TRY"; Syntax.TRY }
    | "//" { Debug.lexDebug "THREAD"; Syntax.THREAD }
    | ('+'|'-')?['0'-'9']+ as integer { Debug.lexDebug (Printf.sprintf "INTVALUE='%s'" (Lexing.lexeme lexbuf)); Syntax.INTVALUE (int_of_string integer) }
    | ('+'|'-')?['0'-'9']+'.'['0'-'9']*(('e'|'E')['0'-'9']+)? as floating { Debug.lexDebug (Printf.sprintf "FLOATVALUE='%s'" (Lexing.lexeme lexbuf));  Syntax.FLOATVALUE (float_of_string floating) }
    | ('+'|'-')?['0'-'9']*'.'['0'-'9']+(('e'|'E')['0'-'9']+)? as floating { Debug.lexDebug (Printf.sprintf "FLOATVALUE='%s'" (Lexing.lexeme lexbuf));  Syntax.FLOATVALUE (float_of_string floating) }
    | "\"" allchar* "\"" as str { 
      let stripped_str = (String.sub str 1 (String.length str - 2)) in
      Debug.lexDebug (Printf.sprintf "STRINGVALUE='%s'" (Lexing.lexeme lexbuf));  Syntax.STRINGVALUE stripped_str }
    | "true" { Debug.lexDebug "BOOLVALUE=true"; Syntax.BOOLVALUE true }
    | "false" { Debug.lexDebug "BOOLVALUE=false"; Syntax.BOOLVALUE false }
    | ('_'('+'|'-'|'*'|'/'|'^'|'@'|':'':'|'|''|'|('&''&')|'~'|'=''='|'!''='|'<'|'>'|'<''='|'>''='|':'':')) as pIdent { Debug.lexDebug (Printf.sprintf "ID='%s'" (Lexing.lexeme lexbuf)); Syntax.ID pIdent }
    | "_" { Debug.lexDebug "SOULIGNE";  Syntax.SOULIGNE }
    | ['a'-'z''A'-'Z''_''?']['a'-'z''A'-'Z''_''0'-'9''?']* as ident { Debug.lexDebug (Printf.sprintf "ID='%s'" (Lexing.lexeme lexbuf)); Syntax.ID ident }
    | _ as c { Debug.lexDebug (Printf.sprintf "Error reading '%c'" c); raise LexicalError }
    | eof { Debug.lexDebug "EOF"; raise Eof }
