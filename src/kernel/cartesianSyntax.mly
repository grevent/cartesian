


%{
  open CartesianTree
  open Types
  
  let createFunctionCallExpression id params = FUNCTIONCALLEXPRESSION (NOTEVALUATED,((IDEXPRESSION (NOTEVALUATED,id)),params))
%}

%token EOL ID ACOO ACOF PTVIRG FLECHD FLECHG
%token DO DEUXPOINTS LAMBDA LET IN MATCH IF
%token THEN ELSE CROO CROF PARO PARF INT CROOPIPE PIPECROF
%token QUOTE NOD ALL POSSIBLE EGAL AND VIRG AS
%token SOULIGNE PTPTPT PTDEUXPOINTS
%token PLUS MINUS MUL DIV PUISS INF SUP EGALEGAL NOTEGAL INFEGAL SUPEGAL PIPE
%token DEUXDEUXPOINTS PT PTPT WITH MOD NOT LOGICALOR LOGICALAND
%token LIST GENID TYPEDEF USE EXTERNAL DEUXPOINTSSUP DEUXPOINTSINF
%token INT FLOAT STRING OBJECT ARRAY IMPLY ACTION DECLARATION
%token TRANSITION RULE PTEXCFLECHD

%token <string> ID COMMENT STRINGVALUE GENID
%token <int> INTVALUE
%token <float> FLOATVALUE
%token <bool> BOOLVALUE

%left PIPE
%left WHERE
%left WITH
%left SUPEGAL INFEGAL INF SUP EGALEGAL NOTEGAL 
%left PLUS MINUS LOGICALOR
%left MUL MOD DIV LOGICALAND
%left DEUXDEUXPOINTS
%left PUISS
%left PT
%left NOT
      
%type <(CartesianTree.cartesianPattern*(CartesianTree.cartesianPattern list)*(CartesianTree.cartesianExpression)) list> assigns
%type <CartesianTree.cartesianPattern> pattern

%type <int> command
					 
%start command
       
%%
  
command: ACTION action EOL { ACTION $2 }
command: DECLARATION declaration EOL { DECLARATION $2 }
command: TRANSITION transition EOL { TRANSITION $2 }
command: OBJECT object EOL { OBJECT $2 }

object: ACOO syncMethod objectDefinitions ACOF { $2 }

declaration: TYPEDEF ID genericIds EGAL typeDef  { DEFINETYPE ($2,$3,$5) }
declaration: USE ID { USE $2 }
declaration: ID patterns EGAL expr { DEFINE ($1,$2,$4) }
declaration: EXTERNAL ID DEUXPOINTS typeDef { EXTERN ($2,$4) }
declaration: ID EGAL object {OBJECTDECL ($1,$3) }

transition: objectPatterns IMPLY expr { RULE ($2,$4) }
transition: objectPatterns PTEXCFLECHD actionListPtVirg { TRANSITION ($2,$4) }

action: ID FLECHG expr { ASSIGNACTION ($1,$3) }
action: DO expr { DO $2 (* Do each action in the list / object delivered by the evaluation *) }
action: expr { EXPRACTION $1 }
action: COPY ID { COPY $1 }

actionListPtVirg: action PTVIRG actionListPtVirg { $1::$3 }
actionListPtVirg: action { [$1]}
actionListPtVirg: action PTVIRG { [$1] }

expr: expr INF expr { INFEXPR (UNKNOWN,$1,$3) }
expr: expr SUP expr { SUPEXPR (UNKOWN,$1,$3) }
expr: expr EGALEGAL expr { EGALEGALEXPR (UNKNOWN,$1,$3) }
expr: expr NOTEGAL expr { NOTEGALEXPR (UNKNOWN,$1,$3) }
expr: expr INFEGAL expr { INFEGALEXPR (UNKNOWN,$1,$3) }
expr: expr SUPEGAL expr { SUPEGALEXPR (UNKNOWN,$1,$3) }
expr: expr PLUS expr { PLUSEXPR (UNKNOWN,$1,$3) }
expr: expr MINUS expr { MINUSEXPR (UNKNOWN,$1,$3) }
expr: expr MUL expr { MULEXPR (UNKNOWN,$1,$3) }
expr: expr MOD expr { MODEXPR (UNKNOWN,$1,$3) }
expr: expr DIV expr { DIVEXPR (UNKNOWN,$1,$3) }
expr: expr PUISS expr { PUISSEXPR (UNKNOWN,$1,$3) }
expr: expr LOGICALOR expr { LOGICALOREXPR (UNKNOWN,$1,$3) }
expr: expr LOGICALAND expr { LOGICALANDEXPR (UNKNOWN,$1,$3) }
expr: expr DEUXDEUXPOINTS expr { DEUXDEUXPOINTSEXPR (UNKNOWN,$1,$3) }
expr: NOT expr { NOTEXPR (UNKNOWN,$2)  }
expr: LAMBDA matchExprs { LAMBDA (UNKNOWN,$2) }
expr: LET assigns IN exprProtected { LET (UNKNOWN,$2,$4) }
expr: MATCH depthModification expr WITH matchExprs { MATCH (UNKNOWN,$2,$3,$5) }
expr: exprProtected PTDEUXPOINTS typeDef { TYPEACCESS (UNKNOWN,$1,$3) }
expr: IF expr THEN expr ELSE exprProtected { IF (UNKNOWN,$2,$4,$6) }
expr: expr PT CROO expr CROF { ARRAYACCESS (UNKNOWN,$1,$4) }
expr: exprProtected { $1 }

exprProtected: PARO expr DEUXPOINTS typeDef PARF { TYPEVERIFICATION (UNKNOWN,$2,$4) }
exprProtected: PARO expr DEUXPOINTSSUP typeDef PARF { TOSUBTYPE (UNKNOWN,$2,$4) (* Used for in cases like 1 :> myInt *) }
exprProtected: PARO expr DEUXPOINTSINF typeDef PARF { CONVERSION (UNKNOWN,(2,$4) (* Used in cases like (x: int) :< float, which is a conversion... }
exprProtected: PARO expr exprProtectedList PARF { FUNCTIONCALL (UNKNOWN,$2,$3) }
exprProtected: INTVALUE { INTVAL (INT,$1) }
exprProtected: FLOATVALUE { FLOATEXPR (FLOAT,$1) }
exprProtected: STRINGVALUE { STRINGEXPR (STRING,$1) }
exprProtected: BOOLVALUE { BOOLEXPR (BOOL,$1) }
exprProtected: ID { ID (UNKNOWN,$1) }
exprProtected: ACOO actionListPtVirg ACOF { ACTIONEXPR (ACTION,(SEQUENCE $2)) }
exprProtected: CROO exprListPtVirg CROF { LISTEXPR (UNKNOWN,$2) } 
exprProtected: CROO CROF { LISTEXPR (UNKNOWN,[]) }
exprProtected: CROO expr PTPT expr CROF {INTERVAL (UNKNOWN,$2,$4) }
exprProtected: CROO expr PTVIRG expr PTPT expr CROF { INTERVALSTEP (UNKNOWN,$2,$4,$6) }
exprProtected: NOD { NODEXPR NOD }
exprProtected: PARO exprVirgList PARF { PAIR (UNKNOWN,$2) }
exprProtected: CROO exprProtected PIPE pattern IN expr CROF { LISTCOMPREHENSION (UNKNOWN,$2,$q4,$6) }
exprProtected: CROOPIPE exprListPtVirg PIPECROF { ARRAY (UNKNOWN,$2) }

exprVirgList: expr VIRG exprVirgList { $1::$3 }
exprVirgList: expr { [ $1 ] }

typeDefProtected: NOD { NODTYPE }
typeDefProtected: INT { INTTYPE }
typeDefProtected: FLOAT { FLOATTYPE }
typeDefProtected: STRING { STRINGTYPE }
typeDefProtected: CROOPIPE typeDef PIPECROF { ARRAYTYPE $2 }
typeDefProtected: CROO typeDef CROF { LISTTYPE $2 }
typeDefProtected: GENID { GENTYPE $1 }
typeDefProtected: PARO typeDefVirgList PARF  { PAIRTYPE $2 }
typeDefProtected: ID { NAMEDTYPE ($1,[]) }
typeDefProtected: PARO typeDefList ID PARF { NAMEDTYPE ($2,$3) }

typeDef: typeDefProtected variants { ALTERNATIVESTYPE ($1::$2) }

typeDefList: typeDefProtected typeDefList { $1::$2 }
typeDefList: typeDefProtected { [$1] }

genericIds: GENID genericIds { $1::$2 }
genericIds: { [] }

variants: PIPE typeDefProtected variants { $2::$3 }
variants: { [] } 

typeDefVirgList: typeDef VIRG typeDefVirgList { $1::$3 }
typeDefVirgList: typeDef { [$1] } 

lambdaExpr: patterns FLECHD exprProtected { ($1,$3) }

matchExprs: lambdaExpr PIPE matchExprs { $1::$3 }
matchExprs: lambdaExpr { [ $1 ] }

depthModification: { ONEELEMENT (* Just matchs one element, not the elements in a list *) }
depthModification: POSSIBLE { ALLPOSSIBLEELEMENTS (* The one not matching are just discarded... *) } 

assigns: pattern patterns EGAL expr AND assigns { ($1,$2,$4)::$6 }
assigns: pattern patterns EGAL expr { [($1,$2,$4)] }

exprListPtVirg: expr PTVIRG exprListPtVirg { $1::$3 }
exprListPtVirg: expr { [$1] } 

objectDefinitions: ID EGAL expr PTVIRG objectDefinitions { ($1,$3)::$5 }
objectDefinitions: ID EGAL expr { [ ($1,$3) ] }

patterns: patternProtected patterns { $1::$2 }
patterns: { [] }

patternListVirg: patternProtected VIRG patternListVirg { $1::$3 }
patternListVirg: patternProtected { [$1] }

pattern: pattern DEUXDEUXPOINTS pattern { CONSPATTERN (NOTEVALUATED,($1,$3)) }
pattern: patternProtected { $1 }

patternProtected: INTVALUE { INTPATTERN (NOTEVALUATED,$1) }
patternProtected: FLOATVALUE { FLOATPATTERN (NOTEVALUATED,$1) }
patternProtected: STRINGVALUE { STRINGPATTERN (NOTEVALUATED,$1) }
patternProtected: BOOLVALUE { BOOLPATTERN (NOTEVALUATED,$1) }
patternProtected: CROO patternListPtVirg CROF { LISTPATTERN (NOTEVALUATED,$2) }
patternProtected: CROO CROF { LISTPATTERN (NOTEVALUATED,[]) }
patternProtected: patternProtected AS ID { RENAMINGPATTERN (NOTEVALUATED,($1,$3)) }
patternProtected: SOULIGNE { WILDCARDPATTERN NOTEVALUATED }
patternProtected: PARO patternListVirg PARF { PAIRPATTERN (NOTEVALUATED,$2) }
patternProtected: ID { IDPATTERN (NOTEVALUATED,$1) }
patternProtected: patternProtected WHERE exprProtected { WHEREPATTERN (NOTEVALUATED,($1,$3)) }
patternProtected: CROOPIPE patternListPtVirg PIPECROF { ARRAYPATTERN (NOTEVALUATED,$2) }
patternProtected: PARO pattern DEUXPOINTS typeDef PARF { TYPEDPATTERN (NOTEVALUATED,($2,$4)) }

objectPattern: ACOO patternObjectDefinitions ACOF { let (isOpen,defs) = $2 in if isOpen then (OPENOBJECTPATTERN (NOTEVALUATED,defs)) else (CLOSEDOBJECTPATTERN (NOTEVALUATED,defs)) }

patternObjectDefinitions: patternObjectDefinition PTVIRG patternObjectDefinitions { let (isOpen,defs) = $3 in (isOpen,$1::defs) }
patternObjectDefinitions: patternObjectDefinition PTVIRG { (false,[$1]) }
patternObjectDefinitions: patternObjectDefinition { (false,[$1]) }
patternObjectDefinitions: PTPTPT { (true,[]) }

patternObjectDefinition: ID EGAL pattern { ($1,$3) }

objectPatterns: objectPattern objectPatterns { $1::$2 }
objectPatterns: objectPattern { [ $1 ] }

exprProtectedList: exprProtected exprProtectedList { $1::$2 }
exprProtectedList: exprProtected { [ $1 ] }

patternListPtVirg: pattern PTVIRG patternListPtVirg { $1::$3 }
patternListPtVirg: pattern { [ $1 ] }

syncMethod: { LOCAL }
syncMethod: PIPE ID PIPE { FRONTEND $2 }
 
