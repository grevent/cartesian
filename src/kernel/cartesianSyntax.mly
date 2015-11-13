


%{
  open Tree
  open Type
  
  let currentId = ref 0;; 
  let newId() = 
	currentId := !currentId + 1;
	!currentId
;;
  
  let createFunctionCallExprn id params = FUNCTIONCALLEXPR (newId(),(IDEXPR (newId(),id)),params)
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
%token TRANSITION RULE PTEXCFLECHD TYPE COPY NEW DELETE REPLACE
%token ACOOPIPE PIPEACOF DEFINE

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
      
%type <Tree.exprNode> command
%type <Tree.objectNode> objectDef
								 
%start command
       
%%
  
command: expr EOL { $1 }

genericIds: GENID genericIds { $1::$2 }
genericIds: { [] }

action: ID FLECHG expr { ASSIGNACTION ($1,$3) }
action: DO expr { DOACTION $2 (* Do each action in the list / object delivered by the evaluation *) }
action: exprProtected { EXPRACTION $1 }
action: COPY ID { COPYACTION $2 }
action: NEW ID EGAL expr { NEWACTION ($2,$4) }
action: DELETE ID { DELETEACTION $2 }
action: REPLACE ID WITH expr { REPLACEACTION ($2,$4) }
action: TYPE ID genericIds EGAL typeDefProtected  { DEFINETYPEACTION ($2,$3,$5) }
action: DEFINE ID patterns EGAL exprProtected { DEFINEACTION ($2,$3,$5) }
action: EXTERNAL ID DEUXPOINTS typeDefProtected { EXTERNACTION ($2,$4) }

actionListPtVirg: action PTVIRG actionListPtVirg { $1::$3 }
actionListPtVirg: action { [$1]}
actionListPtVirg: action PTVIRG { [$1] }

expr: expr INF expr { createFunctionCallExprn "_<" [$1; $3] }
expr: expr SUP expr {  createFunctionCallExprn "_>" [$1; $3] }
expr: expr EGALEGAL expr { createFunctionCallExprn "_==" [$1; $3] }
expr: expr NOTEGAL expr { createFunctionCallExprn "_!=" [$1; $3] }
expr: expr INFEGAL expr { createFunctionCallExprn "_<=" [$1; $3] }
expr: expr SUPEGAL expr { createFunctionCallExprn "_>=" [$1; $3] }
expr: expr PLUS expr { createFunctionCallExprn "_+" [$1; $3] }
expr: expr MINUS expr { createFunctionCallExprn "_-" [$1; $3] }
expr: expr MUL expr { createFunctionCallExprn "_*" [$1; $3] }
expr: expr MOD expr { createFunctionCallExprn "_mod" [$1; $3] }
expr: expr DIV expr { createFunctionCallExprn "_/" [$1; $3] }
expr: expr PUISS expr { createFunctionCallExprn "_^" [$1; $3] }
expr: expr LOGICALOR expr { createFunctionCallExprn "_||" [$1; $3] }
expr: expr LOGICALAND expr { createFunctionCallExprn "_&&" [$1; $3] }
expr: expr DEUXDEUXPOINTS expr { createFunctionCallExprn "_::" [$1; $3] }
expr: NOT expr { createFunctionCallExprn "_not" [$2]  }
expr: LAMBDA matchExprs { LAMBDAEXPR (newId(),$2) }
expr: LET assigns IN exprProtected { LETEXPR (newId(),$2,$4) }
expr: MATCH expr WITH matchExprs { MATCHEXPR (newId(),$2,$4) }
expr: MATCH POSSIBLE expr WITH matchExprs { MATCHPOSSIBLEEXPR (newId(),$3,$5) }
expr: exprProtected PTDEUXPOINTS typeDefProtected { TYPEACCESSEXPR (newId(),$1,$3) }
expr: IF expr THEN expr ELSE exprProtected { createFunctionCallExprn "_if" [$2; $4; $6] }
expr: expr PT CROO expr CROF { createFunctionCallExprn "_get" [$1; $4] }
expr: exprProtected { $1 }
expr: objectDef { OBJEXPR (newId(),$1) }
expr: TRANSITION transition { TRANSITIONEXPR (newId(),$2) }

exprProtected: PARO expr DEUXPOINTS typeDef PARF { TYPEVERIFICATIONEXPR (newId(),$2,$4) }
exprProtected: PARO expr DEUXPOINTSSUP typeDef PARF { TOSUBTYPEEXPR (newId(),$2,$4)  }
exprProtected: PARO expr DEUXPOINTSINF typeDef PARF { CONVERSIONEXPR (newId(),$2,$4)  }
exprProtected: PARO expr exprProtectedList PARF { FUNCTIONCALLEXPR (newId(),$2,$3) }
exprProtected: INTVALUE { INTEXPR $1 }
exprProtected: FLOATVALUE { FLOATEXPR $1 }
exprProtected: STRINGVALUE { STRINGEXPR $1 }
exprProtected: BOOLVALUE { BOOLEXPR $1 }
exprProtected: ID { IDEXPR (newId(),$1) }
exprProtected: ACOO actionListPtVirg ACOF { ACTIONEXPR (newId(),$2) }
exprProtected: CROO exprListPtVirg CROF { LISTEXPR (newId(),$2) } 
exprProtected: CROO CROF { LISTEXPR (newId(),[]) }
exprProtected: CROO expr PTPT expr CROF {INTERVALEXPR (newId(),$2,$4) }
exprProtected: CROO expr PTVIRG expr PTPT expr CROF { INTERVALSTEPEXPR (newId(),$2,$4,$6) }
exprProtected: NOD { NODEXPR }
exprProtected: PARO exprVirgList PARF { PAIREXPR (newId(),$2) }
exprProtected: CROO exprProtected PIPE pattern IN expr CROF { LISTCOMPREHENSIONEXPR (newId(),$2,$4,$6) }
exprProtected: CROOPIPE exprListPtVirg PIPECROF { ARRAYEXPR (newId(),$2) }

exprVirgList: expr VIRG exprVirgList { $1::$3 }
exprVirgList: expr { [ $1 ] }

objectDef: ACOOPIPE objectDefinitions PIPEACOF { OBJECT (LOCAL,$2) }
objectDef: ACOOPIPE ID PIPE objectDefinitions PIPEACOF { OBJECT ((INTERFACE $2),$4) }

transition: objectPatterns IMPLY exprProtected { EXPRTRANS ($1,$3) }
transition: objectPatterns PTEXCFLECHD exprProtected { ACTIONTRANS ($1,$3) (* Should return an object ! *) }

typeDef: typeDefProtected variants { ALTERNATIVESTYPE ($1::$2) }

typeDefProtected: NOD { NODTYPE }
typeDefProtected: INT { INTTYPE }
typeDefProtected: FLOAT { FLOATTYPE }
typeDefProtected: STRING { STRINGTYPE }
typeDefProtected: CROOPIPE typeDef PIPECROF { ARRAYTYPE $2 }
typeDefProtected: CROO typeDef CROF { LISTTYPE $2 }
typeDefProtected: GENID { GENTYPE $1 }
typeDefProtected: PARO typeDefVirgList PARF  { PAIRTYPE $2 }
typeDefProtected: ID { NAMEDTYPE ([],$1) }
typeDefProtected: PARO ID typeDefList PARF { NAMEDTYPE ($3,$2) }
typeDefProtected: OBJECT { OBJECTTYPE }
typeDefProtected: TRANSITION { TRANSITIONTYPE }

typeDefList: typeDefProtected typeDefList { $1::$2 }
typeDefList: typeDefProtected  { [$1] }

variants: PIPE typeDefProtected variants { $2::$3 }
variants: { [] } 

typeDefVirgList: typeDef VIRG typeDefVirgList { $1::$3 }
typeDefVirgList: typeDef { [$1] } 

lambdaExpr: patterns FLECHD exprProtected { ($1,$3) }

matchExprs: lambdaExpr PIPE matchExprs { $1::$3 }
matchExprs: lambdaExpr { [ $1 ] }

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

pattern: pattern DEUXDEUXPOINTS pattern { CONSPATTERN (newId(),$1,$3) }
pattern: patternProtected { $1 }

patternProtected: INTVALUE { INTPATTERN $1 }
patternProtected: FLOATVALUE { FLOATPATTERN $1 }
patternProtected: STRINGVALUE { STRINGPATTERN $1 }
patternProtected: BOOLVALUE { BOOLPATTERN $1 }
patternProtected: CROO patternListPtVirg CROF { LISTPATTERN (newId(),$2) }
patternProtected: CROO CROF { LISTPATTERN (newId(),[]) }
patternProtected: patternProtected AS ID { RENAMINGPATTERN (newId(),$1,$3) }
patternProtected: SOULIGNE { WILDCARDPATTERN (newId()) }
patternProtected: PARO patternListVirg PARF { PAIRPATTERN (newId(),$2) }
patternProtected: ID { IDPATTERN (newId(),$1) }
patternProtected: patternProtected WHERE exprProtected { WHEREPATTERN (newId(),$1,$3) }
patternProtected: CROOPIPE patternListPtVirg PIPECROF { ARRAYPATTERN (newId(),$2) }
patternProtected: PARO pattern DEUXPOINTS typeDef PARF { TYPEDPATTERN (newId(),$2,$4) }

objectPattern: ACOO patternObjectDefinitions ACOF { $2 }

patternObjectDefinitions: patternObjectDefinition PTVIRG patternObjectDefinitions { $1::$3 }
patternObjectDefinitions: patternObjectDefinition PTVIRG { [ $1 ] }
patternObjectDefinitions: patternObjectDefinition { [ $1 ] }
patternObjectDefinitions: PTPTPT { [ OPENOBJPATTERN ] }

patternObjectDefinition: ID EGAL pattern { OBJPATTERN ($1,$3) }

objectPatterns: objectPattern objectPatterns { $1::$2 }
objectPatterns: objectPattern { [ $1 ] }

exprProtectedList: exprProtected exprProtectedList { $1::$2 }
exprProtectedList: exprProtected { [ $1 ] }

patternListPtVirg: pattern PTVIRG patternListPtVirg { $1::$3 }
patternListPtVirg: pattern { [ $1 ] }
 
