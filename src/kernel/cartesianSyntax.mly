


%{
  open Tree
  open Type
  open Debug
  
  let currentId = ref 0;; 
  let newId() = 
	currentId := !currentId + 1;
	!currentId
;;
  
  let createFunctionCallExprn id params = FUNCTIONCALLEXPR (newId(),(IDEXPR (newId(),id)),params)
%}

%token ID ACOO ACOF PTVIRG FLECHD FLECHG
%token DO DEUXPOINTS LAMBDA LET IN MATCH IF
%token THEN ELSE CROO CROF PARO PARF INT CROOPIPE PIPECROF
%token QUOTE NOD ALL POSSIBLE EGAL AND VIRG AS
%token SOULIGNE PTPTPT PTDEUXPOINTS
%token PLUS MINUS MUL DIV PUISS INF SUP EGALEGAL NOTEGAL INFEGAL SUPEGAL PIPE
%token DEUXDEUXPOINTS PT PTPT WITH MOD NOT LOGICALOR LOGICALAND
%token LIST GENID TYPEDEF USE EXTERNAL DEUXPOINTSSUP DEUXPOINTSINF
%token INT FLOAT STRING OBJECT ARRAY IMPLY ACTION DECLARATION
%token TRANSITION RULE PTEXCFLECHD TYPE COPY NEW DELETE REPLACE
%token ACOOPIPE PIPEACOF DEFINE COMMANDEND

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
  
command: expr COMMANDEND { synDebug "command-1"; $1 }

genericIds: GENID genericIds { synDebug "genericIds-1"; $1::$2 }
genericIds: { synDebug "genericIds-2"; [] }

action: ID FLECHG expr { synDebug "action-1"; ASSIGNACTION ($1,$3) }
action: DO expr { synDebug "action-2"; DOACTION $2 (* Do each action in the list / object delivered by the evaluation *) }
action: exprProtected { synDebug "action-3"; EXPRACTION $1 }
action: COPY ID { synDebug "action-4"; COPYACTION $2 }
action: NEW ID EGAL expr { synDebug "action-5"; NEWACTION ($2,$4) }
action: DELETE ID { synDebug "action-6"; DELETEACTION $2 }
action: REPLACE ID WITH expr { synDebug "action-7"; REPLACEACTION ($2,$4) }
action: TYPE ID genericIds EGAL typeDefProtected  { synDebug "action-8"; DEFINETYPEACTION ($2,$3,$5) }
action: DEFINE ID patterns EGAL exprProtected { synDebug "action-9"; DEFINEACTION ($2,$3,$5) }
action: EXTERNAL ID DEUXPOINTS typeDefProtected { synDebug "action-10"; EXTERNACTION ($2,$4) }

actionListPtVirg: action PTVIRG actionListPtVirg { synDebug "actionListPtVirg-1"; $1::$3 }
actionListPtVirg: action { synDebug "actionListPtVirg-2"; [$1]}
actionListPtVirg: action PTVIRG { synDebug "actionListPtVirg-3"; [$1] }

expr: expr INF expr { synDebug "expr-1"; createFunctionCallExprn "_<" [$1; $3] }
expr: expr SUP expr {  synDebug "expr-2"; createFunctionCallExprn "_>" [$1; $3] }
expr: expr EGALEGAL expr { synDebug "expr-3"; createFunctionCallExprn "_==" [$1; $3] }
expr: expr NOTEGAL expr { synDebug "expr-4"; createFunctionCallExprn "_!=" [$1; $3] }
expr: expr INFEGAL expr { synDebug "expr-5"; createFunctionCallExprn "_<=" [$1; $3] }
expr: expr SUPEGAL expr { synDebug "expr-6"; createFunctionCallExprn "_>=" [$1; $3] }
expr: expr PLUS expr { synDebug "expr-7"; createFunctionCallExprn "_+" [$1; $3] }
expr: expr MINUS expr { synDebug "expr-8"; createFunctionCallExprn "_-" [$1; $3] }
expr: expr MUL expr { synDebug "expr-9"; createFunctionCallExprn "_*" [$1; $3] }
expr: expr MOD expr { synDebug "expr-10"; createFunctionCallExprn "_mod" [$1; $3] }
expr: expr DIV expr { synDebug "expr-11"; createFunctionCallExprn "_/" [$1; $3] }
expr: expr PUISS expr { synDebug "expr-12"; createFunctionCallExprn "_^" [$1; $3] }
expr: expr LOGICALOR expr { synDebug "expr-13"; createFunctionCallExprn "_||" [$1; $3] }
expr: expr LOGICALAND expr { synDebug "expr-14"; createFunctionCallExprn "_&&" [$1; $3] }
expr: expr DEUXDEUXPOINTS expr { synDebug "expr-15"; createFunctionCallExprn "_::" [$1; $3] }
expr: NOT expr { synDebug "expr-16"; createFunctionCallExprn "_not" [$2]  }
expr: LAMBDA matchExprs { synDebug "expr-17"; LAMBDAEXPR (newId(),$2) }
expr: LET assigns IN exprProtected { synDebug "expr-18"; LETEXPR (newId(),$2,$4) }
expr: MATCH expr WITH matchExprs { synDebug "expr-19"; MATCHEXPR (newId(),$2,$4) }
expr: MATCH POSSIBLE expr WITH matchExprs { synDebug "expr-20"; MATCHPOSSIBLEEXPR (newId(),$3,$5) }
expr: exprProtected PTDEUXPOINTS typeDefProtected { synDebug "expr-21"; TYPEACCESSEXPR (newId(),$1,$3) }
expr: IF expr THEN expr ELSE exprProtected { synDebug "expr-22"; createFunctionCallExprn "_if" [$2; $4; $6] }
expr: expr PT CROO expr CROF { synDebug "expr-23"; createFunctionCallExprn "_get" [$1; $4] }
expr: exprProtected { synDebug "expr-24"; $1 }
expr: objectDef { synDebug "expr-25"; OBJEXPR (newId(),$1) }
expr: TRANSITION transition { synDebug "expr-26"; TRANSITIONEXPR (newId(),$2) }

exprProtected: PARO expr DEUXPOINTS typeDef PARF { synDebug "exprProtected-1"; TYPEVERIFICATIONEXPR (newId(),$2,$4) }
exprProtected: PARO expr DEUXPOINTSSUP typeDef PARF { synDebug "exprProtected-2"; TOSUBTYPEEXPR (newId(),$2,$4)  }
exprProtected: PARO expr DEUXPOINTSINF typeDef PARF { synDebug "exprProtected-3"; CONVERSIONEXPR (newId(),$2,$4)  }
exprProtected: PARO expr exprProtectedList PARF { synDebug "exprProtected-4"; FUNCTIONCALLEXPR (newId(),$2,$3) }
exprProtected: INTVALUE { synDebug "exprProtected-5"; INTEXPR $1 }
exprProtected: FLOATVALUE { synDebug "exprProtected-6"; FLOATEXPR $1 }
exprProtected: STRINGVALUE { synDebug "exprProtected-7"; STRINGEXPR $1 }
exprProtected: BOOLVALUE { synDebug "exprProtected-8"; BOOLEXPR $1 }
exprProtected: ID { synDebug "exprProtected-9"; IDEXPR (newId(),$1) }
exprProtected: ACOO actionListPtVirg ACOF { synDebug "exprProtected-10"; ACTIONEXPR (newId(),$2) }
exprProtected: CROO exprListPtVirg CROF { synDebug "exprProtected-11"; LISTEXPR (newId(),$2) } 
exprProtected: CROO CROF { synDebug "exprProtected-12"; LISTEXPR (newId(),[]) }
exprProtected: CROO expr PTPT expr CROF { synDebug "exprProtected-13"; INTERVALEXPR (newId(),$2,$4) }
exprProtected: CROO expr PTVIRG expr PTPT expr CROF { synDebug "exprProtected-14"; INTERVALSTEPEXPR (newId(),$2,$4,$6) }
exprProtected: NOD { synDebug "exprProtected-15"; NODEXPR }
exprProtected: PARO exprVirgList PARF { synDebug "exprProtected-16"; PAIREXPR (newId(),$2) }
exprProtected: CROO exprProtected PIPE pattern IN expr CROF { synDebug "exprProtected-17"; LISTCOMPREHENSIONEXPR (newId(),$2,$4,$6) }
exprProtected: CROOPIPE exprListPtVirg PIPECROF { synDebug "exprProtected-18"; ARRAYEXPR (newId(),$2) }

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
 
