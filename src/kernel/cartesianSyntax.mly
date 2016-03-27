%{
  open CartesianDataModel
  open Debug
  
  let currentId = ref 0;; 
  let newId() = 
	currentId := !currentId + 1;
	!currentId
;;
  
  let rec createLambdaExpr patterns expr = match patterns with [] -> expr | car::cdr -> LAMBDAEXPR (newId(),car,(createLambdaExpr cdr expr));;
  let rec createFunctionCallExpr expr params = match params with [] ->  expr | car::cdr -> FUNCTIONCALLEXPR (newId(),(createFunctionCallExpr expr cdr),car);;
  let createIdCallExpr id params = createFunctionCallExpr (IDEXPR (newId(),id)) params;;
%}

%token ID ACOO ACOF PTVIRG FLECHD FLECHG
%token DO DEUXPOINTS LAMBDA LET IN MATCH IF
%token THEN ELSE CROO CROF PARO PARF INT CROOPIPE PIPECROF
%token QUOTE NOD ALL POSSIBLE EGAL AND VIRG AS
%token SOULIGNE PTPTPT PTDEUXPOINTS INCLUDE
%token PLUS MINUS MUL DIV PUISS INF SUP EGALEGAL NOTEGAL INFEGAL SUPEGAL PIPE
%token DEUXDEUXPOINTS PT PTPT WITH MOD NOT LOGICALOR LOGICALAND
%token LIST GENID TYPEDEF USE EXTERNAL DEUXPOINTSSUP DEUXPOINTSINF
%token INT FLOAT STRING OBJECT ARRAY IMPLY ACTION DECLARATION
%token TRANSITION RULE PTEXCFLECHD TYPE COPY NEW DELETE REPLACE
%token ACOOPIPE PIPEACOF DEFINE COMMANDEND INFINF SUPSUP 
%token INFPT SUPPT INFEGALPT SUPEGALPT EGALEGALPT NOTEGALPT PLUSPT MINUSPT MULPT DIVPT
%token INTERFACE ACOOINF SUPACOF INFSUP NOW ACOOTILDE TILDEACOF ERROR

%token <string> ID COMMENT STRINGVALUE GENID CAPID
%token <int> INTVALUE
%token <float> FLOATVALUE
%token <bool> BOOLVALUE

%left PIPE
%left WHERE
%left WITH
%left SUPEGAL INFEGAL INF SUP EGALEGAL NOTEGAL SUPEGALPT INFEGALPT INFPT SUPPT EGALEGALPT NOTEGALPT 
%left PLUS MINUS PLUSPT MINUSPT LOGICALOR
%left MUL MOD DIV LOGICALAND MULPT DIVPT 
%left DEUXDEUXPOINTS
%left PUISS
%left PT
%left NOT
      
%type <CartesianDataModel.actionNode> command
%type <CartesianDataModel.exprNode list> exprProtectedList
								 
%start command
       
%%
  
command: action COMMANDEND { synDebug "command-1"; $1 }

action: ID FLECHG expr { synDebug "action-1"; ASSIGNACTION ($1,$3) }
action: DEFINE TYPE genIdList ID EGAL typeDefProtected { DEFINETYPEACTION ($4,$3,$6) }
action: DEFINE ID patterns EGAL exprProtected { synDebug "action-9"; DEFINEACTION (newId(),$2,(createLambdaExpr $3 $5)) }
action: DEFINE EXTERNAL ID DEUXPOINTS typeDefProtected {DEFINEEXTERNALACTION (newId(),$3,$5) }
action: exprProtected { synDebug "action-3"; EXPRACTION $1 }
action: USE ID { USEACTION (newId(),$2) }

actionListPtVirg: action PTVIRG actionListPtVirg { synDebug "actionListPtVirg-1"; $1::$3 }
actionListPtVirg: action { synDebug "actionListPtVirg-2"; [$1]}
actionListPtVirg: action PTVIRG { synDebug "actionListPtVirg-3"; [$1] }

expr: expr INF expr { synDebug "expr-1"; createIdCallExpr "_<" [$1; $3] }
expr: expr SUP expr {  synDebug "expr-2"; createIdCallExpr "_>" [$1; $3] }
expr: expr EGALEGAL expr { synDebug "expr-3"; createIdCallExpr "_==" [$1; $3] }
expr: expr NOTEGAL expr { synDebug "expr-4"; createIdCallExpr "_!=" [$1; $3] }
expr: expr INFEGAL expr { synDebug "expr-5"; createIdCallExpr "_<=" [$1; $3] }
expr: expr SUPEGAL expr { synDebug "expr-6"; createIdCallExpr "_>=" [$1; $3] }
expr: expr INFPT expr { synDebug "expr-1"; createIdCallExpr "_<." [$1; $3] }
expr: expr SUPPT expr {  synDebug "expr-2"; createIdCallExpr "_>." [$1; $3] }
expr: expr EGALEGALPT expr { synDebug "expr-3"; createIdCallExpr "_==." [$1; $3] }
expr: expr NOTEGALPT expr { synDebug "expr-4"; createIdCallExpr "_!=." [$1; $3] }
expr: expr INFEGALPT expr { synDebug "expr-5"; createIdCallExpr "_<=." [$1; $3] }
expr: expr SUPEGALPT expr { synDebug "expr-6"; createIdCallExpr "_>=." [$1; $3] }
expr: expr PLUS expr { synDebug "expr-7"; createIdCallExpr "_+" [$1; $3] }
expr: expr MINUS expr { synDebug "expr-8"; createIdCallExpr "_-" [$1; $3] }
expr: expr PLUSPT expr { synDebug "expr-7"; createIdCallExpr "_+." [$1; $3] }
expr: expr MINUSPT expr { synDebug "expr-8"; createIdCallExpr "_-." [$1; $3] }
expr: expr MUL expr { synDebug "expr-9"; createIdCallExpr "_*" [$1; $3] }
expr: expr MOD expr { synDebug "expr-10"; createIdCallExpr "_mod" [$1; $3] }
expr: expr DIV expr { synDebug "expr-11"; createIdCallExpr "_/" [$1; $3] }
expr: expr MULPT expr { synDebug "expr-9"; createIdCallExpr "_*." [$1; $3] }
expr: expr DIVPT expr { synDebug "expr-11"; createIdCallExpr "_/." [$1; $3] }
expr: expr PUISS expr { synDebug "expr-12"; createIdCallExpr "_^" [$1; $3] }
expr: expr LOGICALOR expr { synDebug "expr-13"; createIdCallExpr "_||" [$1; $3] }
expr: expr LOGICALAND expr { synDebug "expr-14"; createIdCallExpr "_&&" [$1; $3] }
expr: expr DEUXDEUXPOINTS expr { synDebug "expr-15"; createIdCallExpr "_::" [$1; $3] }
expr: NOT expr { synDebug "expr-16"; createIdCallExpr "_not" [$2]  }
expr: LAMBDA matchExprs { synDebug "expr-17"; FUNCTIONEXPR (newId(),$2) }
expr: LET assigns IN exprProtected { synDebug "expr-18"; LETEXPR (newId(),$2,$4) }
expr: MATCH expr WITH matchExprs { synDebug "expr-19"; createFunctionCallExpr (FUNCTIONEXPR (newId(),$4)) [$2] }
expr: MATCH IN LIST expr WITH matchExprs { synDebug "expr-20"; createIdCallExpr "_matchInList" [(FUNCTIONEXPR (newId(),$6)); $4] }
expr: MATCH IN ARRAY expr WITH matchExprs { synDebug "expr-27"; createIdCallExpr "_matchInArray" [(FUNCTIONEXPR (newId(),$6)); $4] }
expr: exprProtected PTDEUXPOINTS typeDefProtected { synDebug "expr-21"; TYPEACCESSEXPR (newId(),$1,$3) }
expr: IF expr THEN expr ELSE exprProtected { synDebug "expr-22"; createIdCallExpr "_if" [$2; $4; $6] }
expr: expr PT CROO expr CROF { synDebug "expr-23"; createIdCallExpr "_get" [$1; $4] }
expr: exprProtected { synDebug "expr-24"; $1 }
expr: CAPID exprProtected { VARIANTEXPR (newId(),$1,$2) }

exprProtected: PARO expr DEUXPOINTS typeDef PARF { synDebug "exprProtected-1"; TYPEVERIFICATIONEXPR (newId(),$2,$4) }
exprProtected: PARO expr DEUXPOINTSSUP typeDef PARF { synDebug "exprProtected-2"; NARROWTYPEEXPR (newId(),$2,$4)  }
exprProtected: PARO expr DEUXPOINTSINF typeDef PARF { synDebug "exprProtected-3"; GENERALISETYPEEXPR (newId(),$2,$4)  }
exprProtected: PARO expr exprProtectedList PARF { synDebug "exprProtected-4"; createFunctionCallExpr $2 $3 }
exprProtected: INTVALUE { synDebug "exprProtected-5"; INTEXPR (newId(),$1) }
exprProtected: FLOATVALUE { synDebug "exprProtected-6"; FLOATEXPR (newId(),$1) }
exprProtected: STRINGVALUE { synDebug "exprProtected-7"; STRINGEXPR (newId(),$1) }
exprProtected: BOOLVALUE { synDebug "exprProtected-8"; BOOLEXPR (newId(),$1) }
exprProtected: ID { synDebug "exprProtected-9"; IDEXPR (newId(),$1) }
exprProtected: ACOO actionListPtVirg ACOF { synDebug "exprProtected-10"; ACTIONEXPR (newId(),$2) }
exprProtected: CROO exprListPtVirg CROF { synDebug "exprProtected-11"; LISTEXPR (newId(),$2) } 
exprProtected: CROO CROF { synDebug "exprProtected-12"; LISTEXPR (newId(),[]) }
exprProtected: CROO expr PTPT expr CROF { synDebug "exprProtected-13"; createIdCallExpr "_interval" [$2; $4; INTEXPR (newId(),1)] }
exprProtected: CROO expr PTVIRG expr PTPT expr CROF { synDebug "exprProtected-14"; createIdCallExpr "_interval" [$2; $4; $6] }
exprProtected: NOD { synDebug "exprProtected-15"; NODEXPR (newId()) }
exprProtected: PARO exprVirgList PARF { synDebug "exprProtected-16"; PAIREXPR (newId(),$2) }
exprProtected: CROO exprProtected PIPE pattern IN expr CROF { synDebug "exprProtected-17"; createIdCallExpr "_matchInList" [(FUNCTIONEXPR (newId(),[(createLambdaExpr [$4] $2)]));$6] }
exprProtected: CROOPIPE exprListPtVirg PIPECROF { synDebug "exprProtected-18"; ARRAYEXPR (newId(),$2) }
exprProtected: ERROR { synDebug "exprProtected-19"; ERROREXPR (newId()) }

exprVirgList: expr VIRG exprVirgList { $1::$3 }
exprVirgList: expr { [ $1 ] }

typeDef: variant variants { VARIANTTYPE (newId(),($1::$2)) }
typeDef: typeDefProtected FLECHD typeDef { FUNCTIONTYPE (newId(),$1,$3) }
typeDef: typeDefProtected { $1 }

typeDefProtected: NOD { NODTYPE (newId()) }
typeDefProtected: INT { INTTYPE (newId()) }
typeDefProtected: FLOAT { FLOATTYPE (newId()) }
typeDefProtected: STRING { STRINGTYPE (newId()) }
typeDefProtected: CROOPIPE typeDef PIPECROF { ARRAYTYPE (newId(),$2) }
typeDefProtected: CROO typeDef CROF { LISTTYPE (newId(),$2) }
typeDefProtected: GENID { GENTYPE (newId(),$1) }
typeDefProtected: PARO typeDefVirgList PARF  { PAIRTYPE (newId(),$2) }
typeDefProtected: ID { NAMEDTYPE (newId(),$1,[]) }
typeDefProtected: PARO ID typeDefList PARF { NAMEDTYPE (newId(),$2,$3) }
typeDefProtected: PARO typeDef PARF { $2 }

typeDefList: typeDefProtected typeDefList { $1::$2 }
typeDefList: typeDefProtected  { [$1] }

variant: CAPID typeDefProtected { ($1,$2) }

variants: PIPE variant variants { $2::$3 }
variants: { [] } 

typeDefVirgList: typeDef VIRG typeDefVirgList { $1::$3 }
typeDefVirgList: typeDef VIRG typeDef { [$1; $3] } 

lambdaExpr: patterns FLECHD exprProtected { createLambdaExpr $1 $3 }

matchExprs: lambdaExpr PIPE matchExprs { $1::$3 }
matchExprs: lambdaExpr { [ $1 ] }

assigns: pattern patterns EGAL expr AND assigns { ($1,(createLambdaExpr $2 $4))::$6 }
assigns: pattern patterns EGAL expr { [($1,(createLambdaExpr $2 $4))] }

exprListPtVirg: expr PTVIRG exprListPtVirg { $1::$3 }
exprListPtVirg: expr { [$1] } 

patterns: patternProtected patterns { $1::$2 }
patterns: { [] }

patternListVirg: patternProtected VIRG patternListVirg { $1::$3 }
patternListVirg: patternProtected { [$1] }

pattern: pattern DEUXDEUXPOINTS pattern { CONSPATTERN (newId(),$1,$3) }
pattern: patternProtected { $1 }
pattern: CAPID patternProtected { VARIANTPATTERN (newId(),$1,$2) }

patternProtected: INTVALUE { INTPATTERN (newId(),$1) }
patternProtected: FLOATVALUE { FLOATPATTERN (newId(),$1) }
patternProtected: STRINGVALUE { STRINGPATTERN (newId(),$1) }
patternProtected: BOOLVALUE { BOOLPATTERN (newId(),$1) }
patternProtected: CROO patternListPtVirg CROF { LISTPATTERN (newId(),$2) }
patternProtected: CROO CROF { LISTPATTERN (newId(),[]) }
patternProtected: patternProtected AS ID { RENAMINGPATTERN (newId(),$1,$3) }
patternProtected: SOULIGNE { WILDCARDPATTERN (newId()) }
patternProtected: PARO patternListVirg PARF { PAIRPATTERN (newId(),$2) }
patternProtected: ID { IDPATTERN (newId(),$1) }
patternProtected: patternProtected WHERE exprProtected { WHEREPATTERN (newId(),$1,$3) }
patternProtected: CROOPIPE patternListPtVirg PIPECROF { ARRAYPATTERN (newId(),$2) }
patternProtected: PARO pattern DEUXPOINTS typeDef PARF { TYPEDPATTERN (newId(),$2,$4) }

exprProtectedList: exprProtected exprProtectedList { $1::$2 }
exprProtectedList: exprProtected { [ $1 ] }

patternListPtVirg: pattern PTVIRG patternListPtVirg { $1::$3 }
patternListPtVirg: pattern { [ $1 ] }
 
genIdList: GENID genIdList { $1::$2 }
genIdList: { [] }
