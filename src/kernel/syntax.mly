
%{
  open CartesianTree
%}

%token PLUS MINUS MUL MOD DIV PUISS CROO CROF PIPE ACOO ACOF LAMBDA FLECHD FLECHG LET IN DEUXDEUXPOINTS
%token EGAL DEUXPOINTS PT PARO PARF MATCH WITH AND WHILE DO FOR NOT LOGICALOR
%token LOGICALAND PTVIRG VIRG SOULIGNE AS NOD RAISE TRY 
%token CONTEXT PTPTPT EOL IF THEN ELSE CROOPIPE PIPECROF PIPEPIPE
%token STARTSEND THREAD SEND RECEIVE WHERE THREAD
%token INF SUP EGALEGAL NOTEGAL INFEGAL SUPEGAL QUOTE
%token <string> ID
%token <int> INTVALUE
%token <float> FLOATVALUE
%token <string> STRINGVALUE COMMENT
%token <bool> BOOLVALUE

%left THREAD
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

%type <CartesianTree.cartesianTree> phrase
%type <string -> AbstractPrototypeObject.abstractPrototypeObject> prototypeExpression
									    

%start phrase

%%

phrase: expr EOL { $1 }

comment: COMMENT { $1 }
comment: { "" }

action: ID FLECHG expr exprProtectedList { ASSIGNACTION ($1,FUNCTIONCALLEXPRESSION ($3,$4)) }
action: ID FLECHG expr { ASSIGNACTION ($1,$3) }
action: WHILE expr DO action { WHILEACTION ($2,$4) }
action: FOR ID IN expr DO action { FORACTION ($2,$4,$6) }
action: DO action WHILE expr { DOACTION ($2,$4) }
action: RAISE expr { RAISEACTION $2 }
action: expr { EXPRACTION $1 }
action: TRY actionListPtVirg WITH matchExprs { TRYACTION ($2,$4) }
action: ID patterns DEUXPOINTS expr { DEFINEACTION ($1,$2,$4) }
action: CONTEXT exprProtected expr { CONTEXTACTION ($2,$3) }
action: ID STARTSEND action {
  ACTORSTARTSACTION ($1,$3) }
action: ID SEND pattern FLECHD exprProtected otherSignals {
  let (blocking,lst) = $6 in
  ACTORSENDSACTION (blocking,($1,$3,$5)::lst) }
action: ID RECEIVE expr { ACTORRECEIVESACTION ($1,$3) }
action: THREAD action { THREADACTION $2 }
action: ACOO actionListPtVirg ACOF { SEQUENCEACTION $2 }

otherSignals: PIPE ID SEND pattern FLECHD exprProtected otherSignals { 
  let (blocking,lst) = $7 in
  (blocking,($2,$4,$6)::lst) }
otherSignals: PIPE PTPTPT { (false,[]) }
otherSignals: { (true,[]) }

actionListPtVirg: action PTVIRG comment actionListPtVirg { COMMENT ($3,$1)::$4 }
actionListPtVirg: action comment { [ COMMENT ($2,$1) ] }
actionListPtVirg: action PTVIRG comment { [ COMMENT ($3,$1) ] }

expr: expr INF expr { FUNCTIONCALLEXPRESSION ((IDEXPRESSION "_<"),[$1; $3]) }
expr: expr SUP expr { FUNCTIONCALLEXPRESSION ((IDEXPRESSION "_>"),[$1; $3]) }
expr: expr EGALEGAL expr { FUNCTIONCALLEXPRESSION ((IDEXPRESSION "_=="),[$1; $3]) }
expr: expr NOTEGAL expr { FUNCTIONCALLEXPRESSION ((IDEXPRESSION "_!="),[$1; $3]) }
expr: expr INFEGAL expr { FUNCTIONCALLEXPRESSION ((IDEXPRESSION "_<="),[$1; $3]) }
expr: expr SUPEGAL expr { FUNCTIONCALLEXPRESSION ((IDEXPRESSION "_>="),[$1; $3]) }
expr: expr PLUS expr { FUNCTIONCALLEXPRESSION ((IDEXPRESSION "_+"),[$1; $3]) }
expr: expr MINUS expr { FUNCTIONCALLEXPRESSION ((IDEXPRESSION "_-"),[$1; $3]) }
expr: expr MUL expr { FUNCTIONCALLEXPRESSION ((IDEXPRESSION "_*"),[$1; $3]) }
expr: expr MOD expr { FUNCTIONCALLEXPRESSION ((IDEXPRESSION "_*"),[$1; $3]) }
expr: expr DIV expr { FUNCTIONCALLEXPRESSION ((IDEXPRESSION "_/"),[$1; $3]) }
expr: expr PUISS expr { FUNCTIONCALLEXPRESSION ((IDEXPRESSION "_^"),[$1; $3]) }
expr: expr LOGICALOR expr { FUNCTIONCALLEXPRESSION ((IDEXPRESSION "_||"),[$1; $3]) }
expr: expr LOGICALAND expr { FUNCTIONCALLEXPRESSION ((IDEXPRESSION "_&&"),[$1; $3]) }
expr: expr DEUXDEUXPOINTS expr { FUNCTIONCALLEXPRESSION ((IDEXPRESSION "_::"),[$1; $3]) }
expr: NOT expr { FUNCTIONCALLEXPRESSION ((IDEXPRESSION "_not"),[$2]) }
expr: LAMBDA matchExprs { FUNCTIONEXPRESSION $2 }
expr: LET assigns IN exprProtected { LETEXPRESSION ($2,$4) }
expr: MATCH expr WITH matchExprs { MATCHEXPRESSION ($2,$4) }
expr: expr PT ID { ATTRIBUTEACCESSEXPRESSION ($1,$3) }
expr: IF expr THEN expr ELSE exprProtected { FUNCTIONCALLEXPRESSION ((IDEXPRESSION "_if"),[$2; $4; $6]) }
expr: expr PT CROO expr CROF { FUNCTIONCALLEXPRESSION ((IDEXPRESSION "_get"),[$1; $4]) }
expr: exprProtected { $1 }

exprProtected: PARO expr exprProtectedList PARF { FUNCTIONCALLEXPRESSION ($2,$3) }
exprProtected: PARO prototypeDefinition PARF { INSTANCESEXPRESSION $2 }
exprProtected: PARO expr prototypeDefinition PARF { PROTOTYPESEXPRESSION ($2,$3) }
exprProtected: INTVALUE { NUMEXPRESSION ((float_of_int $1),0.0) }
exprProtected: FLOATVALUE { NUMEXPRESSION ($1,0.0) }
exprProtected: PARO FLOATVALUE PTVIRG FLOATVALUE PARF { NUMEXPRESSION ($2,$4) }
exprProtected: CROOPIPE matrixPart PIPECROF { MATRIXEXPRESSION (Array.of_list $2) }
exprProtected: STRINGVALUE { STRINGEXPRESSION $1 }
exprProtected: BOOLVALUE { BOOLEXPRESSION $1 }
exprProtected: ID { IDEXPRESSION $1 }
exprProtected: QUOTE ID { QUOTEDIDEXPRESSION $2 }
exprProtected: ACOO actionListPtVirg ACOF { ACTIONEXPRESSION (SEQUENCEACTION $2) }
exprProtected: ACOO objectDefinitions ACOF { OBJECTEXPRESSION $2 }
exprProtected: CROO exprListPtVirg CROF { LISTEXPRESSION $2 } 
exprProtected: CROO CROF { LISTEXPRESSION [] }
exprProtected: NOD { NODEXPRESSION }
exprProtected: PARO expr PARF { $2 }

matrixPart: exprListPtVirg  PIPEPIPE matrixPart { (Array.of_list $1)::$3 }
matrixPart: exprListPtVirg { [ (Array.of_list $1) ] }

prototypeDefinition: DEUXPOINTS ID prototypeExpression prototypeDefinition { ($3 $2)::$4 }
prototypeDefinition: DEUXPOINTS ID prototypeExpression { [$3 $2] }

lambdaExpr: patterns FLECHD exprProtected { ($1,$3) }

matchExprs: lambdaExpr PIPE matchExprs { $1::$3 }
matchExprs: lambdaExpr { [ $1 ] }

assigns: pattern EGAL expr AND assigns { ($1,$3)::$5 }
assigns: pattern EGAL expr { [($1,$3)] }

prototypeExpression: INTVALUE { fun uc -> NUMPROTOTYPE (uc,(float_of_int $1),0.0) }
prototypeExpression: FLOATVALUE { fun uc -> NUMPROTOTYPE (uc,$1,0.0) }
prototypeExpression: PARO FLOATVALUE PTVIRG FLOATVALUE PARF { fun uc -> NUMPROTOTYPE (uc,$2,$4) }
prototypeExpression: STRINGVALUE { fun uc -> STRINGPROTOTYPE (uc,$1) }
prototypeExpression: BOOLVALUE { fun uc -> BOOLPROTOTYPE (uc,$1) }
prototypeExpression: CROO prototypeExpressionListPtVirg CROF { fun uc -> LISTPROTOTYPE (uc,$2) }
prototypeExpression: CROO CROF { fun uc -> LISTPROTOTYPE (uc,[]) }
prototypeExpression: ACOO prototypeObjectDefinitions ACOF { fun uc -> OBJECTPROTOTYPE (uc,$2) }
prototypeExpression: CROOPIPE matrixNumericPart PIPECROF { fun uc -> MATRIXPROTOTYPE (uc,(Array.of_list $2)) }

numListPtVirg: INTVALUE PTVIRG numListPtVirg { ((float_of_int $1),0.0)::$3 }
numListPtVirg: FLOATVALUE PTVIRG numListPtVirg { ($1,0.0)::$3 }
numListPtVirg: PARO FLOATVALUE PTVIRG FLOATVALUE PARF PTVIRG numListPtVirg { ($2,$4)::$7 }
numListPtVirg: INTVALUE  { [((float_of_int $1),0.0)] }
numListPtVirg: FLOATVALUE { [($1,0.0)] }
numListPtVirg: PARO FLOATVALUE PTVIRG FLOATVALUE PARF { [($2,$4)] }

matrixNumericPart: numListPtVirg PIPEPIPE matrixNumericPart { (Array.of_list $1)::$3 }
matrixNumericPart: numListPtVirg { [(Array.of_list $1)] }

prototypeExpressionListPtVirg: prototypeExpression PTVIRG prototypeExpressionListPtVirg { $1::$3 }
prototypeExpressionListPtVirg: prototypeExpression { [$1] }

prototypeObjectDefinitions: prototypeObjectDefinition PTVIRG prototypeObjectDefinitions { $1::$3 } 
prototypeObjectDefinitions: prototypeObjectDefinition PTVIRG { [ $1 ]}
prototypeObjectDefinitions: prototypeObjectDefinition { [ $1 ] }

prototypeObjectDefinition: ID EGAL prototypeExpression { (fun uc -> ($1,($3 uc))) }

exprListPtVirg: expr PTVIRG exprListPtVirg { $1::$3 }
exprListPtVirg: expr { [$1] } 

objectDefinitions: ID patterns EGAL expr objectDefinitions { ($1,$2,$4)::$5 }
objectDefinitions: ID patterns EGAL expr { [ ($1,$2,$4) ] }

patterns: patternProtected patterns { $1::$2 }
patterns: { [] }

pattern: pattern DEUXDEUXPOINTS pattern { CONSPATTERN ($1,$3) }
pattern: pattern WHERE expr { WHEREPATTERN ($1,$3) }
pattern: patternProtected { $1 }

patternProtected: INTVALUE { NUMPATTERN ((float_of_int $1),0.0) }
patternProtected: FLOATVALUE { NUMPATTERN ($1,0.0) }
patternProtected: PARO FLOATVALUE PTVIRG FLOATVALUE PARF { NUMPATTERN ($2,$4) }
patternProtected: CROOPIPE matrixNumericPart PIPECROF { MATRIXPATTERN (Array.of_list $2) }
patternProtected: STRINGVALUE { STRINGPATTERN $1 }
patternProtected: BOOLVALUE { BOOLPATTERN $1 }
patternProtected: CROO patternListPtVirg CROF { LISTPATTERN $2 }
patternProtected: CROO CROF { LISTPATTERN [] }
patternProtected: patternProtected AS ID { RENAMINGPATTERN ($1,$3) }
patternProtected: SOULIGNE { WILDCARDPATTERN }
patternProtected: PARO pattern PARF { $2 }
patternProtected: ID { IDPATTERN $1 }
patternProtected: ACOO patternObjectDefinitions ACOF { OBJECTPATTERN $2 }

patternObjectDefinitions: patternObjectDefinition PTVIRG patternObjectDefinitions { $1::$3 }
patternObjectDefinitions: patternObjectDefinition PTVIRG { [$1] }
patternObjectDefinitions: patternObjectDefinition { [$1] }

patternObjectDefinition: ID EGAL pattern { ($1,$3) }

exprProtectedList: exprProtected exprProtectedList { $1::$2 }
exprProtectedList: exprProtected { [ $1 ] }

patternListPtVirg: pattern PTVIRG patternListPtVirg { $1::$3 }
patternListPtVirg: pattern { [ $1 ] }

