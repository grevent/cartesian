

%{
  open CartesianTree
%}

%token EOL USE CASE ID ACOO ACOF PTVIRG SEND FLECHD INCLUDE FLECHG
%token DO EACH RECEIVE ACTOR DEUXPOINTS TRANSACTION LAMBDA LET IN  MATCH IF
%token THEN ELSE CROO CROF RULE IMPLY PARO  PARF INT CROOPIPE  PIPECROF
%token QUOTE PTPT NOD  PIPEPIPE EXTENDED BY ALL POSSIBLE EGAL AND VIRG AS
%token SOULIGNE 
%token PLUS MINUS MUL DIV PUISS INF SUP EGALEGAL NOTEGAL INFEGAL SUPEGAL PIPE
%token DEUXDEUXPOINTS PT PTPT WITH MOD NOT LOGICALOR LOGICALAND 

%token INT FLOAT COMPLEX STRING OBJECT ARRAY

%token <string> ID COMMENT STRINGVALUE
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
      
%type <CartesianTree.cartesianTree> phrase
      
%start phrase
       
%%
 
phrase: expr EOL { $1 }

statesAndDefs: state PTVIRG statesAndDefs { $1::$3 }
statesAndDefs: state { [$1] }
statesAndDefs: actionDef PTVIRG statesAndDefs { $1::$3 }
statesAndDefs: actionDef { [$1] }

state: ID SEND ID patterns FLECHD exprProtected { ACTORSENDSSTATE ($1,$3,$4,$6) }
state: expr FLECHD exprProtected { BOOLSTATE ($1,$3) }
state: INCLUDE ID { INCLUDESTATE $2 }
  
comment: COMMENT { $1 }
comment: { "" }

action: actionDef { $1 }
action: ID FLECHG expr exprProtectedList { ASSIGNACTION ($1,FUNCTIONCALLEXPRESSION ($3,$4)) }
action: ID FLECHG expr { ASSIGNACTION ($1,$3) }
action: DO expr { DOACTION $2 (* Do each action in the list / object delivered by the evaluation *) }
action: expr { EXPRACTION $1 }
action: ID RECEIVE expr { ACTORRECEIVESACTION ($1,$3) }
action: ACTOR ID DEUXPOINTS ID expr { DEFINEACTORACTION ($2,$4,$5) (* Definition of actor 2 with reference 4 and parameter object 5 *) }
action: TRANSACTION expr { TRANSACTIONACTION ($2) (* Execute expr as action, with all attributes / objects accessed protected in write / read *) }

actionDef: ID patterns DEUXPOINTS expr { DEFINEACTION ($1,$2,$4) }

actionListPtVirg: action PTVIRG actionListPtVirg { $1::$3 }
actionListPtVirg: action { [$1]}
actionListPtVirg: action PTVIRG { [$1] }

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
expr: MATCH depthModification expr extensions WITH matchExprs { MATCHEXPRESSION ($2,$3,$4,$6) }
expr: expr PT ID { ATTRIBUTEACCESSEXPRESSION ($1,$3) }
expr: IF expr THEN expr ELSE exprProtected { FUNCTIONCALLEXPRESSION ((IDEXPRESSION "_if"),[$2; $4; $6]) }
expr: expr PT CROO expr CROF { FUNCTIONCALLEXPRESSION ((IDEXPRESSION "_get"),[$1; $4]) }
expr: RULE patternsVirg IMPLY patternProtected { }
expr: expr	Protected { $1 }
expr: PARO expr IN set PARF { }

set: INT { }
set: FLOAT { }
set: COMPLEX { }
set: STRING { }
set: OBJECT { }
set: ARRAY { }

exprProtected: PARO expr exprProtectedList PARF { FUNCTIONCALLEXPRESSION ($2,$3) }
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
exprProtected: CROO expr PTPT expr CROF { }
exprProtected: CROO expr PTVIRG expr PTPT expr CROF { }
exprProtected: NOD { NODEXPRESSION }
exprProtected: PARO expr PARF { $2 }
exprProtected: CROO exprProtected PIPE pattern IN expr CROF { }
exprProtected: USE CASE ID ACOO statesAndDefs ACOF { USECASE ($3,$5) }

matrixPart: exprListPtVirg  PIPEPIPE matrixPart { (Array.of_list $1)::$3 }
matrixPart: exprListPtVirg { [ (Array.of_list $1) ] }

lambdaExpr: patterns FLECHD exprProtected { ($1,$3) }

matchExprs: lambdaExpr PIPE matchExprs { $1::$3 }
matchExprs: lambdaExpr { [ $1 ] }

extensions: EXTENDED BY expr { }
extensions: { }

depthModification: { ONEELEMENT (* Just matchs one element, not the elements in a list *) }
depthModification: ALL { ALLELEMENTS (* Matches in depth in the tree... Delivers a similar tree... If no match, then error... Through this more powerfull than a mapping... *) }
depthModification: EACH { EACHELEMENT (* Matches every element of a list / object. No in-depth matching... *) }
depthModification: ALL POSSIBLE { ALLPOSSIBELEMENTS (* The one not matching are just discarded... *) }
depthModification: EACH POSSIBLE { EACHPOSSIBLEELEMENTS (* The one not matching are just discarded...*) }

assigns: pattern patterns EGAL expr AND assigns { ($1,$2,$4)::$6 }
assigns: pattern patterns EGAL expr { [($1,$2,$4)] }

exprListPtVirg: expr PTVIRG exprListPtVirg { $1::$3 }
exprListPtVirg: expr { [$1] } 

objectDefinitions: ID patterns EGAL expr objectDefinitions { ($1,$2,$4)::$5 }
objectDefinitions: ID patterns EGAL expr { [ ($1,$2,$4) ] }

patterns: patternProtected patterns { $1::$2 }
patterns: { [] }

patternsVirg: patternProtected VIRG patternsVirg { }
patternsVirg: patternProtected { }

pattern: pattern DEUXDEUXPOINTS pattern { CONSPATTERN ($1,$3) }
pattern: patternProtected { $1 }

patternProtected: INTVALUE { NUMPATTERN ((float_of_int $1),0.0) }
patternProtected: FLOATVALUE { NUMPATTERN ($1,0.0) }
patternProtected: PARO FLOATVALUE PTVIRG FLOATVALUE PARF { NUMPATTERN ($2,$4) }
patternProtected: CROOPIPE matrixNumericPatternPart PIPECROF { MATRIXPATTERN (Array.of_list $2) }
patternProtected: STRINGVALUE { STRINGPATTERN $1 }
patternProtected: BOOLVALUE { BOOLPATTERN $1 }
patternProtected: CROO patternListPtVirg CROF { LISTPATTERN $2 }
patternProtected: CROO CROF { LISTPATTERN [] }
patternProtected: patternProtected AS ID { RENAMINGPATTERN ($1,$3) }
patternProtected: SOULIGNE { WILDCARDPATTERN }
patternProtected: PARO pattern PARF { $2 }
patternProtected: ID { IDPATTERN $1 }
patternProtected: ACOO patternObjectDefinitions ACOF { OBJECTPATTERN $2 }
patternProtected: patternProtected WHERE exprProtected { }

patternObjectDefinitions: patternObjectDefinition PTVIRG patternObjectDefinitions { $1::$3 }
patternObjectDefinitions: patternObjectDefinition PTVIRG { [$1] }
patternObjectDefinitions: patternObjectDefinition { [$1] }

patternObjectDefinition: ID EGAL pattern { ($1,$3) }

exprProtectedList: exprProtected exprProtectedList { $1::$2 }
exprProtectedList: exprProtected { [ $1 ] }

patternListPtVirg: pattern PTVIRG patternListPtVirg { $1::$3 }
patternListPtVirg: pattern { [ $1 ] }

matrixNumericPatternPart: patternListPtVirg PIPEPIPE matrixNumericPatternPart { (Array.of_list $1)::$3 }
matrixNumericPatternPart: patternListPtVirg { [(Array.of_list $1)] }
