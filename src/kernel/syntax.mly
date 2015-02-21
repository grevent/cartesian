
%{
%}

%token PLUS MINUS MUL MOD DIV PUISS CROO CROF PIPE ACOO ACOF LAMBDA FLECHD FLECHG LET IN DEUXDEUXPOINTS
%token DEFINE EGAL DEUXPOINTS PT PARO PARF MATCH WITH AND WHILE DO FOR NOT LOGICALOR
%token LOGICALAND PTVIRG VIRG SOULIGNE AS CROOPIPE PIPECROF NOD RAISE TRY CONTEXT PTPTPT EOL IF THEN ELSE
%token INF SUP EGALEGAL NOTEGAL INFEGAL SUPEGAL 
%token <string> ID
%token <int> INTVALUE
%token <float> FLOATVALUE
%token <char> CHARVALUE
%token <string> STRINGVALUE COMMENT
%token <bool> BOOLVALUE

%left PIPE
%left WITH
%left SUPEGAL INFEGAL INF SUP EGALEGAL NOTEGAL 
%left PLUS MINUS LOGICALOR
%left MUL MOD DIV LOGICALAND
%left DEUXDEUXPOINTS
%left PUISS
%left PT
%left NOT

%type <AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject> phrase 
%type <string -> AbstractPrototypeObject.abstractPrototypeObject> prototypeExpression

%start phrase

%%

phrase: actionListPtVirg EOL { new ActionsObject.actionsObject $1 }

comment: COMMENT { $1 }
comment: { "" }

action: ID FLECHG expr { new AssignActionObject.assignActionObject $1 $3 }
action: WHILE expr DO action { new WhileActionObject.whileActionObject $2 $4 }
action: FOR ID IN expr DO action { new ForActionObject.forActionObject $2 $4 $6 }
action: DO action WHILE expr { new DoActionObject.doActionObject $2 $4 }
action: RAISE expr { new RaiseActionObject.raiseActionObject $2 }
action: expr { new ExprActionObject.exprActionObject $1 }
action: TRY actionListPtVirg WITH matchExprs { new TryActionObject.tryActionObject $2 $4 }
action: DEFINE ID patterns EGAL expr { new DefineActionObject.defineActionObject $2 $3 $5 }
action: CONTEXT exprProtected expr { new ContextActionObject.contextActionObject $2 $3 }

actionListPtVirg: action PTVIRG comment actionListPtVirg { $1#attachComment $3; $1::$4 }
actionListPtVirg: action comment { begin $1#attachComment $2; [ $1 ] end }
actionListPtVirg: action PTVIRG comment { begin $1#attachComment $3; [ $1 ] end }

expr: expr INF expr { new FunctionCallExpressionObject.functionCallExpressionObject (new IdExpressionObject.idExpressionObject "_<") [$1; $3] }
expr: expr SUP expr { new FunctionCallExpressionObject.functionCallExpressionObject (new IdExpressionObject.idExpressionObject "_>") [$1; $3] }
expr: expr EGALEGAL expr { new FunctionCallExpressionObject.functionCallExpressionObject (new IdExpressionObject.idExpressionObject "_==") [$1; $3] }
expr: expr NOTEGAL expr { new FunctionCallExpressionObject.functionCallExpressionObject (new IdExpressionObject.idExpressionObject "_!=") [$1; $3] }
expr: expr INFEGAL expr { new FunctionCallExpressionObject.functionCallExpressionObject (new IdExpressionObject.idExpressionObject "_<=") [$1; $3] }
expr: expr SUPEGAL expr { new FunctionCallExpressionObject.functionCallExpressionObject (new IdExpressionObject.idExpressionObject "_>=") [$1; $3] }
expr: expr PLUS expr { new FunctionCallExpressionObject.functionCallExpressionObject (new IdExpressionObject.idExpressionObject "_+") [$1; $3] }
expr: expr MINUS expr { new FunctionCallExpressionObject.functionCallExpressionObject (new IdExpressionObject.idExpressionObject "_-") [$1; $3] }
expr: expr MUL expr { new FunctionCallExpressionObject.functionCallExpressionObject (new IdExpressionObject.idExpressionObject "_*") [$1; $3] }
expr: expr MOD expr { new FunctionCallExpressionObject.functionCallExpressionObject (new IdExpressionObject.idExpressionObject "_*") [$1; $3] }
expr: expr DIV expr { new FunctionCallExpressionObject.functionCallExpressionObject (new IdExpressionObject.idExpressionObject "_/") [$1; $3] }
expr: expr PUISS expr { new FunctionCallExpressionObject.functionCallExpressionObject (new IdExpressionObject.idExpressionObject "_^") [$1; $3] }
expr: expr LOGICALOR expr { new FunctionCallExpressionObject.functionCallExpressionObject (new IdExpressionObject.idExpressionObject "_||") [$1; $3] }
expr: expr LOGICALAND expr { new FunctionCallExpressionObject.functionCallExpressionObject (new IdExpressionObject.idExpressionObject "_&&") [$1; $3] }
expr: expr DEUXDEUXPOINTS expr { new FunctionCallExpressionObject.functionCallExpressionObject (new IdExpressionObject.idExpressionObject "_::") [$1; $3] }
expr: NOT expr { new FunctionCallExpressionObject.functionCallExpressionObject (new IdExpressionObject.idExpressionObject "_not") [$2] }
expr: LAMBDA matchExprs { new FunctionExpressionObject.functionExpressionObject $2 }
expr: LET assigns IN exprProtected { new LetExpressionObject.letExpressionObject $2 $4 }
expr: MATCH expr WITH matchExprs { new MatchExpressionObject.matchExpressionObject $2 $4 }
expr: expr PT ID { new AttributeAccessExpressionObject.attributeAccessExpressionObject $1 $3 }
expr: IF expr THEN expr ELSE exprProtected { new FunctionCallExpressionObject.functionCallExpressionObject (new IdExpressionObject.idExpressionObject "_if") [$2; $4; $6] }
expr: expr PT CROO expr CROF { new FunctionCallExpressionObject.functionCallExpressionObject (new IdExpressionObject.idExpressionObject "_get") [$1; $4] }
expr: exprProtected { $1 }

exprProtected: PARO expr exprProtectedList PARF { new FunctionCallExpressionObject.functionCallExpressionObject $2 $3 }
exprProtected: PARO prototypeDefinition PARF { new InstancesExpressionObject.instancesExpressionObject $2 }
exprProtected: PARO expr prototypeDefinition PARF { new PrototypesExpressionObject.prototypesExpressionObject $2 $3 }
exprProtected: INTVALUE { new IntExpressionObject.intExpressionObject $1 }
exprProtected: FLOATVALUE { new FloatExpressionObject.floatExpressionObject $1 }
exprProtected: CHARVALUE { new CharExpressionObject.charExpressionObject $1 }
exprProtected: STRINGVALUE { new StringExpressionObject.stringExpressionObject $1 }
exprProtected: BOOLVALUE { new BoolExpressionObject.boolExpressionObject $1 }
exprProtected: ID { new IdExpressionObject.idExpressionObject $1 }
exprProtected: ACOO actionListPtVirg ACOF { new ActionExpressionObject.actionExpressionObject $2 }
exprProtected: ACOO objectDefinitions ACOF { new ObjectExpressionObject.objectExpressionObject $2 }
exprProtected: CROOPIPE exprListPtVirg PIPECROF { new ArrayExpressionObject.arrayExpressionObject $2 }
exprProtected: CROO exprListPtVirg CROF { new ListExpressionObject.listExpressionObject $2 }
exprProtected: CROO CROF { new ListWrapperExpressionObject.listWrapperExpressionObject [] }
exprProtected: NOD { new NodExpressionObject.nodExpressionObject } 
exprProtected: PARO expr PARF { $2 }

prototypeDefinition: DEUXPOINTS ID prototypeExpression prototypeDefinition { ($3 $2)::$4 }
prototypeDefinition: DEUXPOINTS ID prototypeExpression { [$3 $2] }

lambdaExpr: patterns FLECHD exprProtected { ($1,$3) }

matchExprs: lambdaExpr PIPE matchExprs { $1::$3 }
matchExprs: lambdaExpr { [ $1 ] }

assigns: pattern EGAL expr AND assigns { ($1,$3)::$5 }
assigns: pattern EGAL expr { [($1,$3)] }

prototypeExpression: INTVALUE { fun uc -> (new IntPrototypeObject.intPrototypeObject uc $1) }
prototypeExpression: FLOATVALUE { fun uc -> (new FloatPrototypeObject.floatPrototypeObject uc $1) }
prototypeExpression: CHARVALUE { fun uc -> (new CharPrototypeObject.charPrototypeObject uc $1) }
prototypeExpression: STRINGVALUE { fun uc -> (new StringPrototypeObject.stringPrototypeObject uc $1) }
prototypeExpression: BOOLVALUE { fun uc -> (new BoolPrototypeObject.boolPrototypeObject uc $1) }
prototypeExpression: CROO prototypeExpressionListPtVirg CROF { fun uc -> (new ListPrototypeObject.listPrototypeObject uc (List.map (fun x -> (x uc)) $2)) }
prototypeExpression: CROO CROF { fun uc -> (new ListPrototypeObject.listPrototypeObject uc []) }
prototypeExpression: CROOPIPE prototypeExpressionListPtVirg PIPECROF { fun uc -> (new ArrayPrototypeObject.arrayPrototypeObject uc (Array.of_list (List.map (fun x -> (x uc)) $2))) }
prototypeExpression: ACOO prototypeObjectDefinitions ACOF { (fun uc -> (new ObjectPrototypeObject.objectPrototypeObject uc (List.map (fun x -> (x uc)) $2))) }

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

pattern: pattern DEUXDEUXPOINTS pattern { new ConsPatternObject.consPatternObject $1 $3 } 
pattern: patternProtected { $1 }

patternProtected: INTVALUE { new IntPatternObject.intPatternObject $1 }
patternProtected: FLOATVALUE { new FloatPatternObject.floatPatternObject $1 }
patternProtected: CHARVALUE { new CharPatternObject.charPatternObject $1 }
patternProtected: STRINGVALUE { new StringPatternObject.stringPatternObject $1 }
patternProtected: BOOLVALUE { new BoolPatternObject.boolPatternObject $1 }
patternProtected: CROO patternListPtVirg CROF { new ListPatternObject.listPatternObject $2 }
patternProtected: CROO CROF { new ListPatternObject.listPatternObject [] }
patternProtected: CROOPIPE patternListPtVirg PIPECROF {  new ArrayPatternObject.arrayPatternObject $2 }
patternProtected: patternProtected AS ID { new RenamingPatternObject.renamingPatternObject $1 $3 }
patternProtected: SOULIGNE { new WildcardPatternObject.wildcardPatternObject }
patternProtected: PARO pattern PARF { $2 }
patternProtected: ID { new IdPatternObject.idPatternObject $1 }

exprProtectedList: exprProtected exprProtectedList { $1::$2 }
exprProtectedList: exprProtected { [ $1 ] }

patternListPtVirg: pattern PTVIRG patternListPtVirg { $1::$3 }
patternListPtVirg: pattern { [ $1 ] }

