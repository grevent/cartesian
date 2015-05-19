
open CartesianTree

exception NotAnAction
exception NotAnExpr
exception NotAPattern
exception NotAFunction
exception NotAPrototype
exception NativeFunction
exception NativeAction
  
let rec tree2Object dispatcher tree =
  tree2Action tree

and tree2Action dispatcher tree =
  match tree with
    ACTIONS actions -> new ActionsObject.actionsObject (List.map tree2Action actions)
  | WHILEACTION (expr,action) -> new WhileActionObject.whileActionObject (tree2Expr expr) (tree2Action action)
  | TRYACTION (actions,matchs) -> new TryActionObject.tryActionObject (List.map tree2Action actions) (List.map (fun (patterns,expr) -> ((List.map tree2Pattern patterns),(tree2Expr expr))) matchs)
  | STARTACTION (blocking,signals) -> new StartActionObject.startActionObject dispatcher blocking (List.map (tree2Action dispatcher) signals)
  | RAISEACTION expr -> new RaiseActionObject.raiseActionObject (tree2Expr expr)
  | NATIVEACTION st -> raise NativeAction
  | ASSIGNACTION (id,expr) -> new AssignActionObject.assignActionObject id (tree2Expr expr)
  | CONTEXTACTION (context,expr) -> new ContextActionObject.contextActionObject (tree2Expr context) (tree2Expr expr)
  | DEFINEACTION (id,patterns,expr) ->
     new DefineActionObject.defineActionObject id (List.map tree2Pattern patterns) (tree2Expr expr)
  | DOACTION (action,expr) -> new DoActionObject.doActionObject (tree2Action action) (tree2Expr expr)
  | EXPRACTION expr -> new ExprActionObject.exprActionObject (tree2Expr expr)
  | FORACTION (pattern,value,action) -> new ForActionObject.forActionObject (tree2Pattern pattern) (tree2Expr value) (tree2Action action)
  | COMMENT (comment,tree) ->
     let tmp = (tree2Action tree) in
     begin
       (tmp#attachComment comment);
       comment
     end			
  | _ -> raise NotAnAction

and tree2Expr tree =
  match tree with
    ACTIONEXPRESSION actions -> new ActionExpressionObject.actionExpressionObject (List.map tree2Expr actions)
  | ACTIONWRAPPER action -> new ActionWrapperExpr.actionWrapperExpr (tree2Action action)
  | QUOTEDIDEXPRESSION id -> new QuotedIdExpressionObject.quotedIdExpressionObject id
  | OBJECTWRAPPEREXPRESSION expr -> new ObjectWrapperExpressionObject.objectWrapperExpressionObject (tree2Object expr)
  | PROTOTYPESEXPRESSION (expr,useCases) -> new PrototypesExpressionObject.prototypesExpressionObject (tree2Expr expr) (List.map tree2Prototype useCases)
  | OBJECTEXPRESSION defs ->
     new ObjectExpressionObject.objectExpressionObject (List.map (fun (id,patterns,expr) -> (id,(List.map tree2Pattern patterns),(tree2Expr expr))) defs)
  | BOOLEXPRESSION bl -> new BoolExpressionObject.boolExpressionObject bl
  | FLOATEXPRESSION fl -> new FloatExpressionObject.floatExpressionObject fl
  | INTEXPRESSION i -> new IntExpressionObject.intExpressionObject i
  | LISTEXPRESSION lst -> new ListExpressionObject.listExpressionObject (List.map tree2Expr lst)
  | NODEXPRESSION -> new NodExpressionObject.nodExpressionObject
  | STRINGEXPRESSION s -> new StringExpressionObject.stringExpressionObject s
  | NATIVEFUNCTION im -> new NativeFunctionObject.nativeFunctionObject (tree2Function im)
  | CHAREXPRESSION c -> new CharExpressionObject.charExpressionObject c
  | ARRAYEXPRESSION lst -> new ArrayExpressionObject.arrayExpressionObject (List.map tree2Expr lst)
  | IDEXPRESSION st -> new IdExpressionObject.idExpressionObject st
  | ATTRIBUTEACCESSEXPRESSION (expr,id) -> new AttributeAccessExpressionObject.attributeAccessExpressionObject (tree2Expr expr) id
  | FUNCTIONEXPRESSION lambdas ->
     new FunctionExpressionObject.functionExpressionObject
	 (List.map (fun (patterns,expr) -> (List.map tree2Pattern patterns) (tree2Expr expr))
		   lambdas)
  | FUNCTIONCALLEXPRESSION (fn,params) ->
     new FunctionCallExpressionObject.functionCallExpressionObject (tree2Expr fn) (List.map tree2Expr params)
  | INSTANCESEXPRESSION lst -> new InstancesExpressionObject.instancesExpressionObject (List.map tree2Prototype lst)
  | LETEXPRESSION (defs,expr) ->
      new LetExpressionObject.letExpressionObject (List.map (fun (patterns,expr) -> ((List.map tree2Pattern patterns),(tree2Expr expr))) defs) expr
  | MATCHEXPRESSION (expr,matchs) -> new MatchExpressionObject.matchExpressionObject (tree2Expr expr) (List.map (fun (patterns,expr) -> ((List.map tree2Pattern patterns),(tree2Expr expr))) matchs)
  | COMMENT (comment,tree) ->
     let tmp = (tree2Expr tree) in
     begin
       (tmp#attachComment comment);
       comment
     end			
  | _ -> raise NotAnExpr

and tree2Pattern tree =
  match tree with
    WILDCARDPATTERN -> new WildcardPatternObject.wildcardPatternObject
  | STRINGPATTERN st -> new StringPatternObject.stringPatternObject st
  | RENAMINGPATTERN (pattern,id) -> new RenamingPatternObject.renamingPatternObject (tree2Pattern pattern) id
  | ARRAYPATTERN lst -> new ArrayPatternObject.arrayPatternObject (List.map tree2Pattern lst)
  | BOOLPATTERN bl -> new BoolPatternObject.boolPatternObject bl
  | CHARPATTERN ch -> new CharPatternObject.charPatternObject ch
  | CONSPATTERN (car,cdr) -> new ConsPatternObject.consPatternObject (tree2Pattern car) (tree2Pattern cdr)
  | FLOATPATTERN fl -> new FloatPatternObject.floatPatternObject fl
  | INTPATTERN i -> new IntPatternObject.intPatternObject i
  | LISTPATTERN lst -> new ListPatternObject.listPatternObject (List.map tree2Expr lst)
  | IDPATTERN id -> new IdPatternObject.idPatternObject id
  | COMMENT (comment,tree) ->
     let tmp = (tree2Pattern tree) in
     begin
       (tmp#attachComment comment);
       comment
     end			
  | _ -> raise NotAPattern

and tree2Function tree =
  match tree with
    NATIVEFUNCTION1 st -> raise NativeFunction
  | NATIVEFUNCTION2 st -> raise NativeFunction
  | FUNCTION lambdas ->new FunctionObject.functionObject (List.map (fun (params,expr) -> (List.map tree2Pattern params) (tree2Expr expr)) lambdas)
  | ADDNATIVE -> new AddNativeObject.addNativeObject
  | IFNATIVE -> new IfNativeObject.ifNativeObject
  | IFNATIVEPARTIAL2 (obj1,obj2) -> new IfNativeObject.ifNativePartial2Object (tree2Expr obj1) (tree2Expr obj2)
  | IFNATIVEPARTIAL1 obj -> new IfNativeObject.ifNativePartial1Object (tree2Expr obj)
  | COMMENT (comment,tree) ->
     let tmp = (tree2Function tree) in
     begin
       (tmp#attachComment comment);
       comment
     end
  | _ -> raise NotAFunction

and tree2Prototype tree =
  match tree with
    STRINGPROTOTYPE (uc,st) -> new StringPrototypeObject.stringPrototypeObject uc st
  | OBJECTPROTOTYPE (uc,objs) -> new ObjectPrototypeObject.objectPrototypeObject uc (List.map (fun (id,expr) -> (id,(tree2Expr expr))) objs)
  | BOOLPROTOTYPE (uc,bl) -> new BoolProtypeObject.boolPrototypeObject uc bl
  | ARRAYPROTOTYPE (uc,lst) ->
     let ar = Array.of_list (List.map tree2Prototype lst) in
     new ArrayPrototypeObject.arrayPrototypeObject uc ar
  | CHARPROTOTYPE (uc,ch) -> new CharPrototypeObject.charPrototypeObject uc ch
  | FLOATPROTOTYPE (uc,fl) -> new FloatPrototypeObject.floatPrototypeObject uc fl
  | INTPROTOTYPE (uc,i) -> new IntPrototypeObject.intPrototypeObject uc i
  | LISTPROTOTYPE (uc,lst) -> new ListPrototypeObject.listPrototypeObject uc (List.map tree2Prototype lst)
  | COMMENT (comment,tree) ->
     let tmp = (tree2Expr tree) in
     begin
       (tmp#attachComment comment);
       comment
     end
  | _ -> raise NotAPrototype

and tree2Object tree =
  match tree with
    OBJECT attributes ->
    let obj = new ObjectObject.objectObject in
    List.iter (fun (id,expr) -> obj#addAttribute id (tree2Expr expr));
    obj
  | _ -> raise NotAnObject
;;
