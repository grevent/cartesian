
open CartesianTree

exception NotAnAction
exception NotAnExpr
exception NotAPattern
exception NotAFunction
exception NotAPrototype
exception NotAnObject
exception NativeFunction
exception NativeAction
  
let rec tree2Action dispatcher tree =
  match tree with
    WHILEACTION (expr,action) -> ((new WhileActionObject.whileActionObject
				      (tree2Expr dispatcher expr)
				      (tree2Action dispatcher action) ) :> AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject)
  | TRYACTION (actions,matchs) -> ((new TryActionObject.tryActionObject
				      (List.map (tree2Action dispatcher) actions)
				      (List.map (fun (patterns,expr) -> ((List.map (tree2Pattern dispatcher) patterns),(tree2Expr dispatcher expr))) matchs)
				   ) :> AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject)
  | REGISTERSTARTACTION (actorId,action) -> ((new StartActionObject.startActionObject
						  dispatcher
						  actorId
						  (tree2Action dispatcher action) )
					     :> AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject)
  | RAISEACTION expr -> ((new RaiseActionObject.raiseActionObject (tree2Expr dispatcher expr) ) :> AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject)
  | NATIVEACTION st -> raise NativeAction
  | ASSIGNACTION (id,expr) -> ((new AssignActionObject.assignActionObject id (tree2Expr dispatcher expr) ) :> AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject)
  | CONTEXTACTION (context,expr) -> ((new ContextActionObject.contextActionObject (tree2Expr dispatcher context) (tree2Expr dispatcher expr) ) :> AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject)
  | DEFINEACTION (id,patterns,expr) ->
     ((new DefineActionObject.defineActionObject id (List.map (tree2Pattern dispatcher) patterns) (tree2Expr dispatcher expr) ) :> AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject)
  | DOACTION (action,expr) -> ((new DoActionObject.doActionObject (tree2Action dispatcher action) (tree2Expr dispatcher expr) ) :> AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject)
  | EXPRACTION expr -> ((new ExprActionObject.exprActionObject (tree2Expr dispatcher expr) ) :> AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject)
  | FORACTION (id,value,action) -> ((new ForActionObject.forActionObject id
					 (tree2Expr dispatcher value)
					 (tree2Action dispatcher action) )
				    :> AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject)
  | SEQUENCEACTION actions -> ((new SequenceActionObject.sequenceActionObject (List.map (tree2Action dispatcher) actions)) :> AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject)
  | COMMENT (comment,tree) ->
     let tmp = (tree2Action dispatcher tree) in
     begin
       (tmp#attachComment comment);
       tmp
     end			
  | _ -> raise NotAnAction

and tree2Expr dispatcher tree =
  match tree with
    ACTIONEXPRESSION action -> new ActionExpressionObject.actionExpressionObject (tree2Action dispatcher action)
  | ACTIONWRAPPER action -> new ActionWrapperExpr.actionWrapperExpr (tree2Action dispatcher action)
  | QUOTEDIDEXPRESSION id -> new QuotedIdExpressionObject.quotedIdExpressionObject id
  | OBJECTWRAPPEREXPRESSION expr -> new ObjectWrapperExpressionObject.objectWrapperExpressionObject (tree2Object dispatcher expr)
  | PROTOTYPESEXPRESSION (expr,useCases) -> new PrototypesExpressionObject.prototypesExpressionObject (tree2Expr dispatcher expr) (List.map (tree2Prototype dispatcher) useCases)
  | OBJECTEXPRESSION defs ->
     new ObjectExpressionObject.objectExpressionObject (List.map (fun (id,patterns,expr) -> (id,(List.map (tree2Pattern dispatcher) patterns),(tree2Expr dispatcher expr))) defs)
  | BOOLEXPRESSION bl -> new BoolExpressionObject.boolExpressionObject bl
  | NUMEXPRESSION fl -> new NumExpressionObject.numExpressionObject fl
  | LISTEXPRESSION lst -> new ListExpressionObject.listExpressionObject (List.map (tree2Expr dispatcher) lst)
  | NODEXPRESSION -> new NodExpressionObject.nodExpressionObject
  | STRINGEXPRESSION s -> new StringExpressionObject.stringExpressionObject s
  | NATIVEFUNCTION -> raise NativeFunction
  | MATRIXWRAPPER m -> new MatrixWrapperObject.matrixWrapperObject m
  | MATRIXEXPRESSION m -> new MatrixExpressionObject.matrixExpressionObject
			      (Array.map
				 (fun x ->
				  (Array.map
				     (fun y -> (tree2Expr dispatcher y))
				     x ) )
				 m )
  | IDEXPRESSION st -> new IdExpressionObject.idExpressionObject st
  | ATTRIBUTEACCESSEXPRESSION (expr,id) -> new AttributeAccessExpressionObject.attributeAccessExpressionObject (tree2Expr dispatcher expr) id
  | FUNCTIONEXPRESSION lambdas ->
     new FunctionExpressionObject.functionExpressionObject
	 (List.map (fun (patterns,expr) -> ((List.map (tree2Pattern dispatcher) patterns),(tree2Expr dispatcher expr)))
		   lambdas)
  | FUNCTIONCALLEXPRESSION (fn,params) ->
     new FunctionCallExpressionObject.functionCallExpressionObject (tree2Expr dispatcher fn) (List.map (tree2Expr dispatcher) params)
  | INSTANCESEXPRESSION lst -> new InstancesExpressionObject.instancesExpressionObject (List.map (tree2Prototype dispatcher) lst)
  | LETEXPRESSION (defs,expr) ->
     new LetExpressionObject.letExpressionObject
	 (List.map (fun (pattern,expr) -> ((tree2Pattern dispatcher pattern),(tree2Expr dispatcher expr))) defs)
	 (tree2Expr dispatcher expr)
  | MATCHEXPRESSION (expr,matchs) ->
     new MatchExpressionObject.matchExpressionObject
	 (tree2Expr dispatcher expr)
	 (List.map (fun (patterns,expr) -> ((List.map (tree2Pattern dispatcher) patterns),(tree2Expr dispatcher expr))) matchs)
  | COMMENT (comment,tree) ->
     let tmp = (tree2Expr dispatcher tree) in
     begin
       (tmp#attachComment comment);
       tmp
     end			
  | _ -> raise NotAnExpr

and tree2Pattern dispatcher tree =
  match tree with
    WILDCARDPATTERN -> new WildcardPatternObject.wildcardPatternObject
  | STRINGPATTERN st -> new StringPatternObject.stringPatternObject st
  | RENAMINGPATTERN (pattern,id) -> new RenamingPatternObject.renamingPatternObject (tree2Pattern dispatcher pattern) id
  | MATRIXPATTERN lst -> new MatrixPatternObject.matrixPatternObject
			     (Array.map (fun x -> Array.map (tree2Pattern dispatcher) x) lst)
  | BOOLPATTERN bl -> new BoolPatternObject.boolPatternObject bl
  | CONSPATTERN (car,cdr) -> new ConsPatternObject.consPatternObject (tree2Pattern dispatcher car) (tree2Pattern dispatcher cdr)
  | NUMPATTERN fl -> new NumPatternObject.numPatternObject fl
  | LISTPATTERN lst -> new ListPatternObject.listPatternObject (List.map (tree2Pattern dispatcher) lst)
  | IDPATTERN id -> new IdPatternObject.idPatternObject id
  | COMMENT (comment,tree) ->
     let tmp = (tree2Pattern dispatcher tree) in
     begin
       (tmp#attachComment comment);
       tmp
     end			
  | _ -> raise NotAPattern

and tree2Function dispatcher tree =
  match tree with
  | FUNCTION lambdas -> new FunctionExpressionObject.functionExpressionObject (List.map (fun (params,expr) -> ((List.map (tree2Pattern dispatcher) params),(tree2Expr dispatcher expr))) lambdas)
  | COMMENT (comment,tree) ->
     let tmp = (tree2Function dispatcher tree) in
     begin
       (tmp#attachComment comment);
       tmp
     end
  | _ -> raise NotAFunction

and tree2Prototype dispatcher tree =
  match tree with
    STRINGPROTOTYPE (uc,st) -> new StringPrototypeObject.stringPrototypeObject uc st
  | OBJECTPROTOTYPE (uc,objs) -> new ObjectPrototypeObject.objectPrototypeObject uc (List.map (fun (id,expr) -> (id,(tree2Prototype dispatcher expr))) objs)
  | BOOLPROTOTYPE (uc,bl) -> new BoolPrototypeObject.boolPrototypeObject uc bl
  | MATRIXPROTOTYPE (uc,mat) ->
      new MatrixPrototypeObject.matrixPrototypeObject uc mat
  | NUMPROTOTYPE (uc,fl) -> new NumPrototypeObject.numPrototypeObject uc fl
  | LISTPROTOTYPE (uc,lst) -> new ListPrototypeObject.listPrototypeObject uc (List.map (tree2Prototype dispatcher) lst)
  | COMMENT (comment,tree) ->
     let tmp = (tree2Prototype dispatcher tree) in
     begin
       (tmp#attachComment comment);
       tmp
     end
  | _ -> raise NotAPrototype

and tree2Object dispatcher tree =
  match tree with
    OBJECT attributes ->
    let obj = new ObjectObject.objectObject [] in
    List.iter (fun (id,expr) -> (obj#addAttribute id (tree2Expr dispatcher expr)));
    obj
  | _ -> raise NotAnObject
;;
