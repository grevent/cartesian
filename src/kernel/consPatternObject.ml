
class consPatternObject car cdr =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractPatternObject.abstractPatternObject

  method getIds() = 
    (car#getIds())@(cdr#getIds())
    
  method matchToExpression env expr =
    let exprEval = expr#eval env in

    if exprEval#isObject() then
      match ((exprEval#returnObject())#getAttributes()) with
	(_,hd)::tl -> 
	  begin 
	    (car#matchToExpression env hd) &&
	      (cdr#matchToExpression env (new ObjectWrapperExpressionObject.objectWrapperExpressionObject (new ObjectObject.objectObject tl)))
	  end;
      | [] -> 
	false
    else
      false

  method toTree() =
    CartesianTree.CONSPATTERN ((car#toTree()),(cdr#toTree()))

end;;
