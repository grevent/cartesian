
class ['t] nativeFunctionHelper (default: 't) =
object(self)
  method evalNum obj = self#evalDefault obj
  method evalString obj = self#evalDefault obj
  method evalBool obj = self#evalDefault obj
  method evalObj obj = self#evalDefault obj
  method evalFunction obj = self#evalDefault obj
  method evalAction obj = self#evalDefault obj
  method evalNOD obj = self#evalDefault obj
  method evalId obj = self#evalDefault obj
  method evalMatrix obj = self#evalDefault obj
				       
  method evalDefault (obj: AbstractExpressionObject.abstractExpressionObject) = default
  method evalDefaultWOEval (env: AbstractExpressionObject.abstractExpressionObject Env.env) (obj: AbstractExpressionObject.abstractExpressionObject) = default

  method noEval() = false
    
  method getDefault() = default

  method eval env param =
    Debug.debugStd "nativeFunctionHelper(in)" "eval";
    let result = 
      if self#noEval() then
	(self#evalDefaultWOEval env param)
      else
	begin
	  let el = param#eval env in
	  
	  if (el#isNum()) then
	    self#evalNum el
	  else if (el#isString()) then
	    self#evalString el
	  else if (el#isBool()) then
	    self#evalBool el
	  else if (el#isObject()) then
	    self#evalObj el
	  else if (el#isAction()) then
	    self#evalAction el
	  else if (el#isFunction()) then
	    self#evalFunction el
	  else if (el#isId()) then
	    self#evalId el
	  else if (el#isNOD()) then
	    self#evalNOD el
	  else if (el#isMatrix()) then
	    self#evalMatrix el
	  else
	    default
	end
    in
    Debug.debugStd "nativeFunctionHelper(out)" "eval";
    result

end;;
