
class doActionObject actionObj exprObj =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject

  method exec parents =
    actionObj#exec parents;
    while (exprObj#eval (Env.newEnv parents))#returnBoolean() do
      actionObj#exec parents;
    done;

  method toString() = 
    "do "^(actionObj#toString())^" while "^(exprObj#toString())

end;;
