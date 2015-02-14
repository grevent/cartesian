
class whileActionObject exprObj actionObj =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject

  method exec parents = 
    while (exprObj#eval (Env.newEnv parents))#returnBoolean() do
      actionObj#exec parents
    done;

  method toString() = 
    "while "^(exprObj#toString())^" do "^(actionObj#toString())

end;;
