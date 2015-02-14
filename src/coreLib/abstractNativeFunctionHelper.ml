
class virtual ['t] abstractNativeFunctionHelper =
  virtual method evalFloat: AbstractExpressionObject.abstractExpressionObject -> 't
  virtual method evalInt: AbstractExpressionObject.abstractExpressionObject -> 't
  virtual method evalChar: AbstractExpressionObject.abstractExpressionObject -> 't
  virtual method evalString: AbstractExpressionObject.abstractExpressionObject -> 't
