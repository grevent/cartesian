
class numPrototypeObject uc f = 
object(self)
  inherit AbstractPrototypeObject.abstractPrototypeObject uc
    
  method returnValue() = new NumExpressionObject.numExpressionObject f
    
  method verifyValue vl = 
	if vl#isNum() then
		begin
			let (re0,im0) = vl#returnNum() in
			let (re,im) = f in
		
			if (re == re0) && (im == im0) then
				true
			else
				false
		end
	else
		false

  method toTree() =
    CartesianTree.NUMPROTOTYPE (uc,f)
			    
end;;
      
