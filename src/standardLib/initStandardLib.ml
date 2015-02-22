
let init obj =
  obj#addAttribute "bool?" (new NativeFunctionObject.nativeFunctionObject (new BoolNativeObject.boolNativeObject)); 
  obj#addAttribute "string?" (new NativeFunctionObject.nativeFunctionObject (new StringNativeObject.stringNativeObject));
  obj#addAttribute "float?" (new NativeFunctionObject.nativeFunctionObject (new FloatNativeObject.floatNativeObject));
  obj#addAttribute "int?" (new NativeFunctionObject.nativeFunctionObject (new IntNativeObject.intNativeObject));  
  obj#addAttribute "number?" (new NativeFunctionObject.nativeFunctionObject (new NumberNativeObject.numberNativeObject));
  obj#addAttribute "char?" (new NativeFunctionObject.nativeFunctionObject (new CharNativeObject.charNativeObject)); 
  obj#addAttribute "object?" (new NativeFunctionObject.nativeFunctionObject (new ObjectNativeObject.objectNativeObject));  
  obj#addAttribute "function?" (new NativeFunctionObject.nativeFunctionObject (new FunctionNativeObject.functionNativeObject));  
  obj#addAttribute "list?" (new NativeFunctionObject.nativeFunctionObject (new ListNativeObject.listNativeObject));
  obj#addAttribute "array?" (new NativeFunctionObject.nativeFunctionObject (new ArrayNativeObject.arrayNativeObject));
  obj#addAttribute "iterable?" (new NativeFunctionObject.nativeFunctionObject (new IterableNativeObject.iterableNativeObject));
  obj#addAttribute "comparable?" (new NativeFunctionObject.nativeFunctionObject (new ComparableNativeObject.comparableNativeObject));
  obj#addAttribute "action?" (new NativeFunctionObject.nativeFunctionObject (new ActionNativeObject.actionNativeObject));
  obj#addAttribute "nod?" (new NativeFunctionObject.nativeFunctionObject (new NodNativeObject.nodNativeObject));
  obj#addAttribute "abs" (new NativeFunctionObject.nativeFunctionObject (new AbsNativeObject.absNativeObject));  
  obj#addAttribute "neg" (new NativeFunctionObject.nativeFunctionObject (new NegNativeObject.negNativeObject));  
  obj#addAttribute "cos" (new NativeFunctionObject.nativeFunctionObject (new CosNativeObject.cosNativeObject));  
  obj#addAttribute "sin" (new NativeFunctionObject.nativeFunctionObject (new SinNativeObject.sinNativeObject));  
  obj#addAttribute "tan" (new NativeFunctionObject.nativeFunctionObject (new TanNativeObject.tanNativeObject));  
  obj#addAttribute "compare" (new NativeFunctionObject.nativeFunctionObject (new CompareNativeObject.compareNativeObject));
  obj#addAttribute "min" (new NativeFunctionObject.nativeFunctionObject (new MinNativeObject.minNativeObject));
  obj#addAttribute "max" (new NativeFunctionObject.nativeFunctionObject (new MaxNativeObject.maxNativeObject));

  


