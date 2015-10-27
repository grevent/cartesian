
type typeDescription =
    INT | 
    FLOAT |
    STRING |
    LIST of typeDescription |
    ARRAY of typeDescription |
    PAIR of (typeDescription list) |
    ALTERNATIVES of (typeDescription list) |
    NOD |
    TYPE of typeDescription |
    OPENOBJECT of ((string*typeDescription) list) |
    CLOSEDOBJECT of ((string*typeDescription) list) |    
    NOTEVALUATED |
    GENERIC of int |
    ACTION |
    ERROR of string |
    PARAMETRIZEDTYPE of ((typeDescription list)*typeDescription) |
    ACTOR
