
type cliTree =
	LISTOBJECTS |
	LISTRULES |
	LISTDECLARATIONS |
	EDITOBJECT of int |
	EDITRULE of int |
	EDITDECLARATION of string |
	STARTGUI |
	EVAL |
	EXEC|
	EXIT
;;

let tree2string tree = 
	match tree with
		LISTOBJECTS -> "LIST OBJECTS" |
		LISTRULES -> "LIST RULES" |
		LISTDECLARATIONS -> "LISTDECLARATIONS" |
		EDITOBJECT i -> (Printf.sprintf "LIST DECLARATIONS %d" i) |
		EDITRULE i ->  (Printf.sprintf "EDIT RULE %d" i) |
		EDITDECLARATION id -> (Printf.sprintf "EDIT DECLRATION %s" id) |
		STARTGUI -> "START GUI" |
		EXEC -> "EXECUTE" | 
		EVAL -> "EVAL" |
		EXIT -> "EXIT"		
;;

