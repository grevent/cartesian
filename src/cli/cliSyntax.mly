

%{
  
%}

%token LIST OBJECTS RULES DECLARATIONS OBJECT INTVALUE RULE DECLARATION EOL EDIT ID START GUI EOF EXIT NEW EXEC EVAL

%type <string> ID
%type <CliTree.cliTree> phrase
%type <int> INTVALUE
      
%start phrase
       
%%
  
phrase: command { $1 }

command: EXEC { EXEC }
command: EVAL { EVAL }
command: LIST OBJECTS { LISTOBJECTS }
command: LIST RULES { LISTRULES }
command: LIST DECLARATIONS { LISTDECLARATIONS }
command: EDIT OBJECT INTVALUE { EDITOBJECT $3 }
command: EDIT RULE INTVALUE { EDITRULE $3 }
command: EDIT DECLARATION ID { EDITDECLARATION $3 }
command: START GUI { STARTGUI }
command: EXIT { EXIT }
