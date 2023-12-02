%{
  
%}

%token EOF 
%token REDTOK 
%token GREENTOK 
%token BLUETOK
%token COMMATOK
%token SEMICOLONTOK 
%token COLONTOK
%token GAMETOK
%token <int> INTTOK


%start inputfile 
%type <Syntax.record> inputfile 

%% 
inputfile:
| record eof {$1 @ $2}

record: 
| game record {$1 :: $2}
| /* empty */ {[]}

game: 
| GAMETOK INTTOK COLONTOK grabs {{Syntax.id = $2; grabs = $4}}

grabs:
| grab SEMICOLONTOK grabs {[$1] @ $3}
| grab { [$1] }

grab:
| cubes COMMATOK grab {[$1] @ $3} 
| cubes { [$1] }

cubes: 
| INTTOK REDTOK {Syntax.Red $1} 
| INTTOK BLUETOK {Syntax.Blue $1}
| INTTOK GREENTOK {Syntax.Green $1}


eof:
| EOF {[]}
