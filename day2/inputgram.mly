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
| cubes SEMICOLONTOK grabs {$1 :: $3}
| cubes { [$1] }


cubes:
| INTTOK REDTOK COMMATOK INTTOK GREENTOK COMMATOK INTTOK BLUETOK {{Syntax.red = Some $1; Syntax.green = Some $4; Syntax.blue = Some $7}}
| INTTOK GREENTOK COMMATOK INTTOK BLUETOK COMMATOK INTTOK REDTOK {{Syntax.green = Some $1; Syntax.blue = Some $4; Syntax.red = Some $7}}
| INTTOK BLUETOK COMMATOK INTTOK REDTOK COMMATOK INTTOK GREENTOK {{Syntax.blue = Some $1; Syntax.red = Some $4; Syntax.green = Some $7}}
| INTTOK REDTOK COMMATOK INTTOK BLUETOK COMMATOK INTTOK GREENTOK {{Syntax.blue = Some $4; Syntax.red = Some $1; Syntax.green = Some $7}}
| INTTOK BLUETOK COMMATOK INTTOK GREENTOK COMMATOK INTTOK REDTOK {{Syntax.green = Some $4; Syntax.blue = Some $1; Syntax.red = Some $7}}
| INTTOK GREENTOK COMMATOK INTTOK REDTOK COMMATOK INTTOK BLUETOK {{Syntax.red = Some $4; Syntax.green = Some $1; Syntax.blue = Some $7}}
| INTTOK REDTOK COMMATOK INTTOK GREENTOK {{Syntax.red = Some $1; Syntax.green = Some $4; Syntax.blue = None}}
| INTTOK GREENTOK COMMATOK INTTOK REDTOK {{Syntax.red = Some $4; Syntax.green = Some $1; Syntax.blue = None}}
| INTTOK GREENTOK COMMATOK INTTOK BLUETOK {{Syntax.red = None; Syntax.green = Some $1; Syntax.blue = Some $4}}
| INTTOK BLUETOK COMMATOK INTTOK GREENTOK {{Syntax.red = None; Syntax.green = Some $4; Syntax.blue = Some $1}}
| INTTOK BLUETOK COMMATOK INTTOK REDTOK {{Syntax.red = Some $4; Syntax.green = None; Syntax.blue = Some $1}}
| INTTOK REDTOK COMMATOK INTTOK BLUETOK {{Syntax.red = Some $1; Syntax.green = None; Syntax.blue = Some $4}}
| INTTOK REDTOK {{Syntax.red = Some $1; Syntax.green = None; Syntax.blue = None}}
| INTTOK GREENTOK {{Syntax.red = None; Syntax.green = Some $1; Syntax.blue = None}}
| INTTOK BLUETOK {{Syntax.red = None; Syntax.green = None; Syntax.blue = Some $1}}


// grab:
// | cubes COMMATOK grab {[$1] @ $3} 
// | cubes { [$1] }

// cubes: 
// | INTTOK REDTOK {Syntax.Red $1} 
// | INTTOK BLUETOK {Syntax.Blue $1}
// | INTTOK GREENTOK {Syntax.Green $1}


eof:
| EOF {[]}
