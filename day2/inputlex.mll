{
    open Inputgram 
    open Lexing 
    
    exception SyntaxError of string 
    let next_line lexbuf = 
     let pos = lexbuf.lex_curr_p in
     lexbuf.lex_curr_p <-
    {   pos with pos_bol = lexbuf.lex_curr_pos;
            pos_lnum = pos.pos_lnum + 1
    }
}

let white = [' ' '\t']+
let num = ['1' - '9']['0'-'9']* (* lex a digit from 1 - 9 followed by some number of digits from 0 to 9. *)
let colors = "red" | "blue" | "green"
let game = "Game"
let newline = '\r' | '\n' 

rule token = parse 
| white { token lexbuf }
| newline {next_line lexbuf; token lexbuf} 
| num as i {INTTOK (int_of_string i)}
| "Game" {GAMETOK}
| colors { match Lexing.lexeme lexbuf with
             | "blue" -> BLUETOK
             | "red" -> REDTOK
             | "green" -> GREENTOK
             | _ -> raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf))}
| ";" {SEMICOLONTOK}
| "," {COMMATOK}
| ":" {COLONTOK}
| eof { EOF }
| _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf))}