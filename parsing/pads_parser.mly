%{ open Pads_types 
   open Location
   
   let make_loc p1 p2 =
     { loc_start = p1;
       loc_end = p2;
       loc_ghost = false;
     }
%}

%token <string> ID
%token LBRACE
%token RBRACE
%token LBRACK
%token RBRACK
%token LPAREN
%token RPAREN
%token COMMA
%token EOF
%token SEMICOLON
%token EQ
%token BAR

%token <string> STRING
%token <string> AQUOT

%token <string> UID
%token <char> CHAR
%token <int> INT
%token PINT
%token PFLOAT
%token PSTRING
%token PLIST
%token PEOF
%token PTYPE
%token PDATATYPE
%token OF
%token COLON
%token STAR


(* PADS *)
    
%type <Pads_types.varname * Pads_types.pads_node Pads_types.ast> desc
%type <Pads_types.pads_node Pads_types.ast> pads_const pads_ast pads_dtype inter_ast final_ast
%type <Pads_types.pads_fixed> pfix
%type <Pads_types.precord_entry> rentry
    
%type <Pads_types.varname * Pads_types.pads_node Pads_types.ast> named_p variant

%start <(Pads_types.varname * Pads_types.pads_node Pads_types.ast) list> prog
%%

(* PADS Parsing *)


(* The program is list of descriptions followed by an End of File *)
prog: l = nonempty_list(desc); EOF { l };

(* The main type of node in the AST, a PADS description *)
desc:
  | PTYPE; x = ID; EQ; o = pads_ast { x,o }
  | PDATATYPE; x = ID; EQ; o = pads_dtype { x,o }

pads_dtype: option(BAR) ; l = separated_nonempty_list(BAR,variant)
    { let loc = make_loc $startpos($1) $endpos(l) in
     mk_ast loc @@ Pdatatype l }
 
pads_ast:
 | p1 = inter_ast ; STAR ; p2 = pads_ast
   { let loc = make_loc $startpos(p1) $endpos(p2) in
     mk_ast loc @@ Ptuple (p1,p2) }
 | inter_ast { $1 }

inter_ast:
 | l = delimited(LBRACE,separated_nonempty_list(SEMICOLON,rentry),RBRACE)
   { let loc = make_loc $startpos $endpos in
     mk_ast loc @@ Precord l }
 | LBRACK; p = named_p ; BAR ; pred = AQUOT ; RBRACK
   { let loc = make_loc $startpos($1) $endpos($5) in
     let (x,p) = p in
     mk_ast loc @@ Ppred (x,p,pred) }   
 | p = inter_ast ; PLIST ; LPAREN; s = pfix ; COMMA ; t = pfix ; RPAREN
   { let loc = make_loc $startpos(p) $endpos($7) in
     mk_ast loc @@ Plist (p,s,t) }
 | final_ast { $1 }

     
final_ast:
 | delimited(LPAREN,pads_ast,RPAREN) {$1}
 | x = ID
   { let loc = make_loc $startpos $endpos in
     mk_ast loc @@ Pvar x }
 | pads_const {$1}
     
(* The base types as defined by PADS *)
pads_const:
 | PINT { let loc = make_loc $startpos $endpos in
           mk_ast loc Pint }
 | PFLOAT { let loc = make_loc $startpos $endpos in
           mk_ast loc Pfloat }
 | PSTRING ; LPAREN ; x = pfix ; RPAREN 
   { let loc = make_loc $startpos($1) $endpos($4) in
    mk_ast loc @@ Pstring x }
 | x = pfix
   { let loc = make_loc $startpos $endpos in
     mk_ast loc @@ Pconst x }
   
pfix:
 | INT { PFInt $1} 
 | STRING { PFStr $1 }
 | CHAR { PFStr (String.make 1 $1) }
 | AQUOT { PFRE $1 }
 | PEOF { PFEOF }

rentry: 
 | pads_ast { Unnamed $1 }
 | named_p { let (x,p) = $1 in Named(x,p) }

   
named_p: separated_pair(ID,COLON,pads_ast) { $1 }
variant: separated_pair(UID,OF,pads_ast) { $1 }
