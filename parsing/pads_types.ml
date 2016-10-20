
type loc = Location.t

let pp_loc = Location.print_loc 

type filepath = string [@@deriving show]
type varname = string [@@deriving show]
type aquot = string [@@deriving show]
  
type fPayload =
  | PNone 
  | PVList of varname list 
  | PRec [@@deriving show]

type 'a ast =
  { node : 'a;
    payload : fPayload;
    loc : loc;
  } [@@deriving show]
(* AST Types for PADS *)

(* Inbuilt / OCaml types *)
type precord_entry =
  | Named of varname * pads_node ast
  | Unnamed of pads_node ast [@@deriving show]

and pads_fixed =
  | PFInt of int
  | PFEOF
  | PFStr of string
  | PFRE of aquot
  
and pads_node =
  | Pint
  | Pfloat
  | Pstring of pads_fixed
  | Pconst of pads_fixed
      
  | Pvar of varname
      
  | Ppred of varname * pads_node ast * aquot
  | Precord of precord_entry list
  | Plist of pads_node ast * pads_fixed * pads_fixed
      
  | Ptuple of pads_node ast * pads_node ast
      
  | Pdatatype of (string * pads_node ast) list
      [@@deriving show]

let mk_ast (loc : loc) (node : 'a) : 'a ast = 
  { node; loc; payload = PNone; }

let mk_p_ast (loc : loc) (payload : fPayload) (node : 'a) : 'a ast = 
  { node; loc; payload}
