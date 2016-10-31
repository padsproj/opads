(* 
Use Makefile in examples directory

Desugar:
    ./desugar.sh sample/sample.ml

Compile:
   make sample
*)

open Pads
open PadsParser

let ws = REd ("[ \t]*", "   ")

[%%pads {|

  (* Parses a string terminated by a period *)
  ptype pt_string = Pstring('.')

  (* Parses a string terminated by a newline character *)
  ptype nt_string = Pstring('\n')

  (* Strings can also parse a constant number of chars *)
  ptype cl_string = Pstring(7)

  (* Parses a list of newline terminated strings with no separator,
     terminated by an EOF *)
  ptype sList = nt_string Plist("",EOF)

  (* Lists can also parse their argument a constant number of times *)
  ptype sList2 = cl_string Plist("\n",7)

  (* Parses the constant string "hello" *)
  ptype constant = "hello"

  (* Uses a regular expression to parse an integer. 
     $$ antiquotes, allowing a user to write OCaml.
     RE makes a regular expression with the empty string as
     its default value (if parsing fails). *)
  ptype intRegex = $RE "[0-9]+"$  

  (* Same as above, but defaults to "0" *)
  ptype intDRegex = $REd ("[0-9]+","0")$  

  (* Pint is what you should use for integers though *)
  ptype pint = Pint

  (* Pfloat for floats *)
  ptype pfloat = Pfloat

  (* There are also dependent types. Notably, since they check
     arbitrary ocaml predicates, there is no termination guarantee.
     The OCaml code should return a bool to type check. *)

  ptype posInt = [ i : Pint | $i >= 0$ ]

  (* Records are useful for building more complex data formats.
     They let users access different parsed parts by name
     while just parsing and throwing away unnamed parts. *)
  ptype pRec = { x : posInt; ","; 
                 y : cl_string; ",";
                 z : Pint; "\n" }


  (* It is possible to try multiple different parsings
     using a variant type. 
     This returns the first one that was matched. *)
  pdatatype varType =
  | Hello of "Hello"
  | Some_int of Pint

  (* Finally, there are tuples, which are similar to record, except
     that the data is placed in a tuple instead of a record.
     Any parsing with a unit representation (e.g. constant strings)
     is removed from the tuple type, but still parsed. *)
  ptype tupType = varType * "some_separator" * varType

|}]

let ex_dir = Filename.concat  (Sys.getcwd ()) "sample/exDir"


(* Here I will show off file parsing (which PADS was intended for),
   and direct state parsing (which is useful for testing). *)
let test_strings () =
  let (t_rep, t_md) = pt_string_parse @@ Filename.concat ex_dir "pt_string" in
  let test_state = {current="Some interesting line\n"; rest=["7 charshello"]; loc=start_loc} in
  let (n_rep,n_md,n_state) = nt_string_parse_state test_state in
  let (c_rep,c_md,c_state) = cl_string_parse_state n_state in
  let ((),last_md,_) = constant_parse_state c_state in
  Pads.exit_on_error last_md;
  assert(t_rep = "Sweet string terminated by period");
  assert(n_rep = "Some interesting line");
  assert(c_rep = "7 chars")

let pad_to_7 =
  List.map (fun s ->
    let len = String.length s in
    s ^ (String.make (7-len) ' '))                     
    
let test_lists () =
  let l1 = pad_to_7 ["This";"is";"a";"file";"that";"is";"9"] in
  let l2 = l1 @ ["lines";"long"] in
  let (partial,bad_md) = sList2_parse  @@ Filename.concat ex_dir "9_lines" in
  let (file,md) = sList_parse  @@ Filename.concat ex_dir "9_lines" in
  Pads.exit_on_error md;
  (* Note that bad_md would fail since the whole file is not parsed. Padding is
     necessary for the constant string length to work. *)
  assert (partial = l1);
  assert(file = l2)

let test_nums () =
  let state = {current="157.8"; rest=[]; loc=start_loc} in
  let (irep,imd,state) = intRegex_parse_state state in
  let (failrep,failmd,state) = intDRegex_parse_state state in
  (* Since it failed, it doesn't consume any characters, 
     so we have a float remaining *)
  let (frep,fmd,state) = pfloat_parse_state state in
  let posState = {current="1"; rest=[]; loc=start_loc} in
  let negState = {current="-1"; rest=[]; loc=start_loc} in
  let (posrep,posmd,_)= posInt_parse_state posState in
  let (negrep,negmd,_)= posInt_parse_state negState in
  assert (irep = "157");
  assert (failrep = "0");
  assert (frep = 0.8);
  assert (posrep = 1);
  assert (posmd.pads_num_errors = 0);
  assert (negrep = -1);
  assert (negmd.pads_num_errors = 1)

let test_complex () =
  let state = {current="10,7 chars,-10\n"; rest=["Hellosome_separator50"]; loc=start_loc} in
  let (recrep,recmd,state) = pRec_parse_state state in
  let (tuprep,tupmd,_) = tupType_parse_state state in
  assert (recrep.x = 10);
  assert (recrep.y = "7 chars");
  assert (recrep.z = -10);
  assert (fst tuprep = Hello ());
  assert (snd tuprep = Some_int(50))
    
let _ =
  let _ = test_strings () in
  let _ = test_lists () in
  let _ = test_nums () in
  let _ = test_complex () in
  Printf.printf "Since we haven't failed, all tests were successful!\n"
