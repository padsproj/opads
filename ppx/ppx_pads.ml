open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Location
open Utility


let pads_mapper argv =
  (* Our getenv_mapper only overrides the handling of expressions in the default mapper. *)
  { default_mapper with
    structure = fun mapper strl ->  
      let rec pads_replace acc str =
         (* Processes PADS descriptions *)
        | { pstr_desc = Pstr_extension
            (({txt = "pads"}, PStr
              [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string (pads_str, Some "")); pexp_loc},_)}]
             ),_)
          ; pstr_loc } ->
          let pads_ast = Parser_helper.pads_parse_string pexp_loc pads_str in
          let _ = List.iter (fun (v,s) -> Hashtbl.add Utility.padsUnitTbl v (Ppx_pads_lib.check_uniticity s)) pads_ast in
          let ocaml_asts : Parsetree.structure = Ppx_pads_lib.def_generator pstr_loc pads_ast in
            ocaml_asts :: acc
              
            (*
          let _ = List.iter (fun (x,p) -> Printf.fprintf stdout "%s=%s\n" x (All_types.show_ast All_types.pp_pads_node p)) pads_ast in
            *)
         (* Rest is kept the same*)
         | x -> [default_mapper.structure_item mapper x] :: acc

            
      in
      List.flatten (List.rev (List.fold_left pads_replace [] strl))
  }

let () = register "forest" forest_mapper

   
