(* Types! *)

type 'a pads_md =
  { pads_num_errors : int;
    pads_error_msg : string list;
    pads_data : 'a;
    pads_extra : (string * string) list
  }

type filepath = string
  
(* Regular expressions.
 * Snd part of REd is default rep *)
type pads_re = 
  | RE of string
  | REd of string * string
      
(* Often used as terminators *)
type pads_constant =
  | PTI of int
  | PTS of string
  | PTRE of pads_re
  | PTEOF

      
type read_result = 
  | Error of string list 
  | Contents of string list

type pads_manifest_error =
| RegexMatchError of string
| ListLengthError
| VariantMismatchError
| ListLengthMismatchError

type 'a padsManifest =
  { pads_man_errors : pads_manifest_error list;
    pads_str : string;
    pads_manifest : 'a}

(* Functions *)

    
let empty_md x = 
  { pads_num_errors = 0;
    pads_error_msg = [];
    pads_data = x;
    pads_extra = []
  }

let error_md l x =
  { pads_num_errors = List.length l;
    pads_error_msg = l;
    pads_data = x;
    pads_extra = []
  }

let make_mani s m =
  { pads_man_errors = [];
    pads_str = s;
    pads_manifest = m}
    
let print_md_errors md =
  List.iter (fun err ->                                                         
    Printf.printf "Error: %s\n" err
  ) md.pads_error_msg    

let exit_on_error md =
  if md.pads_num_errors > 0
  then begin
    print_md_errors md;
    exit 1;
  end
  else ()
    
let sub_starts_with s1 s2 = 
  String.length s1 >= String.length s2 &&
  String.sub s1 0 (String.length s2) = s2

let read_file (path:string) : read_result =
  if not (Sys.file_exists path) then
    Error [Printf.sprintf "No such file: %s" path]
  else if Sys.is_directory path then
    Error [Printf.sprintf "Is a directory: %s" path]
  else 
    let open Core in
    let input = In_channel.read_all path in
    let l =
      match String.split_lines input |> List.rev with
      | [] -> []
      | hd :: tl ->
        (* TODO: This is somewhat broken as it sanitizes the input. We
           should move to a model that just uses a string *)
        let eol = if String.is_substring ~substring:"\r\n" input then "\r\n" else "\n" in
        let tl = List.map ~f:(fun s -> s ^ eol) tl in
        let hd = 
          if Core.String.is_suffix ~suffix:eol input
          then hd ^ eol
          else hd
        in
        hd :: tl |> List.rev
    in
    Contents l 
