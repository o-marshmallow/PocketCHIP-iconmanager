let file_to_edit = "/usr/share/pocket-home/config.json";;
let backup () =
  let _ =Sys.command ("cp " ^ file_to_edit ^ " " ^ file_to_edit^".backup") in
  ()

exception UnexpectedSyntax;;
exception Invalid_choice;;

let json = Yojson.Safe.from_file file_to_edit;;

let from_pages, pages =
  match json with
  | `Assoc ([a;("pages",pages)]) ->
     (fun p -> (`Assoc ([a;("pages",p)]) : Yojson.Safe.json))
     , pages
  | _ -> raise UnexpectedSyntax

let from_fst_pages, fst_pages =
  match pages with
  | `List (elt::tail) ->
     (fun pages -> `List (pages::tail))
     , elt
  | _ -> raise UnexpectedSyntax

let from_items_list, items_list =
  match fst_pages with
  | `Assoc ([a;("items",`List(list));tail]) ->
     (fun items -> `Assoc ([a;("items",`List(items));tail]) )
     , list
  | _ -> raise UnexpectedSyntax
;;

let reconstruct items =
  from_pages (from_fst_pages (from_items_list items))

let elt_from_json i = function
  | `Assoc ([ ("name",  `String(name));
	      ("icon",  `String(icon));
	      ("shell", `String(shell)) ]) -> 
     let indice = (string_of_int i) in
     let line1 = indice^" -\tName : "^name^"\n" in
     let line2 = "\tIcon : "^icon^"\n" in
     let line3 = "\tShell : "^shell^"\n" in
     line1^line2^line3
  | _ -> raise UnexpectedSyntax

let available_icons items_list = 
  List.mapi
    (fun i elt -> elt_from_json (i+1) elt)
    items_list

let print_list list =
  List.iter print_string list

type choice =
  | List
  | Add
  | Delete
  | Exit_save
  | Exit_wo

let choice_of_int = function
  | 1 -> List
  | 2 -> Add
  | 3 -> Delete
  | 4 -> Exit_save
  | 5 -> Exit_wo
  | _ -> raise Invalid_choice

let show_menu () =
  print_endline "╔══════════Choose an option══════════╗";
  print_endline "║ 1. Icons list                      ║";
  print_endline "╠════════════════════════════════════╣";
  print_endline "║ 2. Add an icon                     ║";
  print_endline "╠════════════════════════════════════╣";
  print_endline "║ 3. Delete an icon                  ║";
  print_endline "╠════════════════════════════════════╣";
  print_endline "║ 4. Exit saving changes             ║";
  print_endline "╠════════════════════════════════════╣";
  print_endline "║ 5. Exit without saving changes     ║";
  print_endline "╚════════════════════════════════════╝"

let rec choose () =
  try
    print_string "Choice : ";
    let value = read_int () in
    choice_of_int value
  with
  | Invalid_choice -> 
     let () = print_endline "Invalid option, try again...\n" in
     choose ()
  | _ ->
     let () = print_endline "Write either 1, 2, 3 or 4\n" in
     choose ()

let rec get_delete_number list =
  try
    Printf.printf "Which icon to delete ? [1-%d]\n" (List.length list);
    let i = read_int () in
    (i-1)
  with
    _ -> print_endline "Invalid choice"; get_delete_number list

let delete_item list i =
  let rec aux buf i = function
    | [] -> assert false
    | elt::tail ->
       if i = 0
       then (List.rev buf)@tail
       else aux (elt::buf) (i-1) tail
  in
  aux [] i list

let rec add_entry items_list =
  print_string "Name of the new icon : ";
  let name = read_line () in
  print_string "Icon path : ";
  let path = read_line () in
  print_string "Shell command to launch : ";
  let shell = read_line () in
  let json = `Assoc ([ ("name",  `String(name));
	      ("icon",  `String(path));
	      ("shell", `String(shell)) ]) in
  print_endline "Confirm adding ? [Y(es)/n(o)/c(ancel)]";
  let answ = read_line () in
  let answ = String.uppercase answ in
  match answ with
  | ""
  | _ when answ.[0]='Y' -> items_list@[json]
  | _ when answ.[0]='N' -> add_entry items_list
  | _ when answ.[0]='C' -> items_list
  | _ -> print_endline "Unknown command"; add_entry items_list
      
let rec main items_list =
  show_menu ();
  let choice = choose () in
  let items_list =
    match choice with
    | List ->
       let () = print_list (available_icons items_list) in
       let () = print_endline "Press any key to continue..." in
       let _ = read_line () in
       items_list
    | Add -> add_entry items_list
    | Delete ->
       if (List.length items_list) = 0
       then
	 let () = print_endline "No icons ! Please add at least one" in
	 items_list
       else
	 let i = get_delete_number items_list in
	 delete_item items_list i
    | Exit_save ->
       backup ();
       let json = reconstruct items_list in
       let str = Yojson.Safe.pretty_to_string json in
       let out = open_out file_to_edit in
       output_string out str;
       flush out;
       close_out out;
       exit 0
    | Exit_wo -> exit 0
  in
  main items_list
;;

main items_list
