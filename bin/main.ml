open Sys
open Unix

let get_home_dir () = getenv_opt "HOME"

let home_dir () =
  let home_dir = get_home_dir () in
  match home_dir with Some dir -> dir | None -> "/"

let history_file_name () =
  let homeDir = get_home_dir () in
  match homeDir with Some dir -> Some (dir ^ "/.osh_history") | None -> None

let add_to_history historyFile line =
  match historyFile with
  | None -> ()
  | Some historyFile ->
      let oc = open_out_gen [ Open_append; Open_creat ] 0o666 historyFile in
      output_string oc (line ^ "\n");
      close_out oc

let parseCommandAndArgs line =
  let list = String.split_on_char ' ' line in
  match list with [] -> ("", []) | cmd :: args -> (cmd, [ cmd ] @ args)

let runCommand cmd args =
  match cmd with
  | _ ->
      (try execvp cmd (Array.of_list args)
       with Unix_error (err, _, _) ->
         print_endline (cmd ^ ": " ^ error_message err));
      exit 255

let print_status program status =
  match status with
  | WEXITED 255 -> ()
  | WEXITED 0 -> ()
  | WEXITED code -> Printf.printf "%s exited with code %d\n" program code
  | WSIGNALED signal ->
      Printf.printf "%s was killed by signal %d\n" program signal
  | WSTOPPED signal ->
      Printf.printf "%s was stopped by signal %d\n" program signal

let get_current_dir () =
  let cwd = getcwd () in
  match cwd with
  | exception Unix_error (err, _, _) ->
      print_endline (error_message err);
      None
  | dir -> (
      String.starts_with ~prefix:(home_dir ()) dir |> function
      | true ->
          Some
            ("~"
            ^ String.sub dir
                (String.length (home_dir ()))
                (String.length dir - String.length (home_dir ())))
      | false -> Some dir)

let prompt_user () =
  let line = read_line () in
  let cmd, args = parseCommandAndArgs line in
  add_to_history (history_file_name ()) line;
    match cmd with
    | "exit" -> exit 0;
    | "cd" -> chdir(List.nth args 1)
    | _ ->
      match fork () with
      | 0 -> runCommand cmd args
      | _ ->
          let _, status = wait () in
          print_status "Program" status

let osh () =
  print_endline "Welcome to Osh!";
  try
    while true do
      let cwd = get_current_dir () in
      match cwd with
      | None ->
          print_string "> ";
          prompt_user ()
      | Some dir ->
          Printf.printf "\027[92m%s \027[0m" dir;
          prompt_user ()
    done
  with End_of_file -> print_endline "Goodbye!"
;;

handle_unix_error osh ()
