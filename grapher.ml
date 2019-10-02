open Lwt
open Cohttp
open Cohttp_lwt_unix
open Str
open Soup

let get_body scp =
  Client.get (Uri.of_string @@ "http://www.scp-wiki.net/scp-" ^ scp)
  >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  ignore code;
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  body

let get_scps scp =
  let body = Lwt_main.run @@ get_body scp in
  let soup = parse body in
  soup $$ "a[href]" |> fold (fun xs a -> R.attribute "href" a :: xs) []
  |> List.map @@ (fun x ->
      if string_match ("scp-\\([0-9]+\\)" |> regexp) x 1
      then matched_group 1 x
      else ""
    )
  |> List.filter (fun x -> String.length x != 0)


let rec traverse depth (scp_relation : (string * string list) list) current_scp =
  if depth < 1 then scp_relation
  else
    begin
      print_endline @@ "Checking: SCP-" ^ current_scp;
      let referenced_scps = get_scps current_scp in
      let scp_relation = (current_scp, referenced_scps) :: scp_relation in
      let new_scps = referenced_scps |> List.filter (fun x -> not @@ List.mem_assoc x scp_relation) in
      List.fold_left (traverse @@ depth - 1) scp_relation new_scps
    end

let init_file () =
  let oc = open_out "graph.dot" in
  write_channel oc "digraph {\n";
  oc

let write_to_file oc scp references =
  List.iter (fun x -> write_channel oc @@ "\"SCP-" ^ scp ^ "\" -> \"SCP-" ^ x ^ "\"\n") references

let close_file oc =
  write_channel oc "}";
  close_out oc

let _ =
  let scp = "002" in
  let oc = init_file () in
  traverse 2 [] scp
  |> List.sort_uniq compare
  |> List.iter (fun (scp, references) ->
      write_to_file oc scp references;
      print_endline ("> " ^ scp ^ ":");
      List.iter print_endline references
    );
  close_file oc
