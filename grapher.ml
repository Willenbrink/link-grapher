open Lwt
open Cohttp
open Cohttp_lwt_unix
open Str
open Soup
open Sexplib
open Sexplib.Std

module type Domain =
sig
  type t [@@deriving sexp]
  type db = {references : (t * (t list)) list; unvisited_nodes : t list} [@@deriving sexp]
  val regex : Str.regexp
  val id_to_string : t -> string
  val id_to_url : t -> Uri.t
  val string_to_id : string -> t
  val empty_db : unit -> db
end

module SCP : Domain =
struct
  type t = string [@@deriving sexp]
  type db = { references : (t * (t list)) list; unvisited_nodes : t list} [@@deriving sexp]
  let url = "http://www.scp-wiki.net/"
  let regex = regexp @@ "/scp-\\([0-9]+\\)"
  let id_to_string id = "SCP-" ^ id
  let id_to_url id = Uri.of_string @@ url ^ "scp-" ^ id
  let string_to_id str = str
  let empty_db () = {references = []; unvisited_nodes = []}
end

module Writer_SEXP (M : Domain) =
struct
  let write_file (db : M.db) =
    let oc = open_out "scp.sexp" in
    M.sexp_of_db db
    |> Sexp.to_string
    |> write_channel oc;
    close_out oc

  let read_file () : M.db =
    try
      read_file "scp.sexp"
      |> Sexp.of_string
      |> M.db_of_sexp
    with Sexplib.Conv.Of_sexp_error (_,_) -> M.empty_db ()
end

module Scraper (M : Domain) =
struct
  let get_body id =
    Unix.sleepf 0.1;
    Client.get @@ M.id_to_url id
    >>= fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    if code != 200 then raise Not_found;
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    body

  let get_references id regex =
    let body = Lwt_main.run @@ get_body id in
    let soup = parse body in
    soup $$ "a[href]"
    |> fold (fun xs a -> R.attribute "href" a :: xs) []
    |> List.map @@ (fun x ->
        if string_match regex x 0
        then matched_group 1 x
        else ""
      )
    |> List.filter (fun x -> String.length x != 0)
    |> List.map M.string_to_id

  let rec traverse depth {M.references : (M.t * M.t list) list; unvisited_nodes} id =
    if depth < 1 then {M.references; unvisited_nodes = id::unvisited_nodes}
    else
      begin
        print_endline @@ "Checking: " ^ M.id_to_string id;
        let new_references = get_references id M.regex in
        let database = (id, new_references) :: references in
        let new_scps = new_references |> List.filter (fun x -> not @@ List.mem_assoc x database) in
        List.fold_left (traverse @@ depth - 1) {references = database; unvisited_nodes} new_scps
      end

  let file_name = "graph.dot"
  let first_line = "digraph {\n"
  let last_line = "}"

  let write_dot_file {M.references; _} =
    let init_file () =
      let oc = open_out file_name in
      write_channel oc first_line;
      oc
    in
    let write_to_file oc (scp,references) =
      references
      |> List.map M.id_to_string
      |> List.iter (fun x -> write_channel oc @@ "\"" ^ M.id_to_string scp ^ "\" -> \"" ^ x ^ "\"\n")
    in
    let close_file oc =
      write_channel oc last_line;
      close_out oc
    in
    let oc = init_file () in
    List.iter (write_to_file oc) references;
    close_file oc

  let read_db_file () =
    let file = open_in "graph.db" in
    let input_line' () = input_line file in
    ()
    (*TODO use preprocessor? Perhaps there is a different feature for compile-time constants*)
end

let _ =
  let module SCP_Scraper = Scraper (SCP) in
  let open SCP_Scraper in
  let module Writer = (Writer_SEXP(SCP)) in
  let ({SCP.unvisited_nodes; _} as database) = Writer.read_file () in
  let scp = if database = SCP.empty_db () then SCP.string_to_id "002" else List.hd unvisited_nodes in
  let database = traverse 5 database scp in
  Writer.write_file database;
  write_dot_file database
