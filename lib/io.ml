open Base
open Import
open Eio

exception Error of string


  

let () =
  Stdlib.Printexc.register_printer (function
    | Error msg ->
        Some ("Error: " ^ msg)
    | _ ->
        None )

let caseless_equal a b =
  if phys_equal a b then true
  else
    let len = String.length a in
    len = String.length b
    &&
    let stop = ref false in
    let idx = ref 0 in
    while (not !stop) && !idx < len do
      let c1 = String.unsafe_get a !idx in
      let c2 = String.unsafe_get b !idx in
      let open Char in
      if Char.lowercase c1 <> Char.lowercase c2 then stop := true ;
      Int.incr idx
    done ;
    not !stop

let content_type_lowercase = 
  String.lowercase Header.Private.Key.content_type

let content_length_lowercase =
  String.lowercase Header.Private.Key.content_length

module Make (Chan : sig
  type input

  type output

  val read_line : input -> string option

  val read_exactly : input -> int -> string option

  val write : output -> string list -> unit
end) =
struct
  open Base

  let read_header =
    let init_content_length = -1 in
    let rec loop chan content_length (content_type : string option) =
      let line = Chan.read_line chan in
      match line with
      | None ->
          None
      | Some "" | Some "\r" ->
          Some (content_length, content_type)
      | Some line -> (
        match String.lsplit2 ~on:':' line with
        | None ->
            loop chan content_length content_type
        | Some (k, v) ->
            let k = Stdlib.String.trim k in
            if
              caseless_equal k content_length_lowercase
              && content_length = init_content_length
            then
              let content_length = Int.of_string_opt (Stdlib.String.trim v) in
              match content_length with
              | None ->
                  raise @@ Error "Content-Length is invalid"
              | Some content_length ->
                  loop chan content_length content_type
            else if
              caseless_equal k content_type_lowercase
              && content_type |> Option.is_none
            then
              let content_type = Stdlib.String.trim v in
              loop chan content_length (Some content_type)
            else loop chan content_length content_type )
    in
    fun chan ->
      let res = loop chan init_content_length None in
      match res with
      | None ->
          None
      | Some (content_length, content_type) ->
          let () =
            if content_length = init_content_length then
              raise @@ Error "content length absent"
            else ()
          in
          Some (Header.create ?content_type ~content_length ())

  let read chan =
    let header = read_header chan in
    match header with
    | None ->
        None
    | Some header -> (
        let len = Header.content_length header in
        let buf = Chan.read_exactly chan len in
        match buf with
        | None ->
            raise (Error "unable to read json")
        | Some buf ->
            let json = Json.of_string buf in
            Some (Jsonrpc.Packet.t_of_yojson json) )

  let write chan packet =
    let json = Jsonrpc.Packet.yojson_of_t packet in
    let data = Json.to_string json in
    let content_length = String.length data in
    let header = Header.create ~content_length () in
    Chan.write chan [Header.to_string header; data]
end
