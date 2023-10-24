open Jsonrpc
open Eio
include Base
module Header= Lsp.Header
type 'a prom = 'a Eio.Promise.t * 'a Eio.Promise.u

module Json = struct
include Jsonrpc.Json
  type t = Ppx_yojson_conv_lib.Yojson.Safe.t

  let to_pretty_string (t : t) = Yojson.Safe.pretty_to_string ~std:false t
  let error = Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
  let pp ppf (t : t) = Yojson.Safe.pretty_print ppf t

  let rec of_dyn (t : Dyn.t) : t =
    match t with
    | Opaque ->
      `String "<opaque>"
    | Unit ->
      `String "()"
    | Int i ->
      `Int i
    | Int32 i ->
      `Int (Int32.to_int_exn i)
    | Nativeint i ->
      `Int (Nativeint.to_int_exn i)
    | Int64 i ->
      `Int (Int64.to_int_exn i)
    | Bool b ->
      `Bool b
    | String s ->
      `String s
    | Bytes s ->
      `String (Bytes.to_string s)
    | Char c ->
      `String (String.make 1 c)
    | Float f ->
      `Float f
    | Option None ->
      `String "<none>"
    | Option (Some s) ->
      of_dyn s
    | List xs ->
      `List (List.map ~f:of_dyn xs)
    | Array xs ->
      `List (List.map ~f:of_dyn (Array.to_list xs))
    | Tuple xs ->
      `List (List.map ~f:of_dyn xs)
    | Record r ->
      `Assoc (List.map r ~f:(fun (k, v) -> k, of_dyn v))
    | Variant (name, args) ->
      `Assoc [ name, of_dyn (List args) ]
    | Set xs ->
      `List (List.map ~f:of_dyn xs)
    | Map map ->
      `List (List.map map ~f:(fun (k, v) -> `List [ of_dyn k; of_dyn v ]))
  ;;

  let to_string t = Yojson.Safe.to_string t

  let of_string s = Yojson.Safe.from_string s

  let yojson_of_t x = x

  let t_of_yojson x = x

  let error = Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error

  let yojson_of_list = Ppx_yojson_conv_lib.Yojson_conv.yojson_of_list

  module Jsonable = Ppx_yojson_conv_lib.Yojsonable

  let bool b = `Bool b

  let field fields name conv = List.Assoc.find ~equal:String.equal fields name  |> Option.map ~f:conv

  let field_exn fields name conv =
    match field fields name conv with
    | Some f -> f
    | None -> error ("missing field: " ^ name) (`Assoc fields)

  module Conv = struct
    include Ppx_yojson_conv_lib.Yojson_conv
  end

  module O = struct
    let ( <|> ) c1 c2 json =
      match c1 json with
      | s -> s
      | (exception Jsonrpc.Json.Of_json (_, _))
      | (exception Conv.Of_yojson_error (_, _)) -> c2 json
  end

  module Object = struct
    type json = t

    type nonrec t = (string * t) list

    let yojson_of_t t : json = `Assoc t

    let t_of_yojson (t : json) : t =
      match t with
      | `Assoc t -> t
      | json -> error "object expected" json
  end

  module Option = struct
    type 'a t = 'a option

    let yojson_of_t f = function
      | None -> `Null
      | Some x -> f x

    let t_of_yojson f = function
      | `Null -> None
      | json -> Some (f json)
  end

  module Of = struct
    let list = Ppx_yojson_conv_lib.Yojson_conv.list_of_yojson

    let pair f g json =
      match json with
      | `List [ x; y ] -> (f x, g y)
      | json -> error "pair" json

    let int_pair =
      let int = Ppx_yojson_conv_lib.Yojson_conv.int_of_yojson in
      pair int int

    let untagged_union (type a) name (xs : (t -> a) list) (json : t) : a =
      match
        List.find_map xs ~f:(fun conv ->
            try Some (conv json)
            with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (_, _) ->
              None)
      with
      | None -> error name json
      | Some x -> x

    let literal_field (type a) (name : string) (k : string) (v : string)
        (f : t -> a) (json : t) : a =

    let open Printf in
    let open String in
      match json with
      | `Assoc xs -> (
        let ks, xs =
          List.partition_map xs ~f:(fun (k', v') ->
              if k = k' then
                if  Stdlib.(=)(`String v ) v' then Either.first k
                else error (sprintf "%s: incorrect key %s" name k) json
              else Either.second (k', v'))
        in
        match ks with
        | [] -> error (sprintf "%s: key %s not found" name k) json
        | [ _ ] -> f (`Assoc xs)
        | _ :: _ -> error (sprintf "%s: multiple keys %s" name k) json)
      | _ -> error (sprintf "%s: not a record (key: %s)" name k) json
  end
end


module Id = struct
  include Id
  module Table = Stdlib.MoreLabels.Hashtbl.Make (Id)
end

module Notify = struct
  type t =
    | Stop
    | Continue
end

module Sender = struct
  type t = {
    mutable called : bool;
    for_ : Id.t;
    send : Response.t -> unit (*FIber*);
  }

  let make id send = { for_ = id; called = false; send }

  let send t (r : Response.t) : unit =
    if t.called
    then failwith "cannot send response twice"
    else if not (Id.equal t.for_ r.id)
    then failwith "invalid id"
    else t.called <- true;
    t.send r
  ;;
end

exception Stopped of Request.t
let () =
  Stdlib.Printexc.register_printer (function
    | Stopped req ->
      let json = Request.yojson_of_t req in
      Some ("Session closed. Request will not be answered. " ^ Json.to_pretty_string json)
    | _ ->
      None)
;;

module Reply = struct
  type t =
    | Now of Response.t
    | Later of ((Response.t -> unit) -> unit)

  let now (r : Response.t) = Now r

  (*used if you would like to send the response at a later date. though I'm not sure i get why *)
  let later f = Later f

  let send (t : t) ~sw sender =
    match t with
    | Now r ->
      Sender.send sender r
    | Later f ->
      Fiber.fork ~sw (fun () -> f (fun (r : Response.t) -> Sender.send sender r))
  ;;
end

  let response_of_exn id exn =
    let error =
      match exn with
      | Jsonrpc.Response.Error.E resp ->
        resp
      | e ->
        let data = `String (exn |> Exn.to_string) in
        Response.Error.make ~code:InternalError ~data ~message:"uncaught exception" ()
    in
    Response.error id error
  ;;


