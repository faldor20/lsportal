open Base

module Json = struct
  type t = Ppx_yojson_conv_lib.Yojson.Safe.t

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
