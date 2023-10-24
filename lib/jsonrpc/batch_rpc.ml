open Jsonrpc
open Eio
open Base
open Import


module Make(Chan: Io_types.Packet_Channel)= struct
  include Core.Core(Chan)

  module Batch = struct
    type response =
      Jsonrpc.Request.t * (Jsonrpc.Response.t, [ `Stopped | `Cancelled ]) Result.t prom

    type t = [ `Notification of Notification.t | `Request of response ] list ref

    let await (req, resp) = read_request_ivar req resp
    let create () = ref []

    (** Useses this Rpc connection to send a notifiction out via the output stream*)
    let notification t n = t := `Notification n :: !t

    (** Useses this Rpc connection to send a request out via the output stream*)
    let request (t : t) r : response =
      let ivar = Promise.create () in
      let resp = r, ivar in
      t := `Request resp :: !t;
      resp
    ;;
  end
  let submit (t : _ t) (batch : Batch.t) =
    check_running t;
    let pending = !batch in
    batch := [];
    let pending, ivars =
      List.fold_left pending ~init:([], []) ~f:(fun (pending, ivars) -> function
        | `Notification n ->
          Jsonrpc.Packet.Notification n :: pending, ivars
        | `Request ((r : Request.t), ivar) ->
          Jsonrpc.Packet.Request r :: pending, (r.id, ivar) :: ivars)
    in
    List.iter ivars ~f:(fun (id, ivar) -> register_request_ivar t id ivar);
    Chan.send t.chan pending
  ;;
  end