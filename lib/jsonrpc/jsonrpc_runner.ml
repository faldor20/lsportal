open Jsonrpc
open Eio
open Base
open Import
open Core

module Reply=Reply
module Notify=Notify
module Flow_io=Flow_io
module Log=Log
module Json=Json
(*End of extra stuff i added to make this work*)


module Make(Chan:Io_types.Packet_Channel)=struct 
  include Run_rpc.Make(Chan)

  include Batch_rpc.Make(Chan)

  let create
    ?(on_request = on_request_fail)
    ?(on_notification = on_notification_fail)
    ~name
    chan
    state
    =
    let pending = Id.Table.create 10 in
    {
      chan;
      on_request;
      on_notification;
      pending;
      stopped = Eio.Promise.create ();
      name;
      running = false;
      tick = 0;
      state;
      pending_requests_stopped = false;
  
    }
  ;;



  let notification t (n : Notification.t) =
    check_running t;
    Chan.send t.chan [ Notification n ]
  ;;

  let request t (req : Request.t) =
    check_running t;
    Chan.send t.chan [ Request req ];
    let ivar = Promise.create () in
    register_request_ivar t req.id ivar;
    read_request_ivar req ivar
  ;;

  let request_with_cancel t (req : Request.t) =
    let ivar = Promise.create () in
    let cancel = Promise.resolve (ivar |> snd) (Error `Cancelled) in
    let resp =
      check_running t;
      Chan.send t.chan [ Request req ];
      register_request_ivar t req.id ivar;
      let res = Promise.await (ivar |> fst) in
      match res with
      | Ok s ->
        `Ok s
      | Error `Cancelled ->
        `Cancelled
      | Error `Stopped ->
        raise (Stopped req)
    in
    cancel, resp
  ;;
end
