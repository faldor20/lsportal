open Jsonrpc
open Import
open Eio

module Core(Chan : Io_types.Packet_Channel) = struct
  exception Done of string
  type 'state t = {
    chan : Chan.t;
    on_request : ('state, Request.t) context -> Reply.t * 'state (*FIber*);
    on_notification : ('state, Notification.t) context -> Notify.t * 'state; (*FIber*)
    pending : (Response.t, [ `Stopped | `Cancelled ]) Result.t prom Id.Table.t;
    stopped : unit prom;
    name : string;
    mutable running : bool;
    mutable tick : int;
    mutable state : 'state;
    mutable pending_requests_stopped : bool;
  }

  and ('a, 'message) context = 'a t * 'message

  type cancel = unit (*FIber*)

  let fire cancel = cancel

  module Context = struct
    type nonrec ('a, 'id) t = ('a, 'id) context

    let message = snd
    let session = fst
    let state t = (session t).state
  end

  let log t = Log.log ~section:t.name
  let log_s t = Log.log_s ~section:t.name

  let on_request_fail ctx : Reply.t * _ =
    let req : Request.t = Context.message ctx in
    let state = Context.state ctx in
    let error = Response.Error.make ~code:InternalError ~message:"not implemented" () in
    Reply.now (Response.error req.id error), state
  ;;

  let on_notification_fail ctx =
    let state = Context.state ctx in
    Notify.Continue, state
  ;;

  let stop_pending_requests t =
    if t.pending_requests_stopped
    then ()
    else (
      t.pending_requests_stopped <- true;
      let to_cancel =
        Id.Table.fold t.pending ~init:[] ~f:(fun ~key:_ ~data:x acc -> x :: acc)
      in
      Id.Table.clear t.pending;
      (*to cancel somehow?*)
      to_cancel
      |> Eio.Fiber.List.iter ~max_fibers:10 (fun (prom, resolver) ->
        let res = Eio.Promise.peek prom in
        match res with
        | Some _ ->
          ()
        | None ->
          Eio.Promise.resolve resolver (Error `Stopped)))
  ;;

  let state t = t.state
  let stopped t = Eio.Promise.await (t.stopped |> fst)

  let stop t =
    (*TODO:not sure if this is actually a fork and join equiv*)
    Fiber.both (fun () -> Chan.close t.chan `Read) (fun () -> stop_pending_requests t)
  ;;

  let close t =
    Fiber.all
      [
        (fun () -> Chan.close t.chan `Read);
        (fun () -> Chan.close t.chan `Write);
        (fun () -> Promise.resolve (t.stopped |> snd) ());
        (fun () -> stop_pending_requests t);
      ]
  ;;

  let send_response t resp =
    (*TODO decide if i should fork here or not. there are pros and cons either way*)
    log t (fun () -> Log.msg "sending response" [ "response", Response.yojson_of_t resp ]);
    Chan.send t.chan [ Response resp ];
    log_s t "sent response"
  ;;

  (** throws an error if it is not running*)
  let check_running t =
    if not t.running
    then (
      log_s t "check running failed";
      failwith @@ Printf.sprintf "jsonrpc server %s is not running" t.name)
  ;;

  (** Regisetrs an Ivar that will store the response to this request*)
  let register_request_ivar t id ivar =
    match Id.Table.find_opt t.pending id with
    | Some _ ->
      failwith "duplicate request id"
    | None ->
      Id.Table.add t.pending ~key:id ~data:ivar
  ;;

  (** Await the response arriving into the ivar for this request Ivar *)
  let read_request_ivar req ivar =
    let res = Promise.await (ivar |> fst) in
    match res with
    | Ok s ->
      s
    | Error `Cancelled ->
      assert false
    | Error `Stopped ->
      raise (Stopped req)
  ;;
end