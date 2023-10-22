open Jsonrpc
open Eio
open Base

exception Done of string

module Json = struct
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
        `Assoc (List.map r ~f:(fun (k, v) -> (k, of_dyn v)))
    | Variant (name, args) ->
        `Assoc [(name, of_dyn (List args))]
    | Set xs ->
        `List (List.map ~f:of_dyn xs)
    | Map map ->
        `List (List.map map ~f:(fun (k, v) -> `List [of_dyn k; of_dyn v]))
end

module Log = struct
  open Stdlib

  let level : (string option -> bool) ref = ref (fun _ -> false)

  let out = ref Format.err_formatter

  type message = {message: string; payload: (string * Json.t) list}

  let msg message payload = {message; payload}

  let log ?section k =
    if !level section then (
      let message = k () in
      ( match section with
      | None ->
          Format.fprintf !out "%s@." message.message
      | Some section ->
          Format.fprintf !out "[%s] %s@." section message.message ) ;
      ( match message.payload with
      | [] ->
          ()
      | fields ->
          Format.fprintf !out "%a@." Json.pp (`Assoc fields) ) ;
      Format.pp_print_flush !out () )

  let setup on printer =
    (if on then level := fun _ -> true) ;
    out := printer
end
(*End of extra stuff i added to make this work*)

module Id = struct
  include Id
  module Table = Stdlib.MoreLabels.Hashtbl.Make (Id)
end

module Notify = struct
  type t = Stop | Continue
end

module Sender = struct
  type t = {mutable called: bool; for_: Id.t; send: Response.t -> unit (*FIber*)}

  let make id send = {for_= id; called= false; send}

  let send t (r : Response.t) : unit  =
    if t.called then failwith "cannot send response twice"
    else if not (Id.equal t.for_ r.id) then failwith "invalid id"
    else t.called <- true ;
    t.send r
end

exception Stopped of Request.t

let () =
  Stdlib.Printexc.register_printer (function
    | Stopped req ->
        let json = Request.yojson_of_t req in
        Some
          ( "Session closed. Request will not be answered. "
          ^ Json.to_pretty_string json )
    | _ ->
        None )

module Reply = struct
  type t =
    | Now of Response.t
    | Later of ((Response.t -> unit ) -> unit )

  let now (r : Response.t) = Now r

  (*used if you would like to send the response at a later date. though I'm not sure i get why *)
  let later f = Later f

  let send (t : t) ~sw sender =
    match t with
    | Now r ->
        Sender.send sender r
    | Later f ->
      Fiber.fork ~sw (fun()->
        f (fun (r : Response.t) -> Sender.send sender r)
  );
end

module Make (Chan : sig
  type t

  val send : t -> Packet.t list -> unit (*FIber*)

  val recv : t -> Packet.t option (*FIber*)

  val close : t -> [`Read | `Write] -> unit (*FIber*)
end) =
struct
  type 'a prom = 'a Eio.Promise.t * 'a Eio.Promise.u

  type 'state t =
    { chan: Chan.t
    ; on_request: ('state, Request.t) context -> Reply.t * 'state (*FIber*)
    ; on_notification: ('state, Notification.t) context -> Notify.t * 'state
          (*FIber*)
    ; pending: (Response.t, [`Stopped | `Cancelled]) Result.t prom Id.Table.t
    ; stopped: unit prom
    ; name: string
    ; mutable running: bool
    ; mutable tick: int
    ; mutable state: 'state
    ; mutable pending_requests_stopped: bool }

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

  let log_s t s = Log.log ~section:t.name (fun x -> Log.msg s [])

  let response_of_exn id exn =
    let error =
      match exn with
      | Jsonrpc.Response.Error.E resp ->
          resp
      | e ->
          let data = `String (exn |> Exn.to_string) in
          Response.Error.make ~code:InternalError ~data
            ~message:"uncaught exception" ()
    in
    Response.error id error

  let on_request_fail ctx : Reply.t * _ =
    (*FIber*)
    let req : Request.t = Context.message ctx in
    let state = Context.state ctx in
    let error =
      Response.Error.make ~code:InternalError ~message:"not implemented" ()
    in
    (Reply.now (Response.error req.id error), state)

  let state t = t.state

  let on_notification_fail ctx =
    let state = Context.state ctx in
    (Notify.Continue, state)

  let stop_pending_requests t =
    if t.pending_requests_stopped then ()
    else (
      t.pending_requests_stopped <- true ;
      let to_cancel =
        Id.Table.fold t.pending ~init:[] ~f:(fun ~key:_ ~data:x acc ->
            x :: acc )
      in
      Id.Table.clear t.pending ;
      (*to cancel somehow?*)
      to_cancel
      |> Eio.Fiber.List.iter ~max_fibers:10 (fun (prom, resolver) ->
             let res = Eio.Promise.peek prom in
             match res with
             | Some _ ->
                 ()
             | None ->
                 Eio.Promise.resolve resolver (Error `Stopped) ) )

  (**Creates a Rpc conncection over the provided channel. 
  the on_request handler will trigger on recieving a request 
  the on_notifiction handler will be called on recieving a notifiction
  if you don't register a handler they will throw an error 
  (I think this is for if your rpc connection is just supposed to be a client.
   that way it will throw if it recieves a message that should be meant for a server)*)
  let create ?(on_request = on_request_fail)
      ?(on_notification = on_notification_fail) ~name chan state =
    let pending = Id.Table.create 10 in
    { chan
    ; on_request
    ; on_notification
    ; pending
    ; stopped= Eio.Promise.create ()
    ; name
    ; running= false
    ; tick= 0
    ; state
    ; pending_requests_stopped= false }

  let stopped t = Eio.Promise.await (t.stopped |> fst)

  let stop t =
    (*TODO:not sure if this is actually a fork and join equiv*)
    Fiber.both
      (fun () -> Chan.close t.chan `Read)
      (fun () -> stop_pending_requests t)

  let close t =
    Fiber.all
      [ (fun () -> Chan.close t.chan `Read)
      ; (fun () -> Chan.close t.chan `Write)
      ; (fun () -> Promise.resolve (t.stopped |> snd) ())
      ; (fun () -> stop_pending_requests t) ]

  type nextAction =
    | Done
    | Loop  (**Start the communication with the json rpc channel*)

  let run t =
    (*We make a new switch so we can terminate all these commands*)
    Switch.run
    @@ fun later ->
    let send_response resp =
(*TODO decide if i should fork here or not. there are pros and cons either way*)          
      log t (fun () ->
          Log.msg "sending response" [("response", Response.yojson_of_t resp)] ) ;
      Chan.send t.chan [Response resp];
      log_s t "sent response"
    in
    (* The main loop, consumes a single message from the cannel and then prcesses it. Then eventually we call loop again*)
    let rec loop () =
      let next : nextAction =
        t.tick <- t.tick + 1 ;
        log t (fun () -> Log.msg "new tick" [("tick", `Int t.tick)]) ;
        let res = Chan.recv t.chan in
        log t (fun () -> Log.msg "waited for something" []) ;
        match res with
        | None ->
            log t (fun () ->
                Log.msg "recieved nothing from channel. closing server" [] ) ;
            Done
        | Some packet -> (
          (*Now we have a packet we use one of the functions defined below to handle it*)
          match packet with
          | Notification r ->
              on_notification r
          | Request r ->
              on_request r
          | Response r ->
              let () = Fiber.fork ~sw:later (fun () -> on_response r) in
              Loop
          | Batch_call _ ->
              failwith "batch requests aren't supported"
          | Batch_response _ ->
              assert false )
      in
      (*Now the loop is finished and we dicide whether to continue*)
      match next with Loop -> loop () | Done -> ()
    and on_response r =
      let log (what : string) =
        log t (fun () ->
            Log.msg ("response: " ^ what) [("id", r.id|>Jsonrpc.Id.yojson_of_t)] )
      in
      log_s t "got response" ;
      match Id.Table.find_opt t.pending r.id with
      | None ->
          log "dropped" ; ()
      | Some ivar -> (
          log "acknowledged" ;
          Id.Table.remove t.pending r.id ;
          let resp = Promise.peek (ivar |> fst) in
          match resp with
          | Some _ ->
              ()
          | None ->
              Promise.resolve (ivar |> snd) (Ok r) )
    and on_request (r : Request.t) =
      log t (fun () -> Log.msg "handling request " ["method:",`String r.method_]) ;
      let result =
        let sent = ref false in
        try Ok (t.on_request (t, r))
        with exn ->
          if !sent then (* TODO log *)
            Error ()
          else
            let response = response_of_exn r.id exn in
            sent := true ;
            Fiber.fork ~sw:later (fun () -> send_response response) ;
            Error ()
      in
      log t (fun () -> Log.msg "received result" []) ;
      match result with
      | Error () ->
          Loop
      | Ok (reply, state) ->
          t.state <- state ;
          let sender = Sender.make r.id send_response in
          ( try Reply.send ~sw:later reply sender
            with exn ->
              if sender.called then
                (* TODO we should log *)
                log t (fun () -> Log.msg "Sender was called and threw" [])
              else
                let resp = response_of_exn r.id exn in
                Sender.send sender resp ) ;
          Loop
    and on_notification (r : Notification.t) : nextAction (*FIber*) =
      log t (fun ()-> Log.msg "got notification " [("method",`String r.method_)]);
      let res = try Ok (t.on_notification (t, r)) with exn -> Error exn in
      match res with
      | Ok (next, state) -> (
          t.state <- state ;
          match next with Stop -> Done | Continue -> Loop )
      | Error errors ->
          Stdlib.Format.eprintf
            "Uncaught error when handling notification:@.%a@.Error:@.%s@."
            Json.pp
            (Notification.yojson_of_t r)
            (Dyn.to_string
               (Dyn.list (fun _ -> Dyn.string "not keeping errors") [errors]) ) ;
          Loop
    in
    Fiber.fork ~sw:later (fun () ->
        log_s t "Rpc started" ;
        t.running <- true ;
        loop () ;
        (*stop any tasks still running*)
        Switch.fail later (Done ("Stopped running  " ^ t.name)) ;
        close t )

  (** throws an error if it is not running*)
  let check_running t =
    if not t.running then( log_s t "check running failed" ;
    failwith @@ Printf.sprintf "jsonrpc server %s is not running" t.name
    )

  (** Useses this Rpc connection to send a notifiction out via the output stream*)
  let notification t (n : Notification.t) =
    check_running t ;
    Chan.send t.chan [Notification n]

  (** Regisetrs an Ivar that will store the response to this request*)
  let register_request_ivar t id ivar =
    match Id.Table.find_opt t.pending id with
    | Some _ ->
        failwith "duplicate request id"
    | None ->
        Id.Table.add t.pending ~key:id ~data:ivar

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

  (** Useses this Rpc connection to send a request out via the output stream*)
  let request t (req : Request.t) =
    check_running t ;
    Chan.send t.chan [Request req] ;
    let ivar = Promise.create () in
    register_request_ivar t req.id ivar ;
    read_request_ivar req ivar

  let request_with_cancel t (req : Request.t) =
    let ivar = Promise.create () in
    let cancel = Promise.resolve (ivar |> snd) (Error `Cancelled) in
    let resp =
      check_running t ;
      Chan.send t.chan [Request req] ;
      register_request_ivar t req.id ivar ;
      let res = Promise.await (ivar |> fst) in
      match res with
      | Ok s ->
          `Ok s
      | Error `Cancelled ->
          `Cancelled
      | Error `Stopped ->
          raise (Stopped req)
    in
    (cancel, resp)

  module Batch = struct
    type response =
      Jsonrpc.Request.t
      * (Jsonrpc.Response.t, [`Stopped | `Cancelled]) Result.t prom

    type t = [`Notification of Notification.t | `Request of response] list ref

    let await (req, resp) = read_request_ivar req resp

    let create () = ref []

    (** Useses this Rpc connection to send a notifiction out via the output stream*)
    let notification t n = t := `Notification n :: !t

    (** Useses this Rpc connection to send a request out via the output stream*)
    let request (t : t) r : response =
      let ivar = Promise.create () in
      let resp = (r, ivar) in
      t := `Request resp :: !t ;
      resp
  end

  let submit (t : _ t) (batch : Batch.t) =
    check_running t ;
    let pending = !batch in
    batch := [] ;
    let pending, ivars =
      List.fold_left pending ~init:([], []) ~f:(fun (pending, ivars) -> function
        | `Notification n ->
            (Jsonrpc.Packet.Notification n :: pending, ivars)
        | `Request ((r : Request.t), ivar) ->
            (Jsonrpc.Packet.Request r :: pending, (r.id, ivar) :: ivars) )
    in
    List.iter ivars ~f:(fun (id, ivar) -> register_request_ivar t id ivar) ;
    Chan.send t.chan pending
end
