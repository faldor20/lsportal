open Eio

open Jsonrpc
open Import

module Make(Chan:Io_types.Packet_Channel)= struct
  include Core.Core(Chan)

  (** Handles Running the rpc connection and processing oncoming and outgoing messages*)
    type nextAction =
      | Stop
      | Loop (**Start the communication with the json rpc channel*)

    (* The main loop, consumes a single message from the cannel and then prcesses it. Then eventually we call loop again*)
    let on_response ~sw t (r : Response.t) =
      let log (what : string) =
        log t (fun () ->
          Log.msg ("response: " ^ what) [ "id", r.id |> Jsonrpc.Id.yojson_of_t ])
      in
      log_s t "got response";
      match Id.Table.find_opt t.pending r.id with
      | None ->
        log "dropped";
        ()
      | Some ivar ->
        log "acknowledged";
        Id.Table.remove t.pending r.id;
        let resp = Promise.peek (ivar |> fst) in
        (match resp with Some _ -> () | None -> Promise.resolve (ivar |> snd) (Ok r))
    ;;

    let on_request ~sw t (r : Request.t) =
      log t (fun () -> Log.msg "handling request " [ "method:", `String r.method_ ]);
      let result =
        let sent = ref false in
        try Ok (t.on_request (t, r)) with
        | exn ->
          if !sent
          then (* TODO log *)
            Error ()
          else (
            let response = response_of_exn r.id exn in
            sent := true;
            Fiber.fork ~sw (fun () -> send_response t response);
            Error ())
      in
      log t (fun () -> Log.msg "received result" []);
      match result with
      | Error () ->
        Loop
      | Ok (reply, state) ->
        t.state <- state;
        let sender = Sender.make r.id (send_response t) in
        (try Reply.send ~sw reply sender with
         | exn ->
           if sender.called
           then
             (* TODO we should log *)
             log t (fun () -> Log.msg "Sender was called and threw" [])
           else (
             let resp = response_of_exn r.id exn in
             Sender.send sender resp));
        Loop
    ;;

    let on_notification t (r : Notification.t) : nextAction (*FIber*) =
      log t (fun () -> Log.msg "got notification " [ "method", `String r.method_ ]);
      let res = try Ok (t.on_notification (t, r)) with exn -> Error exn in
      match res with
      | Ok (next, state) ->
        t.state <- state;
        (match next with Stop -> Stop | Continue -> Loop)
      | Error errors ->
        Stdlib.Format.eprintf
          "Uncaught error when handling notification:@.%a@.Error:@.%s@."
          Json.pp
          (Notification.yojson_of_t r)
          (errors |> Exn.to_string);
        Loop
    ;;

    let run t =
      (*We make a new switch so we can terminate all these commands*)
      Switch.run
      @@ fun sw ->
      let rec loop ~sw t =
        let next : nextAction =
          t.tick <- t.tick + 1;
          log t (fun () -> Log.msg "new tick" [ "tick", `Int t.tick ]);
          let res = Chan.recv t.chan in
          log t (fun () -> Log.msg "waited for something" []);
          
          match res with
          | None ->
            log t (fun () -> Log.msg "recieved nothing from channel. closing server" []);
            Stop
          | Some packet ->
            (*Now we have a packet we use one of the functions defined below to handle it*)
            (match packet with
             | Notification r ->
               on_notification t r
             | Request r ->
               on_request ~sw t r
             | Response r ->
               let () = Fiber.fork ~sw (fun () -> on_response ~sw t r) in
               Loop
             | Batch_call _ ->
               failwith "batch requests aren't supported"
             | Batch_response _ ->
               assert false)
        in
        (*Now the loop is finished and we dicide whether to continue*)
        match next with Loop -> loop ~sw t | Stop -> ()
      in
      Fiber.fork ~sw (fun () ->
        log_s t "Rpc started";
        t.running <- true;
        loop ~sw t;
        (*stop any tasks still running*)
        Switch.fail sw (Done ("Stopped running  " ^ t.name));
        close t)
    ;;
  end