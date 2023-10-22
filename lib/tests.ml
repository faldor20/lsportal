open Base
open Eio
open Lib
open Jsonrpc.Request

let%expect_test "sub" =
  let b = Cf_slice.of_subarray 1 2 [|1; 2; 3; 4; 5|] in
  Stdio.printf "%s:"
  @@ Sexp.to_string (Cf_slice.sexp_of_t (Array.sexp_of_t Int.sexp_of_t) b) ;
  [%expect {| ((vector(1 2 3 4 5))(start 1)(limit 2)): |}]

exception Done of string
(*
let make_connected_pair name1 name2 sw mgr =
  let p1i, p1o = Eio.Process.pipe ~sw mgr in
  let p2i, p2o = Eio.Process.pipe ~sw mgr in
  let log = traceln "[%s] %s" in
  let log_request name method_ =
    log name @@ "got request with method " ^ method_
  in
  let log_notification name method_ =
    log name @@ "got notifictaion with method " ^ method_
  in
  let s1 =
    configureLspForwarder name1 p2o p1i
      (fun (_, req) ->
        log_request name1 req.method_ ;
        Response.ok req.id (`String "hi") )
      (fun (_, notif) -> log_notification name1 notif.method_)
  in
  let s2 =
    configureLspForwarder "forwarder" p1o p2i
      (fun (_, req) ->
        log_request name2 req.method_ ;
        Response.ok req.id (`String ("hi. method:" ^ req.method_)) )
      (fun (_, notif) -> log_notification name2 notif.method_)
  in
  (s1, s2)
*)

(** This sets up two connected RPC servers and then returns flows to write to their in and out streams.
The forwarder   *)
let make_forwarder_pair a_name b_name sw mgr =
  (*setup the intput and output flows*)
  let b_input_o, b_input_i = Eio.Process.pipe ~sw mgr in
  let b_out_o, b_out_i = Eio.Process.pipe ~sw mgr in
  let a_in_o, a_in_i = Eio.Process.pipe ~sw mgr in
  let a_out_o, a_out_i = Eio.Process.pipe ~sw mgr in
  (* make some logging helper functions*)
  let log = traceln "{%s} %s" in
  let log_request name method_ =
    log name
    @@ Printf.sprintf "got request with method '%s' forwarding on" method_
  in
  let log_notification name method_ =
    log name @@ "got notifictaion with method " ^ method_
  in
  (* make config for the forwarders*)
  let a_forwarder_c =
    LspForwarder.create_config
      ~req_pre:(fun (_, req) -> log_request a_name req.method_)
      ~notif_pre:(fun (_, notif) -> log_notification a_name notif.method_)
      "ls_forwarder" ~io:(a_in_o, a_out_i)
  in
  let b_forwarder_c =
    LspForwarder.create_config "editor_forwarder"
      ~req_pre:(fun (_, req) -> log_request b_name req.method_)
      ~notif_pre:(fun (_, notif) -> log_notification b_name notif.method_)
      ~io:(b_input_o, b_out_i)
  in
  (* create the linked forwarders*)
  let a_forwarder, b_forwarder =
    LspForwarder.create_pair a_forwarder_c b_forwarder_c
  in
  (a_forwarder, b_forwarder, (b_out_o, b_input_i), (a_out_o, a_in_i))

let make_echo_server name ~sw ~clock (inp, out) =
  Rpc.create
    ~on_request:(fun (server, req) ->
      traceln "%s : got request %s" name req.method_ ;
      if String.( = ) req.method_ "slow" then
        ( (fun f ->
            Time.sleep clock 2.0 ;
            Jsonrpc.Response.ok req.id
              (`String
                (Printf.sprintf "Hi from %s. Your method: %s" name req.method_)
                )
            |> f )
          |> Jsonrpc_fiber.Reply.Later
        , server.state )
      else
        ( Jsonrpc.Response.ok req.id
            (`String
              (Printf.sprintf "Hi from %s. Your method: %s" name req.method_) )
          |> Jsonrpc_fiber.Reply.Now
        , server.state ) )
    ~on_notification:(fun (server, notif) ->
      traceln "%s_echoer: Got notification: '%s'" name notif.method_ ;
      (Jsonrpc_fiber.Notify.Continue, server.state) )
    ~name (Fiber_io.make inp out) "state"

(**Makes 4 connected servers an editor and lang_server that echo out messages, and one forwarder for each, which forward messages to each other and are linked to their respective echo server via a pipe *)
let make_connected_servers ~sw ~clock mgr =
  let fw_editor, fw_ls, editor_io, ls_io =
    make_forwarder_pair "fw_editor" "fw_lsp" sw mgr
  in
  let editor = make_echo_server ~sw ~clock "editor" editor_io in
  let lang_server = make_echo_server ~sw ~clock "langServer" ls_io in
  Fiber.fork_daemon ~sw (fun _ ->
      try
        Switch.run (fun sw ->
            Fiber.all
              [ (fun () -> Rpc.run editor)
              ; (fun () -> Rpc.run fw_editor)
              ; (fun () -> Rpc.run fw_ls)
              ; (fun () -> Rpc.run lang_server) ] ;
            `Stop_daemon )
      with
      | Eio.Exn.Multiple exns ->
          `Stop_daemon
      | exn ->
          traceln "%s" @@ Printexc.to_string exn ;
          `Stop_daemon ) ;
  (editor, lang_server)

let traceLogs () =
  (Jsonrpc_fiber.Log.level := fun x -> true) ;
  Jsonrpc_fiber.Log.out := Stdlib.Format.err_formatter

let send_test_request rpc ~sw ~id method_ =
  Fiber.fork ~sw (fun () ->
      traceln "sending a request" ;
      let response =
        Rpc.request rpc
          (Jsonrpc.Request.create
             ~params:(`List [`String "myParam"])
             ~id:(`Int id) ~method_ () )
      in
      match response.result with
      | Ok res ->
          traceln "editor_response: %s"
          @@ Jsonrpc_fiber.Json.to_pretty_string res
      | Error err ->
          traceln "editor_response_error: err: %s" err.message )

let send_test_notification rpc method_ =
  traceln "sending a notification" ;
  Rpc.notification rpc
    (Jsonrpc.Notification.create
       ~params:(`List [`String "myParam"])
       ~method_ () )

let%expect_test "dual servers" =
  traceLogs();
  Eio_main.run (fun env ->

      Switch.run (fun sw ->
          let clock = Eio.Stdenv.clock env in
          let editor, lang_server =
            make_connected_servers ~sw ~clock (Eio.Stdenv.process_mgr env)
          in
          traceln "===== test 1===== sending from editor" ;
          send_test_request editor ~sw ~id:1 "hi from editor" ;
          traceln "===== test 1===== sending from slow" ;
          send_test_request editor ~sw ~id:100 "slow" ;
          traceln "===== test 1.5===== sending from editor again" ;
          send_test_request editor ~sw ~id:11 "hi from editor2" ;
          traceln "===== test 1.5===== sending from editor again2" ;
          send_test_request editor ~sw ~id:12 "hi from editor3" ;
          traceln "===== test 2===== sending from langserver" ;
          send_test_request lang_server ~sw ~id:2 "hi from langServer" ;
          traceln "===== test 3===== notification from editor" ;
          send_test_notification editor "notified! from_editor" ;
          traceln "===== test 4===== notification from editor" ;
          send_test_notification editor "notified_again_from_editor" ;
          traceln "===== test 4===== notification from langserver" ;
          send_test_notification lang_server "notified_from server" ;
          Fiber.fork_daemon ~sw (fun () ->
              Eio.Time.sleep clock 3.0 ;
              Rpc.stop lang_server ;
              Rpc.stop editor ;
              `Stop_daemon ) ) ) ;
  [%expect
    {|
    +===== test 1===== sending from editor
    +sending a request
    +[fw_lsp] got request with method 'hi from editor' forwarding on
    +langServer : got request hi from editor
    +editor_response: "Hi from langServer. Your method: hi from editor"
    +===== test 1.5===== sending from editor again
    +sending a request
    +[fw_lsp] got request with method 'hi from editor' forwarding on
    +langServer : got request hi from editor
    +editor_response: "Hi from langServer. Your method: hi from editor"
    +===== test 2===== sending from langserver
    +sending a request
    +[fw_editor] got request with method 'hi from langServer' forwarding on
    +editor : got request hi from langServer
    +editor_response: "Hi from editor. Your method: hi from langServer"
    +===== test 3===== notification from editor
    +sending a notification
    +[fw_lsp] got notifictaion with method notified! from_editor
    +===== test 4===== notification from editor
    +sending a notification
    +langServer_echoer: Got notification: 'notified! from_editor'
    +===== test 4===== notification from langserver
    +sending a notification
    +[fw_lsp] got notifictaion with method notified_again_from_editor
    +[fw_editor] got notifictaion with method notified_from server
    +langServer_echoer: Got notification: 'notified_again_from_editor'
    +editor_echoer: Got notification: 'notified_from server'
    +exception reading lineCancelled: Stdlib.Exit
    +exception reading lineCancelled: Stdlib.Exit
    +exception reading lineCancelled: Stdlib.Exit
    +exception reading lineCancelled: Stdlib.Exit |}]

(*
let%expect_test "rpc_works" =
  Eio_main.run
  @@ fun env ->
  let mngr = Eio.Stdenv.process_mgr env in
  try
    Switch.run (fun sw ->
        let clock = Eio.Stdenv.clock env in
        (*TODO: I need to forward the content length message to the clients*)
        let _, stdout, stdin =
          create_process ~sw ~mngr
            ~bin:
              ( "/home/eli/.nix-profile/bin/vscode-html-language-server"
              |> Fpath.of_string |> Result.ok |> Option.value_exn )
        in
        let slave =
          Rpc.create ~name:"slave" (Fiber_io.make stdout stdin) "state"
        in
        let stdin_o, stdin_i = Eio.Process.pipe ~sw mngr in
        let stdout_o, stdout_i = Eio.Process.pipe ~sw mngr in
        let editor =
          Rpc.create ~name:"editor" (Fiber_io.make stdout_o stdin_i) "state"
        in
        (*let msg="Content-Length: 28\n\n{\"jsonrpc\":\"2.0\",\"id\":1} "in
  *)
        let server =
          configureLspForwarder "server" stdout_i stdin_o
            (fun (_, req) ->
              traceln "%s"
                ( req |> Jsonrpc.Request.yojson_of_t
                |> Jsonrpc_fiber.Json.to_pretty_string ) ;
              Rpc.request slave req )
            (fun (_, notif) ->
              traceln "%s"
                ( notif |> Jsonrpc.Notification.yojson_of_t
                |> Jsonrpc_fiber.Json.to_pretty_string ) ;
              Rpc.notification slave notif )
        in
        (*
  I need to write a mock initialisation request using the editor client. or i can get rid of editor and just write canned data into Flows
  *)
        Fiber.fork ~sw (fun () ->
            Eio.Time.sleep clock 0.5 ;
            Switch.fail sw (Failure "timeout of switch") ) ;
        Rpc.run server )
  with Eio.Cancel.Cancelled _ -> traceln "success " ; [%expect {|
  |}]
*)
