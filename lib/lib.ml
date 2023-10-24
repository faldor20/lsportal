open Base
module Response = Jsonrpc.Response
module Rpc = Jsonrpc_runner.Make (Jsonrpc_runner.Flow_io)
open Jsonrpc_runner
open Eio
open Eio.Std
module Log = Log

module LspForwarder = struct
  open Eio.Flow

  type ('a, 'b, 'c) config = {
    req_pre : ('c Rpc.t * Jsonrpc.Request.t -> unit) option;
    notif_pre : ('c Rpc.t * Jsonrpc.Notification.t -> unit) option;
    transform_req : (Jsonrpc.Request.t -> Jsonrpc.Request.t) option;
    transform_notif : (Jsonrpc.Notification.t -> Jsonrpc.Notification.t) option;
    name : string;
    io : 'a Flow.source * 'b Flow.sink;
  }

  let create_config
    ?req_pre
    ?notif_pre
    ?transform_notif
    ?transform_req
    ~(io : _ Flow.source * _ Flow.sink)
    name
    =
    { req_pre; notif_pre; name; io; transform_notif; transform_req }
  ;;

  (*TODO: Cleanup all this so that forward messages takes a type that only has those functions it needs *)

  (**Makes the source Rpc server forward messages to the destination server*)
  let forward_messages
    ?(notif_pre = fun _ -> ())
    ?(req_pre = fun _ -> ())
    ?(transform_notif = fun x -> x)
    ?(transform_req = fun x -> x)
    ~(source : _ Rpc.t)
    (dest : _ Rpc.t ref)
    =
    {
      source with
      on_request =
        (fun (slave, request) ->
          req_pre (slave, request);
          let new_request = transform_req request in
          ( (fun f -> Rpc.request !dest new_request |> f) |> Jsonrpc_runner.Reply.later,
            slave.state ));
      on_notification =
        (fun (slave, notif) ->
          notif_pre (slave, notif);
          let new_notification = transform_notif notif in
          Rpc.notification !dest new_notification;
          Jsonrpc_runner.Notify.Continue, slave.state);
    }
  ;;

  (**Makes the source Rpc server forward messages to the destination server*)
  let forward_messages_cfg { req_pre; notif_pre; transform_req; transform_notif } =
    forward_messages ?req_pre ?notif_pre ?transform_req ?transform_notif
  ;;

  let makeLspForwarder config =
    let make_fio (inp, out) = Flow_io.make inp out in
    Rpc.create ~name:config.name (make_fio config.io) "fakeState"
  ;;

  (** creates two Rpc servers that forward messages to each other*)
  let create_pair config1 config2 =
    (*We wish to make the pairs mutually referential but if we did an immutable update that will break the reference, so we need refs here instead*)
    let forwarder_a = makeLspForwarder config1 |> ref in
    let forwarder_b = makeLspForwarder config2 |> ref in
    forwarder_a := forward_messages_cfg config1 ~source:!forwarder_a forwarder_b;
    forwarder_b := forward_messages_cfg config2 ~source:!forwarder_b forwarder_a;
    !forwarder_a, !forwarder_b
  ;;
end

(**Creates a process that will communicate over the two Flows returned by this function
   @param args
     a list of args to pass to the process starting with the binary name/path eg: ["ocamllsp";"--stdio"]
   @return A Flow for the standardIn and Standardout *)
let create_process ?stderrDest ~sw ~mngr args =
  (*Create eio pipes so that we can let the process wrie in one end and return the other for us to read/write to *)
  let stdin_o, stdin_i = Eio.Process.pipe ~sw mngr in
  let stdout_o, stdout_i = Eio.Process.pipe ~sw mngr in
  let _ =
    Eio.Process.spawn ~sw mngr ~stdin:stdin_o ~stdout:stdout_i ?stderr:stderrDest args
  in
  stdout_o, stdin_i
;;

open Eio

(**Creates a pair of forwarders that forward messages to each other.
   One is a slave lsp server and the other is to communicate with the editor
   @param args the arguments to pass to the slave lsp server *)
let create ~mngr ~sw ~env ~chunk_regex args =
  let stdout, stdin = create_process ~stderrDest:(Stdenv.stderr env) ~sw ~mngr args in
  let transform_notif = Transformer.trasform_server_notifiction ~chunk_regex in
  let ls_forwarder_c =
    LspForwarder.create_config "ls_forwarder" ~transform_notif ~io:(stdout, stdin)
  in
  let editor_forwarder_c =
    LspForwarder.create_config "editor_forwarder" ~io:(Stdenv.stdin env, Stdenv.stdout env)
  in
  let ls_forwarder, editor_forwarder =
    LspForwarder.create_pair ls_forwarder_c editor_forwarder_c
  in
  ls_forwarder, editor_forwarder
;;
