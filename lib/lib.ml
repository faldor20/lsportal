open Base
module Response = Jsonrpc.Response
module Rpc = Jsonrpc_fiber.Make (Fiber_io)
open Jsonrpc_fiber
open Eio
open Eio.Std

module LspForwarder = struct
  open Eio.Flow

  type ('a, 'b, 'c) config =
    { req_pre: ('c Rpc.t * Jsonrpc.Request.t -> unit) option
    ; notif_pre: ('c Rpc.t * Jsonrpc.Notification.t -> unit) option
    ; name: string
    ; io: 'a Flow.source * 'b Flow.sink }

  let create_config ?req_pre ?notif_pre ~(io : _ Flow.source * _ Flow.sink) name
      =
    {req_pre; notif_pre; name; io}

  (**Makes the source Rpc server forward messages to the destination server*)
  let forward_messages ?(notif_pre = fun _ -> ()) ?(req_pre = fun _ -> ())
      ~(source : _ Rpc.t) (dest : _ Rpc.t ref) =
    { source with
      on_request=
        (fun (slave, request) ->
          req_pre (slave, request) ;
          ( (fun f -> Rpc.request !dest request |> f)
            |> Jsonrpc_fiber.Reply.later
          , slave.state ) )
    ; on_notification=
        (fun (slave, notif) ->
          notif_pre (slave, notif) ;
          Rpc.notification !dest notif ;
          (Jsonrpc_fiber.Notify.Continue, slave.state) ) }

  (**Makes the source Rpc server forward messages to the destination server*)
  let forward_messages_cfg cfg =
    forward_messages ?req_pre:cfg.req_pre ?notif_pre:cfg.notif_pre

  let makeLspForwarder config =
    let make_fio (inp, out) = Fiber_io.make inp out in
    Rpc.create ~name:config.name (make_fio config.io) "fakeState"

  (** creates two Rpc servers that forward messages to each other*)
  let create_pair config1 config2 =
    (*We wish to make the pairs mutually referential but if we did an immutable update that will break the reference, so we need refs here instead*)
    let forwarder_a = makeLspForwarder config1 |> ref in
    let forwarder_b = makeLspForwarder config2 |> ref in
    forwarder_a := forward_messages_cfg config1 ~source:!forwarder_a forwarder_b ;
    forwarder_b := forward_messages_cfg config2 ~source:!forwarder_b forwarder_a ;
    (!forwarder_a, !forwarder_b)
end

let create_process ~sw ~mngr ~env ~bin =
  let bin = Fpath.to_string bin in
  (*
      let stdin_i, stdin_o = Unix.pipe ~cloexec:true () in
      let stdout_i, stdout_o = Unix.pipe ~cloexec:true () in

*)
  let stdin_o, stdin_i = Eio.Process.pipe ~sw mngr in
  let stdout_o, stdout_i = Eio.Process.pipe ~sw mngr in
  (*
    let open Eio.Path in
    Eio.Path.open_out ~sw ~create:(`Or_truncate 0o600)
      (Stdenv.cwd env / "lsp_log.log")
*)
  let pid =
    Eio.Process.spawn ~sw mngr ~executable:bin ~stdin:stdin_o ~stdout:stdout_i
      ~stderr:(Stdenv.stderr env) [bin; "--stdio"]
  in
  let res = (pid, stdout_o, stdin_i) in
  res

open Eio

let create mngr ~sw ~(slaveBin : Fpath.t) env =
  let _, stdout, stdin = create_process ~sw ~mngr ~env ~bin:slaveBin in
  let ls_forwarder_c =
    LspForwarder.create_config "ls_forwarder" ~io:(stdout, stdin)
  in
  let editor_forwarder_c =
    LspForwarder.create_config "editor_forwarder"
      ~io:(Stdenv.stdin env, Stdenv.stdout env)
  in
  let ls_forwarder, editor_forwarder =
    LspForwarder.create_pair ls_forwarder_c editor_forwarder_c
  in
  (ls_forwarder, editor_forwarder)
