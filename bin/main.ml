open Base
open Stdio
open Eio.Std
open Eio_main
open Lsportal.Lib
open Lsportal
open Eio

let main () =
  Eio_main.run
  @@ fun env ->
  let mngr = Eio.Stdenv.process_mgr env in
  Switch.run
  @@ fun sw ->
  (*
  Fiber.fork_daemon ~sw (fun () ->
      let log =
        Eio.Path.open_out ~sw ~create:(`Or_truncate 0o600)
          (Stdenv.cwd env / "log.log")
      in
      Buf_write.with_flow log (fun writer ->
          Lsportal.Jsonrpc_fiber.Log.setup true
            (Stdlib.Format.make_formatter
               (fun x i k -> Flow.write log [Cstruct.of_string x])
               (fun () -> Buf_write.flush writer) ) ;
          Eio.Fiber.await_cancel () ) ;
      failwith "log writer daemon stopped" ) ;*)
  
  (*TODO: I need to forward the content length message to the clients*)
  Lsportal.Jsonrpc_fiber.Log.setup true Stdlib.Format.err_formatter;
  let ls_bin=( "/home/eli/.nix-profile/bin/typescript-language-server"
        |> Fpath.of_string |> Result.ok |> Option.value_exn) in
  (*
  let _, stdout, stdin =
    create_process ~sw ~mngr
      ~bin:
         )
  in

  let slave = Rpc.create ~name:"slave" (Fiber_io.make stdout stdin) "state" in

  let server =
    configureLspForwarder "forwarder" (Stdenv.stdout env) (Stdenv.stdin env)
      (fun (server, req) -> Rpc.request slave req)
      (fun (server, notif) -> Rpc.notification slave notif)
  in
  (*setup the slave client to forward any requests it gets via the forwarder*)
  let slave =
    { slave with
      on_request=
        (fun (slave, request) ->
          (Rpc.request server request |> Jsonrpc_fiber.Reply.now, slave.state)
          )
    ; on_notification=
        (fun (slave, notif) ->
          Rpc.notification server notif ;
          (Jsonrpc_fiber.Notify.Continue, slave.state) ) }
  in
  *)
  let fw_editor,fw_ls=create ~sw mngr ~slaveBin:ls_bin env in 
  Fiber.both (fun () -> Rpc.run fw_editor) (fun () -> Rpc.run fw_ls)


let () = main ()
