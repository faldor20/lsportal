open Base
open Stdio
open Eio.Std
open Eio_main
open Lsportal.Lib
open Lsportal
open Eio
open Cmdliner

let validaiton regex args f =
  match regex, args with
  | Some regex, args when not @@ List.is_empty args ->
    f regex args
  | None, _ ->
    failwith "missing regex, must supply a regex to locate the embbeded code"
  | _ ->
    failwith "invalid arguments"
;;

let makeRegex regex =
  try regex |> Re.Pcre.re |> Re.compile with exn -> failwith "invalid regex string"
;;

open Transformer
open Transformer.Sub_doc

let lsportal cmd regex exclusion_regex exclusion_exclusion extension dd args =
  validaiton regex args @@ fun regex args ->
  Log.setup true Stdlib.Format.err_formatter;
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let mngr = Eio.Stdenv.process_mgr env in
  Log.log_s ~section:"startup"
  @@ Printf.sprintf "langserver command:'%s'\n" (args |> String.concat ~sep:" ");
  Log.log_s ~section:"startup" @@ Printf.sprintf "regex:'%s'" regex;
  Log.log_s ~section:"startup"
  @@ Printf.sprintf
       "exclusion_regex:'%s'"
       (exclusion_regex |> Option.sexp_of_t String.sexp_of_t |> Sexp.to_string_hum);
  let config =
    {
      regex = regex |> makeRegex;
      exclusion_regex = exclusion_regex |> Option.map ~f:makeRegex;
      extension;
      exclusion_exclusion = exclusion_exclusion |> Option.map ~f:makeRegex;
    }
  in
  let fw_editor, fw_ls = create ~sw ~mngr ~env ~config args in
  Fiber.both (fun () -> Rpc.run fw_editor) (fun () -> Rpc.run fw_ls)
;;

let double_dash =
  let doc = "Standalone -- argument" in
  Arg.(value & flag & info [ "" ] ~doc)
;;

let cmd =
  let doc = "The command to run to execute the language server" in
  Arg.(required & pos 0 (some string) None & info [] ~doc)
;;

let regex =
  let doc = "regex pattern to match the sections of code" in
  Arg.(value & opt (some string) None & info [ "regex" ] ~doc)
;;

let exclusion_regex =
  let doc = "regex pattern to exclude sections" in
  Arg.(value & opt (some string) None & info [ "exclusion" ] ~doc)
;;

let exclusion_exclusion =
  let doc =
    "regex pattern to exclude from the exclusion, useful for template strings with \
     interpolation inside"
  in
  Arg.(value & opt (some string) None & info [ "exclusion_exclusion" ] ~doc)
;;

let extension =
  let doc = "the file extension your language server would expect" in
  Arg.(required & opt (some string) None & info [ "extension" ] ~doc)
;;

(*
   let prefix =
   let doc = "a prefix to find befor the code alternative to the --regex option" in
   Arg.(value& opt (some string) None & info [ "prefix" ] ~doc)
   ;;
   let suffix =
   let doc = "a suffix to find after the code " in
   Arg.(value& opt (some string) None & info [ "suffix" ] ~doc)
   ;;
*)

let lspArgs = Arg.(value & pos_all string [] & info [] ~docv:"LSP_ARGS")

let cmd =
  let doc =
    "Wraps another language server allowing it to be used on a subset of a buffer"
  in
  let term =
    Term.(
      const lsportal
      $ cmd
      $ regex
      $ exclusion_regex
      $ exclusion_exclusion
      $ extension
      $ double_dash
      $ lspArgs)
  in
  let info = Cmd.info ~doc "lsportal" in
  Cmd.v info term
;;

let main () = Stdlib.exit @@ Cmdliner.Cmd.eval cmd
let () = main ()
