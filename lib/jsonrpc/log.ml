open Base
open Import
  open Stdlib

  let level : (string option -> bool) ref = ref (fun _ -> false)
  let out = ref Format.err_formatter

  type message = {
    message : string;
    payload : (string * Json.t) list;
  }

  let msg message payload = { message; payload }

  let log ?section k =
    if !level section
    then (
      let message = k () in
      (match section with
       | None ->
         Format.fprintf !out "%s@." message.message
       | Some section ->
         Format.fprintf !out "[%s] %s@." section message.message);
      (match message.payload with
       | [] ->
         ()
       | fields ->
         Format.fprintf !out "%a@." Json.pp (`Assoc fields));
      Format.pp_print_flush !out ())
  ;;

  let log_s ?section s = log ?section (fun x -> msg s [])

  let setup on printer =
    (if on then level := fun _ -> true);
    out := printer
  ;;
