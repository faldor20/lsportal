open Eio
open Base
open Lsp
open Lsp.Types

let make_did_open_notifiction text =
  Jsonrpc.Notification.create
    ~method_:"textDocument/didOpen"
    ~params:
      (`Assoc
        [
          ( "textDocument",
            `Assoc
              [
                "uri", `String "file:///test.txt";
                "languageId", `String "typescript";
                "version", `Int 1;
                "text", `String text;
              ] );
        ])
    ()
;;

(**if override is set to true the text will override it's lenght of text starting from the provided position *)
let make_change_event ?end_pos ?(override = false) (s_char, s_line) text =
  let open Lsp.Types in
  (*If the end isn't provided we assum it's at the same point  as *)
  let e_char, e_line =
    end_pos
    |> Option.value_or_thunk ~default:(fun () ->
      if not override
      then s_char, s_line
      else (
        let lines = String.split_lines text in
        let line_count = lines |> List.length in
        (* we subtract one from the length because for inserting new content if we were adding a single char the positions would be the same*)
        let chars =
          if line_count = 1
          then (
            traceln "line_count: %i" line_count;
            traceln "s_chars: %i" s_char;
            let text_len = text |> String.length in
            traceln "text_len: %i" text_len;
            s_char + text_len - 1)
          else lines |> List.last_exn |> String.length
        in
        traceln "chars: %i" chars;
        chars, s_line + line_count - 1))
  in
  let change_range =
    Range.create
      ~end_:(Position.create ~character:e_char ~line:e_line)
      ~start:(Position.create ~character:s_char ~line:s_line)
  in
  let document_change_event =
    Lsp.Types.TextDocumentContentChangeEvent.create ~range:change_range ~text ()
  in
  document_change_event
;;

let make_did_change_notifiction ?(version = 2) uri position text =
  let document_change_event = make_change_event position text in
  Lsp.Client_notification.TextDocumentDidChange
    (Lsp.Types.DidChangeTextDocumentParams.create
       ~contentChanges:[ document_change_event ]
       ~textDocument:{ version; uri })
  |> Lsp.Client_notification.to_jsonrpc
;;
