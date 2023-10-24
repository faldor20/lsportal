open Eio.Std
open Jsonrpc
open Import
open Lsp
open Base

(*
   Goals:

https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didOpen
intercept the textDocument/didOpen notification

interface DidOpenTextDocumentParams {
	/**
	 * The document that was opened.
	 */
	textDocument: TextDocumentItem;
}
we can read the textDocument and parse out our chunks
then we generate a guid for each chunk
then we send a new open notification for each chunk

2. 
We need to intercept the save command and send a save notification for each chunk

interface TextDocumentItem {
	/**
	 * The text document's URI.
	 */
	uri: DocumentUri;

	/**
	 * The text document's language identifier.
	 */
	languageId: string;

	/**
	 * The version number of this document (it will increase after each
	 * change, including undo/redo).
	 */
	version: integer;

	/**
	 * The content of the opened text document.
	 */
	text: string;
}

3.We need
*)

module TextChunk = struct
  (** text Chunk source*)
  type chunkSource = {
    uri : string;
    substring : Lsp.Private.Substring.t;
    range : int * int;
  }

  (**A chunk of text that *)
  type t = {
    uri : string; (** The Uri we save*)
    version : int;
      (** The version number of this document (it will increase after each
          change, including undo/redo).

          This could end up out of sync with the parent document

          TODO: is that an issue? *)
    languageId : string; (**This id will match an id from the lsp spec*)
    text : string;
    source : chunkSource; (** Where the source text *)
  }

  type chunkRule = { regex : Re.re }

  let get_chunks text chunk_rule =
    let matches = Re.all chunk_rule.regex text in
    matches
    |> List.map ~f:(fun m ->
      let pos, end_pos = Re.Group.offset m 1 in
      let len = end_pos - pos in
      let substring = Lsp.Private.Substring.of_slice text ~pos ~len in
      { uri = ""; substring; range = pos, len })
  ;;

  (**Takes a byte sequence of a string and converts any char within the range to a space*)
  let whitespacify ~pos ~len bytes =
    for i = 0 to len - 1 do
      let byte = Bytes.unsafe_get bytes (pos + i) in
      if Char.( = ) byte '\n' then () else Bytes.unsafe_set bytes (pos + i) ' '
    done
  ;;
end

open TextChunk

(**takes a list of substrings and the original text and creates a new text that replaces all text outside the substrings with whitespace*)
let generate_isolated_doc text (substrings : chunkSource list) =
  (* TODO:Check this all for off by one errors *)
  let bytes = Bytes.of_string text in
  let last_pos, last_len =
    List.fold
      ~init:(0, 0)
      ~f:(fun (last_pos, last_len) { range } ->
        (*replace the text between the last chunk and the current chunk with whitespace*)
        let last_end = last_pos + last_len in
        let sub_pos, sub_len = range in
        let len_to_sub = last_end + sub_pos in
        whitespacify ~pos:last_end ~len:len_to_sub bytes;
        sub_pos, sub_len)
      substrings
  in
  let last_end = last_pos + last_len in
  
  whitespacify ~pos:last_end ~len:(Bytes.length bytes - last_end) bytes;
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bytes
;;


let trasform_server_notifiction ~chunk_regex  notif =
  let lsp_notif = Lsp.Client_notification.of_jsonrpc notif in
  match lsp_notif with
  | Error a ->
    notif
  | Ok lsp_notif ->
    (match lsp_notif with
     | Client_notification.TextDocumentDidOpen { textDocument } ->
       let substrings = get_chunks textDocument.text { regex = chunk_regex} in
       (match substrings with
        | [] ->
          Jsonrpc_runner.Log.log_s ~section:"transformer" "no chunks found";
          lsp_notif
        | _ ->
          let new_text = generate_isolated_doc textDocument.text substrings in
          let notif =
            Client_notification.TextDocumentDidOpen
              {
                textDocument = { textDocument with text = new_text; languageId = "html" };
              }
          in
          notif)
     | Client_notification.TextDocumentDidClose notif ->
       lsp_notif
     | Client_notification.TextDocumentDidChange notif ->
       (*TODO: Here i should check if the edit is outside the range of my chunks, if so, update the chunks positions but no the contents*)
       lsp_notif
     | _ ->
       lsp_notif)
    |> Lsp.Client_notification.to_jsonrpc
;;

let maybeRewrapNotification (notif : Jsonrpc.Notification.t) rewrapper =
  match rewrapper notif with None -> notif | Some new_notif -> new_notif
;;
