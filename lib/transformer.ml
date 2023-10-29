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

module Sub_doc = struct
  (** text Chunk source*)
  type chunkSource = { range : int * int }

  (**A chunk of text that *)
  type t = {
    doc : Text_document.t;
    chunks : chunkSource list; (** Where the source text *)
  }

  type chunkRule = {
    regex : Re.re;
    extension : string;
    exclusion_regex : Re.re option;
  }

  let get_chunks text chunk_rule =
    let matches = Re.all chunk_rule.regex text in
    matches
    |> List.map ~f:(fun m ->
      let pos, end_pos = Re.Group.offset m 1 in
      let len = end_pos - pos in
      { range = pos, len })
  ;;

  (**Takes a byte sequence of a string and converts any char within the range to a space*)
  let whitespacify ~pos ~len bytes =
    for i = 0 to len - 1 do
      let byte = Bytes.unsafe_get bytes (pos + i) in
      if Char.( = ) byte '\n' then () else Bytes.unsafe_set bytes (pos + i) ' '
    done
  ;;

  (**takes a list of substrings and the original text and creates a new text that replaces all text outside the substrings with whitespace*)
  let generate_isolated_doc ~languageId text (chunks : chunkSource list) =
    (* TODO:Check this all for off by one errors *)
    let bytes = Bytes.of_string text in
    let last_pos, last_len =
      List.fold
        ~init:(0, 0)
        ~f:(fun (last_pos, last_len) { range } ->
          (*replace the text between the last chunk and the current chunk with whitespace*)
          let last_end = last_pos + last_len in
          let chunk_pos, chunk_len = range in
          let len_to_chunk_start = chunk_pos - last_end in
          whitespacify ~pos:last_end ~len:len_to_chunk_start bytes;
          chunk_pos, chunk_len)
        chunks
    in
    (*replace the text after the last chunk with whitespace*)
    let last_end = last_pos + last_len in
    whitespacify ~pos:last_end ~len:(Bytes.length bytes - last_end) bytes;
    let new_text = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bytes in
    new_text
  ;;

  (* I'm not sure if we really need this? If we do we should use the lsp text_document implimentation for easier updating*)
end

open Sub_doc
open Lsp.Types

type docMap = Sub_doc.t Map.M(String).t

type state = {
  docs : docMap;
  config : chunkRule;
}

let update_document ~state uri version content_changes =
  (*TODO: Here i should check if the edit is outside the range of my chunks, if so, update the chunks positions but no the contents*)
  (*
     I have a few issues here:
     1. It's hard to know when a new chunk is made. If one edit contains the start of a chunk and another contains the end then we have an issue
     1a. I could simply remake the chunks on every edit, but that sounds extremely wasteful
     (how bad would this actually be)
     1b. I could accumulte a list of "dirty points" and then re-scan those regions only
     After some benchmarking this is absurdly fast. like <10ms to find the chunks  in 12k lines of html
  *)
  match Map.find state.docs (uri |> Types.DocumentUri.to_string) with
  | None ->
    Jsonrpc_runner.Log.log_s ~section:"transformer" "no document for this uri";
    None
  | Some subDoc ->
    Jsonrpc_runner.Log.log_s ~section:"transformer" "attempting to update document";
    traceln "doc length%i" (subDoc.doc |> Text_document.text |> String.length);
    (*First we apply the text edits*)
    let newDoc =
      Text_document.apply_content_changes subDoc.doc ~version content_changes
    in
    traceln "unchanged  doc length%i" (subDoc.doc |> Text_document.text |> String.length);
    traceln "doc length%i" (newDoc |> Text_document.text |> String.length);
    (*Then we recalculate the chunks for the updated document *)
    let newChunks = get_chunks (Text_document.text newDoc) state.config in
    traceln "orig_text '%i'" (Text_document.text newDoc |> String.length);
    let new_text =
      generate_isolated_doc ~languageId:"html" (Text_document.text newDoc) newChunks
    in
    (*Now we can modify the changeEvents so that the actual language server will make the correct changes*)
    let new_changes =
      List.map content_changes ~f:(fun c ->
        let pos =
          Text_document.absolute_position newDoc (c.range |> Option.value_exn).start
        in
        traceln "pos:%i " pos;
        traceln "new_text '%i'" (new_text |> String.length);
        let len = c.text |> String.length in
        let new_change_text = new_text |> String.sub ~pos ~len in
        traceln "new_change_text '%i'" (new_change_text |> String.length);
        { c with text = new_change_text })
    in
    (*Now to ensure we are in sync with the actual server we apply these new content-changes to the unchaged doc
      TODO: This may not be necissary, but it can't hurt
    *)
    let newDoc = Text_document.apply_content_changes subDoc.doc ~version new_changes in
    let newSubDoc = { doc = newDoc; chunks = newChunks } in
    traceln "doc length%i" (newSubDoc.doc |> Text_document.text |> String.length);
    let newMap =
      Map.set state.docs ~key:(uri |> Types.DocumentUri.to_string) ~data:newSubDoc
    in
    Some ({ state with docs = newMap }, content_changes)
;;

let setUriExtension ext uri =
  let path = Uri.to_path uri in
  match Fpath.of_string path with
  | Ok path ->
    path |> Fpath.set_ext ext |> Fpath.to_string |> Uri.of_path
  | Error (`Msg msg) ->
    failwith @@ "uri not a path" ^ msg
;;

let changeUriTextExtTDoc ext (doc : TextDocumentItem.t) =
  { doc with uri = doc.uri |> setUriExtension ext }
;;

let changeUriTextExt ext (doc : VersionedTextDocumentIdentifier.t) =
  { doc with uri = doc.uri |> setUriExtension ext }
;;

let changeUriTextID ext (doc : TextDocumentIdentifier.t) =
  { doc with uri = doc.uri |> setUriExtension ext }
;;

let change_notification_uri ext (client_notif : Client_notification.t) =
  match client_notif with
  | Client_notification.DidSaveTextDocument p ->
    Client_notification.DidSaveTextDocument
      { p with textDocument = changeUriTextID ext p.textDocument }
  | Client_notification.TextDocumentDidOpen { textDocument = doc } ->
    Client_notification.TextDocumentDidOpen
      { textDocument = changeUriTextExtTDoc ext doc }
  | Client_notification.TextDocumentDidClose { textDocument = doc } ->
    Client_notification.TextDocumentDidClose { textDocument = changeUriTextID ext doc }
  | Client_notification.TextDocumentDidChange { textDocument = doc; contentChanges } ->
    Client_notification.TextDocumentDidChange
      { textDocument = changeUriTextExt ext doc; contentChanges }
  | Client_notification.WillSaveTextDocument p ->
    Client_notification.WillSaveTextDocument
      { p with textDocument = changeUriTextID ext p.textDocument }
  | _ ->
    client_notif
;;

let change_request_uri ext (client_request : Client_request.packed) =
  let open Client_request in
  match client_request with
  | E (TextDocumentHover p) ->
    E (TextDocumentHover { p with textDocument = changeUriTextID ext p.textDocument })
  | E (TextDocumentDefinition p) ->
    E
      (TextDocumentDefinition { p with textDocument = changeUriTextID ext p.textDocument })
  | E (TextDocumentReferences p) ->
    E
      (TextDocumentReferences { p with textDocument = changeUriTextID ext p.textDocument })
  | E (TextDocumentFormatting p) ->
    E
      (TextDocumentFormatting { p with textDocument = changeUriTextID ext p.textDocument })
  | E (TextDocumentRangeFormatting p) ->
    E
      (TextDocumentRangeFormatting
         { p with textDocument = changeUriTextID ext p.textDocument })
  | E (TextDocumentOnTypeFormatting p) ->
    E
      (TextDocumentOnTypeFormatting
         { p with textDocument = changeUriTextID ext p.textDocument })
  | E (TextDocumentCodeLens p) ->
    E (TextDocumentCodeLens { p with textDocument = changeUriTextID ext p.textDocument })
  | E (TextDocumentDeclaration p) ->
    E
      (TextDocumentDeclaration
         { p with textDocument = changeUriTextID ext p.textDocument })
  | E (TextDocumentTypeDefinition p) ->
    E
      (TextDocumentTypeDefinition
         { p with textDocument = changeUriTextID ext p.textDocument })
  | E (TextDocumentImplementation p) ->
    E
      (TextDocumentImplementation
         { p with textDocument = changeUriTextID ext p.textDocument })
  | E (TextDocumentCompletion p) ->
    E
      (TextDocumentCompletion { p with textDocument = changeUriTextID ext p.textDocument })
  | E (TextDocumentPrepareRename p) ->
    E
      (TextDocumentPrepareRename
         { p with textDocument = changeUriTextID ext p.textDocument })
  | E (TextDocumentRename p) ->
    E (TextDocumentRename { p with textDocument = changeUriTextID ext p.textDocument })
  | E (TextDocumentFoldingRange p) ->
    E
      (TextDocumentFoldingRange
         { p with textDocument = changeUriTextID ext p.textDocument })
  | E (LinkedEditingRange p) ->
    E (LinkedEditingRange { p with textDocument = changeUriTextID ext p.textDocument })
  | E (SelectionRange p) ->
    E (SelectionRange { p with textDocument = changeUriTextID ext p.textDocument })
  | E (SignatureHelp p) ->
    E (SignatureHelp { p with textDocument = changeUriTextID ext p.textDocument })
  | E (CodeAction p) ->
    E (CodeAction { p with textDocument = changeUriTextID ext p.textDocument })
  | E (TextDocumentHighlight p) ->
    E (TextDocumentHighlight { p with textDocument = changeUriTextID ext p.textDocument })
  | E (DocumentSymbol p) ->
    E (DocumentSymbol { p with textDocument = changeUriTextID ext p.textDocument })
  | _ ->
    client_request
;;

let trasform_server_notifiction ~(state : state) notif =
  let lsp_notif = Lsp.Client_notification.of_jsonrpc notif in
  Jsonrpc_runner.Log.log_s ~section:"transformer" "running transformer on notifiction";
  match lsp_notif with
  | Error a ->
    Jsonrpc_runner.Log.log_s ~section:"transformer" "Error parsing notifiction";
    state, notif
  | Ok lsp_notif ->
    let state, notif =
      match lsp_notif with
      | Client_notification.TextDocumentDidOpen ({ textDocument } as params) ->
        let chunks = get_chunks textDocument.text state.config in
        Jsonrpc_runner.Log.log_s ~section:"transformer"
        @@ Printf.sprintf "found %i chunks"
        @@ List.length chunks;
        (match chunks with
         | [] ->
           Jsonrpc_runner.Log.log_s ~section:"transformer" "no chunks found";
           (*this actually mutates the stirng*)
           whitespacify
             ~pos:0
             ~len:(String.length textDocument.text)
             (Bytes.unsafe_of_string_promise_no_mutation textDocument.text);
           let doc = Text_document.make ~position_encoding:`UTF8 params in
           let subDoc = { doc; chunks } in
           ( {
               state with
               docs =
                 Map.set state.docs ~key:(textDocument.uri |> Uri.to_string) ~data:subDoc;
             },
             lsp_notif )
         | _ ->
           let new_text =
             generate_isolated_doc ~languageId:"html" textDocument.text chunks
           in
           let newParams =
             { params with textDocument = { params.textDocument with text = new_text } }
           in
           let doc = Text_document.make ~position_encoding:`UTF8 newParams in
           let subDoc = { doc; chunks } in
           let newMap =
             Map.set
               state.docs
               ~key:(textDocument.uri |> Types.DocumentUri.to_string)
               ~data:subDoc
           in
           (*TODO: update the Text_document with the new text*)
           let notif = Client_notification.TextDocumentDidOpen newParams in
           Jsonrpc_runner.Log.log_s ~section:"transformer"
           @@ "forwarding on text : "
           ^ new_text;
           { state with docs = newMap }, notif)
      | Client_notification.TextDocumentDidClose notif ->
        state, lsp_notif
      | Client_notification.TextDocumentDidChange
          ({ textDocument = { uri; version }; contentChanges } as notif) ->
        (match update_document ~state uri version contentChanges with
         | None ->
           state, lsp_notif
         | Some (new_state, new_changes) ->
           ( new_state,
             Client_notification.TextDocumentDidChange
               { notif with contentChanges = new_changes } ))
      | _ ->
        Jsonrpc_runner.Log.log_s ~section:"transformer"
        @@ "Hit unsupported notifiction, not transforming method:"
        ^ notif.method_;
        state, lsp_notif
    in
    (*We change the extension because sometimes the language server will be unhappy if the extension doesn't match what they want*)
    let notif = notif |> change_notification_uri state.config.extension in
    state, notif |> Lsp.Client_notification.to_jsonrpc
;;

let transform_client_request ~state (req:Request.t) =
  let id = req.id in
    match Client_request.of_jsonrpc req with
  | Error msg ->
    failwith @@ "error deserialising json rpc message to client_request: " ^ msg
  | Ok req ->
    (match req |> change_request_uri state.config.extension with
     | E req ->
       state, req |> Lsp.Client_request.to_jsonrpc_request ~id )
;;

let maybeRewrapNotification (notif : Jsonrpc.Notification.t) rewrapper =
  match rewrapper notif with None -> notif | Some new_notif -> new_notif
;;
