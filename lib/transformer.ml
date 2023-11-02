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
  let log = Jsonrpc_runner.Log.log_s ~section:"sub_doc"

  (** text Chunk source*)
  type chunkSource = { range : int * int }

  (**A chunk of text that *)
  type t = {
    doc : Text_document.t;
    chunks : chunkSource list; (** Where the source text *)
    original_uri : Uri.t;
    transformed_uri : Uri.t;
  }

  type chunkRule = {
    regex : Re.re;
    extension : string;
    exclusion_regex : Re.re option;
    exclusion_exclusion : Re.re option;
  }

  (*TODO: write tests for this *)

  (**rewrites the chunks to exclude the exclusion chunks. can split chunks, remove chunks, or shorten chunks*)
  let get_excluded_chunks chunks ~exclusion =
    (*adjust the pos an len to exclude an portions covered by the exclusion_regex. potentially split up chunks that have some portion within the exclusion*)
    let rec exclude_chunks (chunks : chunkSource list) exclusion_chunk =
      let chunk_to_string chunk =
        Printf.sprintf "%i,%i" (fst chunk.range) (snd chunk.range)
      in
      match chunks with
      | [] ->
        []
      | ({ range = pos, len } as chunk) :: chunks ->
        let e_pos, e_len = exclusion_chunk.range in
        let end_ = pos + len in
        let e_end = e_pos + e_len in
        let chunk_inside_exclusion = pos >= e_pos && end_ <= e_end in
        let exclusion_start_inside = e_pos >= pos && e_pos <= end_ in
        let exclusion_end_inside = e_end >= pos && e_end <= end_ in
        (* traceln "exclusion chunk %s" @@ chunk_to_string exclusion_chunk;*)
        if chunk_inside_exclusion
        then
          (* traceln "chunk (%i,%i) is inside exclusion, removing" pos end_ ;*)
          (*the chunk is inside the exclusion, so we need to remove it*)
          exclude_chunks chunks exclusion_chunk
        else if exclusion_start_inside && exclusion_end_inside
        then (
          (* traceln "chunk (%i,%i) is split by exclusion" pos end_ ;*)

          (*the exclusion is inside the chunk, so we need to split the chunk*)
          let new_chunk = { range = pos, e_pos - pos } in
          let new_chunk2 = { range = e_end, end_ - e_end } in
          new_chunk :: new_chunk2 :: exclude_chunks chunks exclusion_chunk)
        else if exclusion_start_inside
        then (
          (*traceln "chunk (%i,%i) ends inside exclusion" pos end_ ;*)

          (*the exclusion starts inside the chunk but ends outside, so we need to shorten the chunk*)
          let new_chunk = { range = pos, e_pos - pos } in
          new_chunk :: exclude_chunks chunks exclusion_chunk)
        else if exclusion_end_inside
        then (
          (* traceln "chunk (%i,%i) starts inside exclusion" pos end_ ;*)
          (*the exclusion ends inside the chunk but starts outside, so we need to shorten the chunk*)
          let new_chunk = { range = e_end, end_ - e_end } in
          new_chunk :: exclude_chunks chunks exclusion_chunk)
        else
          (*the exclusion is outside the chunk so we don't need to modify it*)
          chunk :: exclude_chunks chunks exclusion_chunk
    in
    if List.is_empty exclusion
    then chunks
    else
      List.fold ~init:chunks exclusion ~f:(fun chunks exclusion_chunk ->
        exclude_chunks chunks exclusion_chunk)
      |> List.filter ~f:(fun x -> x.range |> snd |> ( <> ) 0)
  ;;

  let get_chunks text regex =
    let matches = Re.all regex text in
    matches
    |> List.concat_map ~f:(fun m ->
    (*
      Re.Group.all m
      |> Array.to_list
      |> List.tl_exn
      |> List.iter ~f:(fun x -> traceln "chunk (%s)\n" x);
*)
      (*Normally we want the second match group because the first one is the entire regex whereas we only want the defined group, however if the user wraps the entire regex in one big match group there won't be a second. So we assume they want the entire regex*)
      let groups =
        (*we remove any offsets with -1,-1 becuase they are not matches*)
        Re.Group.all_offset m
        |> Array.filter ~f:(fun (x, y) -> not (x = -1 && y = -1))
        |> Array.to_list
      in
      if groups |> List.length = 0
      then []
      else
        groups
        |> List.tl
        |> Option.value_or_thunk ~default:(fun () -> [ groups |> List.hd_exn ])
        |> List.map ~f:(fun (pos, end_pos) ->
          let len = end_pos - pos in
          { range = pos, len }))
    |> List.filter ~f:(fun x -> x.range |> snd |> ( <> ) 0)
  ;;

  let get_chunks text chunk_rule =
    log @@ Printf.sprintf "getting chunks for text %s " text;
    let chunks = get_chunks text chunk_rule.regex in
    log @@ Printf.sprintf "found %i chunks before exclusion" @@ List.length chunks;
    match chunk_rule.exclusion_regex with
    | None ->
      chunks
    | Some exclusion_regex ->
      let exclusion_chunks = get_chunks text exclusion_regex in
      (match chunk_rule.exclusion_exclusion with
       | None ->
         log @@ Printf.sprintf "found %i exclusion_chunks" @@ List.length exclusion_chunks;
         let post_exclusion = get_excluded_chunks chunks ~exclusion:exclusion_chunks in
         log @@ Printf.sprintf "%i chunks after exclusion" @@ List.length post_exclusion;
         post_exclusion
       | Some exclusion_exclusion ->
         let exclusion_exclusion = get_chunks text exclusion_exclusion in
(*         traceln "found %i exclusion_exclusinos" @@ List.length exclusion_exclusion;*)
         let exclusion_chunks =
           get_excluded_chunks exclusion_chunks ~exclusion:exclusion_exclusion
         in
         let post_exclusion = get_excluded_chunks chunks ~exclusion:exclusion_chunks in
         log
         @@ Printf.sprintf "%i chunks after double exclusion"
         @@ List.length post_exclusion;
         post_exclusion)
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

type state = {
  docs : Sub_doc.t list;
  config : chunkRule;
}

let get_by_trans_uri uri docs =
  List.find docs ~f:(fun doc -> Uri.equal doc.transformed_uri uri)
;;

let get_by_original_uri uri docs =
  List.find docs ~f:(fun doc -> Uri.equal doc.original_uri uri)
;;

let set_doc_by_original_uri uri new_doc docs =
  let inserted = ref false in
  let docs =
    List.map docs ~f:(fun doc ->
      if Uri.equal doc.original_uri uri
      then (
        inserted := true;
        new_doc)
      else doc)
  in
  if !inserted then docs else new_doc :: docs
;;

let set_doc_by_trans_uri uri new_doc docs =
  let inserted = ref false in
  let docs =
    List.map docs ~f:(fun doc ->
      if Uri.equal doc.transformed_uri uri
      then (
        inserted := true;
        new_doc)
      else doc)
  in
  if !inserted then docs else new_doc :: docs
;;

let log = Jsonrpc_runner.Log.log_s ~section:"transformer"

let update_document
  ~state
  uri
  version
  (content_changes : TextDocumentContentChangeEvent.t list)
  =
  (*TODO: Here i should check if the edit is outside the range of my chunks, if so, update the chunks positions but no the contents*)
  (*
     I have a few issues here:
     1. It's hard to know when a new chunk is made. If one edit contains the start of a chunk and another contains the end then we have an issue
     1a. I could simply remake the chunks on every edit, but that sounds extremely wasteful
     (how bad would this actually be)
     1b. I could accumulte a list of "dirty points" and then re-scan those regions only
     After some benchmarking this is absurdly fast. like <10ms to find the chunks  in 12k lines of html
  *)
  match state.docs |> get_by_original_uri uri with
  | None ->
    log "no document for this uri";
    None
  | Some subDoc ->
    log "attempting to update document";
    Logs.debug (fun m -> m "document text %s" (Text_document.text subDoc.doc));
    (*First we apply the text edits*)
    let edits =
      List.map content_changes ~f:(fun c ->
        let range = c.range |> Option.value_exn in
        TextEdit.create ~range ~newText:c.text)
    in
    let newDoc = Text_document.apply_content_changes subDoc.doc content_changes in
    (*Then we recalculate the chunks for the updated document *)
    let newChunks = get_chunks (Text_document.text newDoc) state.config in
    let new_text =
      generate_isolated_doc
        ~languageId:"html"
        (Text_document.text newDoc |> String.map ~f:(fun x -> x))
        newChunks
    in
    (*Now we can modify the changeEvents so that the actual language server will make the correct changes*)
    let new_changes =
      List.map content_changes ~f:(fun c ->
        let pos =
          Text_document.absolute_position newDoc (c.range |> Option.value_exn).start
        in
        let pos2 =
          Text_document.absolute_position subDoc.doc (c.range |> Option.value_exn).end_
        in
        if pos <> pos2
        then
          log
          @@ Printf.sprintf
               "ERROR!! pos in current doc and pos in old doc have diverged, this is \
                very bad. pos:%i pos2 %i"
               pos
               pos2;
        let len = c.text |> String.length in
        let new_change_text = new_text |> String.sub ~pos ~len in
        { c with text = new_change_text })
    in
    (*Now to ensure we are in sync with the actual server we apply these new content-changes to the unchaged doc
      TODO: This may not be necissary, but it can't hurt
    *)
    let newSubDoc = { subDoc with doc = newDoc; chunks = newChunks } in
    let newMap = state.docs |> set_doc_by_original_uri uri newSubDoc in
    Some ({ state with docs = newMap }, new_changes)
;;

let set_uri_extension ext uri =
  let path = Uri.to_path uri in
  match Fpath.of_string path with
  | Ok path ->
    path |> Fpath.set_ext ext |> Fpath.to_string |> Uri.of_path
  | Error (`Msg msg) ->
    failwith @@ "uri not a path" ^ msg
;;

let changeUriTextExtTDoc ext (doc : TextDocumentItem.t) =
  { doc with uri = doc.uri |> set_uri_extension ext }
;;

let changeUriTextExt ext (doc : VersionedTextDocumentIdentifier.t) =
  { doc with uri = doc.uri |> set_uri_extension ext }
;;

let changeUriTextID ext (doc : TextDocumentIdentifier.t) =
  { doc with uri = doc.uri |> set_uri_extension ext }
;;

let change_client_notification_uri ext (client_notif : Client_notification.t) =
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

let change_client_request_uri ext (client_request : Client_request.packed) =
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

let transform_client_notification ~(state : state) notif =
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
           let doc = Text_document.make ~position_encoding:`UTF8 params in
           let transformed_uri =
             textDocument.uri |> set_uri_extension state.config.extension
           in
           let subDoc =
             {
               doc;
               chunks;
               transformed_uri;
               original_uri = doc |> Text_document.documentUri;
             }
           in
           (*this actually mutates the string*)
           whitespacify
             ~pos:0
             ~len:(String.length textDocument.text)
             (Bytes.of_string textDocument.text);
           ( {
               state with
               docs = state.docs |> set_doc_by_original_uri textDocument.uri subDoc;
             },
             lsp_notif )
         | _ ->
           (*First we save our representation, we don't want to save the the modified version, we always need to keep the real doc in memory so we can correctly apply edits *)
           let doc = Text_document.make ~position_encoding:`UTF8 params in
           let transformed_uri =
             textDocument.uri |> set_uri_extension state.config.extension
           in
           let subDoc =
             {
               doc;
               chunks;
               original_uri = doc |> Text_document.documentUri;
               transformed_uri;
             }
           in
           let new_docs = state.docs |> set_doc_by_original_uri textDocument.uri subDoc in
           (*new we make a modified version to send to the languageserver*)
           let new_text =
             generate_isolated_doc ~languageId:"html" textDocument.text chunks
           in
           let newParams =
             { params with textDocument = { params.textDocument with text = new_text } }
           in
           let notif = Client_notification.TextDocumentDidOpen newParams in
           Jsonrpc_runner.Log.log_s ~section:"transformer"
           @@ "forwarding on text : "
           ^ new_text;
           { state with docs = new_docs }, notif)
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
    let notif = notif |> change_client_notification_uri state.config.extension in
    state, notif |> Lsp.Client_notification.to_jsonrpc
;;

let transform_client_request ~state (req : Request.t) =
  let id = req.id in
  match Client_request.of_jsonrpc req with
  | Error msg ->
    failwith @@ "error deserialising json rpc message to client_request: " ^ msg
  | Ok req ->
    (match req |> change_client_request_uri state.config.extension with
     | E req ->
       state, req |> Lsp.Client_request.to_jsonrpc_request ~id)
;;

(**get's the original uri associated with some transformed uri*)
let get_original_uri state ~trans_uri =
  let originalDoc = state.docs |> get_by_trans_uri trans_uri in
  match originalDoc with
  | None ->
    let uri_str = trans_uri |> DocumentUri.to_path in
    log @@ "no document found for this transformed_uri:" ^ uri_str;
    None
  | Some doc ->
    Some (doc.doc |> Text_document.documentUri)
;;

let transform_server_notification ~(state : state) notification =
  match Server_notification.of_jsonrpc notification with
  | Error msg ->
    failwith @@ "error deserialising json rpc message to server_notification: " ^ msg
  | Ok notification ->
    let notification =
      match notification with
      | Server_notification.PublishDiagnostics p ->
        (match get_original_uri state ~trans_uri:p.uri with
         | None ->
           notification
         | Some originalUri ->
           let original_extension =
             originalUri
             |> Uri.to_path
             |> Fpath.of_string
             |> Stdlib.Result.get_ok
             |> Fpath.get_ext
           in
           Server_notification.PublishDiagnostics
             { p with uri = p.uri |> set_uri_extension @@ original_extension })
      | _ ->
        notification
    in
    state, notification |> Lsp.Server_notification.to_jsonrpc
;;

let transform_server_request ~state (request : Jsonrpc.Request.t) =
  let id = request.id in
  match Server_request.of_jsonrpc request with
  | Error msg ->
    failwith @@ "error deserialising json rpc message to server_request: " ^ msg
  | Ok request ->
    let request = match request with _ -> request in
    (match request with E req -> state, req |> Lsp.Server_request.to_jsonrpc_request ~id)
;;

let maybeRewrapNotification (notif : Jsonrpc.Notification.t) rewrapper =
  match rewrapper notif with None -> notif | Some new_notif -> new_notif
;;
