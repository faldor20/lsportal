open Base
open Eio
open Jsonrpc.Request
open Jsonrpc_runner
open Lsp.Types
open Test_utils
open Lsportal.Lib
open Lsportal

let a = {ts|
  let a=
  a
|ts}

exception Done of string

(** This sets up two connected RPC servers and then returns flows to write to their in and out streams.
    The forwarder *)
let make_forwarder_pair ~state_a ~state_b a_name b_name sw mgr =
  (*setup the intput and output flows*)
  let b_input_o, b_input_i = Eio.Process.pipe ~sw mgr in
  let b_out_o, b_out_i = Eio.Process.pipe ~sw mgr in
  let a_in_o, a_in_i = Eio.Process.pipe ~sw mgr in
  let a_out_o, a_out_i = Eio.Process.pipe ~sw mgr in
  (* make some logging helper functions*)
  let log = traceln "{%s} %s" in
  let log_request name method_ =
    log name @@ Printf.sprintf "got request with method '%s' forwarding on" method_
  in
  let log_notification name method_ =
    log name @@ "got notifictaion with method " ^ method_
  in
  (* make config for the forwarders*)
  let a_forwarder_c =
    LspForwarder.create_config
      ~req_pre:(fun (_, req) -> log_request a_name req.method_)
      ~notif_pre:(fun (_, notif) -> log_notification a_name notif.method_)
      "ls_forwarder"
      ~io:(a_in_o, a_out_i)
  in
  let b_forwarder_c =
    LspForwarder.create_config
      "editor_forwarder"
      ~req_pre:(fun (_, req) -> log_request b_name req.method_)
      ~notif_pre:(fun (_, notif) -> log_notification b_name notif.method_)
      ~io:(b_input_o, b_out_i)
  in
  (* create the linked forwarders*)
  let a_forwarder, b_forwarder =
    LspForwarder.create_pair ~state_a ~state_b a_forwarder_c b_forwarder_c
  in
  a_forwarder, b_forwarder, (b_out_o, b_input_i), (a_out_o, a_in_i)
;;

let make_echo_server name ~sw ~clock (inp, out) =
  Rpc.create
    ~on_request:(fun (server, req) ->
      traceln "%s : got request %s" name req.method_;
      if String.( = ) req.method_ "slow"
      then
        ( (fun f ->
            Time.sleep clock 0.5;
            Jsonrpc.Response.ok
              req.id
              (`String (Printf.sprintf "Hi from %s. Your method: %s" name req.method_))
            |> f)
          |> Jsonrpc_runner.Reply.Later,
          server.state )
      else
        ( Jsonrpc.Response.ok
            req.id
            (`String (Printf.sprintf "Hi from %s. Your method: %s" name req.method_))
          |> Jsonrpc_runner.Reply.Now,
          server.state ))
    ~on_notification:(fun (server, notif) ->
      traceln "%s_echoer: Got notification: '%s'" name notif.method_;
      Jsonrpc_runner.Notify.Continue, server.state)
    ~name
    (Jsonrpc_runner.Flow_io.make inp out)
    "state"
;;

(**Makes 4 connected servers an editor and lang_server that echo out messages, and one forwarder for each, which forward messages to each other and are linked to their respective echo server via a pipe *)
let make_connected_servers ~sw ~clock mgr =
  let fakeState = ref "" in
  let fw_editor, fw_ls, editor_io, ls_io =
    make_forwarder_pair ~state_a:fakeState ~state_b:fakeState "fw_editor" "fw_lsp" sw mgr
  in
  let editor = make_echo_server ~sw ~clock "editor" editor_io in
  let lang_server = make_echo_server ~sw ~clock "langServer" ls_io in
  Fiber.fork_daemon ~sw (fun _ ->
    try
      Switch.run (fun sw ->
        Fiber.all
          [
            (fun () -> Rpc.run editor);
            (fun () -> Rpc.run fw_editor);
            (fun () -> Rpc.run fw_ls);
            (fun () -> Rpc.run lang_server);
          ];
        `Stop_daemon)
    with
    | Eio.Exn.Multiple exns ->
      `Stop_daemon
    | exn ->
      traceln "%s" @@ Base.Exn.to_string exn;
      `Stop_daemon);
  editor, lang_server
;;

let traceLogs () =
  (Log.level := fun x -> true);
  Log.out := Stdlib.Format.err_formatter
;;

let send_test_request rpc ~sw ~id method_ =
  Fiber.fork ~sw (fun () ->
    traceln "sending a request";
    let response =
      Rpc.request
        rpc
        (Jsonrpc.Request.create
           ~params:(`List [ `String "myParam" ])
           ~id:(`Int id)
           ~method_
           ())
    in
    match response.result with
    | Ok res ->
      traceln "editor_response: %s" @@ Json.to_pretty_string res
    | Error err ->
      traceln "editor_response_error: err: %s" err.message)
;;

let send_test_notification rpc method_ =
  traceln "sending a notification";
  Rpc.notification
    rpc
    (Jsonrpc.Notification.create ~params:(`List [ `String "myParam" ]) ~method_ ())
;;

let%expect_test "dual servers" =
  Eio_main.run (fun env ->
    Switch.run (fun sw ->
      let clock = Eio.Stdenv.clock env in
      let editor, lang_server =
        make_connected_servers ~sw ~clock (Eio.Stdenv.process_mgr env)
      in
      traceln "===== test 1===== sending from editor";
      send_test_request editor ~sw ~id:1 "hi from editor";
      traceln "===== test 1===== sending from slow";
      send_test_request editor ~sw ~id:100 "slow";
      traceln "===== test 1.5===== sending from editor again";
      send_test_request editor ~sw ~id:11 "hi from editor2";
      traceln "===== test 1.5===== sending from editor again2";
      send_test_request editor ~sw ~id:12 "hi from editor3";
      traceln "===== test 2===== sending from langserver";
      send_test_request lang_server ~sw ~id:2 "hi from langServer";
      traceln "===== test 3===== notification from editor";
      send_test_notification editor "notified! from_editor";
      traceln "===== test 4===== notification from editor";
      send_test_notification editor "notified_again_from_editor";
      traceln "===== test 4===== notification from langserver";
      send_test_notification lang_server "notified_from server";
      Fiber.fork_daemon ~sw (fun () ->
        Eio.Time.sleep clock 3.0;
        Rpc.stop lang_server;
        Rpc.stop editor;
        `Stop_daemon)));
  [%expect
    {|
    +===== test 1===== sending from editor
    +sending a request
    +===== test 1===== sending from slow
    +sending a request
    +===== test 1.5===== sending from editor again
    +sending a request
    +===== test 1.5===== sending from editor again2
    +sending a request
    +===== test 2===== sending from langserver
    +sending a request
    +===== test 3===== notification from editor
    +sending a notification
    +{fw_editor} got request with method 'hi from langServer' forwarding on
    +{fw_lsp} got request with method 'hi from editor' forwarding on
    +{fw_lsp} got request with method 'slow' forwarding on
    +editor : got request hi from langServer
    +langServer : got request hi from editor
    +{fw_lsp} got request with method 'hi from editor2' forwarding on
    +langServer : got request slow
    +{fw_lsp} got request with method 'hi from editor3' forwarding on
    +langServer : got request hi from editor2
    +{fw_lsp} got notifictaion with method notified! from_editor
    +===== test 4===== notification from editor
    +sending a notification
    +langServer : got request hi from editor3
    +editor_response: "Hi from langServer. Your method: hi from editor"
    +editor_response: "Hi from langServer. Your method: hi from editor2"
    +{fw_lsp} got notifictaion with method notified_again_from_editor
    +===== test 4===== notification from langserver
    +sending a notification
    +langServer_echoer: Got notification: 'notified! from_editor'
    +{fw_editor} got notifictaion with method notified_from server
    +langServer_echoer: Got notification: 'notified_again_from_editor'
    +editor_response: "Hi from langServer. Your method: hi from editor3"
    +editor_response: "Hi from editor. Your method: hi from langServer"
    +editor_echoer: Got notification: 'notified_from server'
    +editor_response: "Hi from langServer. Your method: slow"
    +exception reading line("Cancelled: Stdlib.Exit")
    +exception reading line("Cancelled: Stdlib.Exit")
    +exception reading line("Cancelled: Stdlib.Exit")
    +exception reading line("Cancelled: Stdlib.Exit") |}]
;;

open Transformer

let chunk_regex = {|<!--html-->([\s\S]*?)<!--html-->|} |> Re.Pcre.re |> Re.compile

let default_state =
  {
    docs = [];
    config = { regex = chunk_regex; exclusion_regex = None; extension = "ts"; exclusion_exclusion=None };
  }
;;

(* This will test the notification transformer code *)
let%expect_test "notification transformer simple" =
  let did_open_notification_text =
    {|
let testString=
  `
<!--html-->
let a=10
  
<!--html-->  
`
let testString2=
  `
<!--html-->
a+10
  
<!--html-->  
`
|}
  in
  let did_open_notification =
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
                  "text", `String did_open_notification_text;
                ] );
          ])
      ()
  in
  let getTextDocument notif =
    notif |> Lsp.Client_notification.of_jsonrpc |> function
    | Ok (Lsp.Client_notification.TextDocumentDidOpen { textDocument }) ->
      textDocument
    | _ ->
      failwith "wrong notification"
  in
  let state, transformed =
    Transformer.transform_client_notification ~state:default_state did_open_notification
  in
  Stdio.printf "start length:%i end_length:%i" (String.length did_open_notification_text)
  @@ String.length (getTextDocument transformed).text;
  [%expect {|start length:118 end_length:118|}];
  Stdio.printf "%s\n"
  @@ (transformed |> Jsonrpc.Notification.yojson_of_t |> Json.to_pretty_string);
  [%expect
    {|
    {
      "params": {
        "textDocument": {
          "languageId": "typescript",
          "text": "\n               \n   \n           \nlet a=10\n  \n             \n \n                \n   \n           \na+10\n  \n             \n \n",
          "uri": "file:///test.ts",
          "version": 1
        }
      },
      "method": "textDocument/didOpen",
      "jsonrpc": "2.0"
    }

  |}]
;;

(*TODO: ensure this works with newlines at the start *)
let did_open_notification_text =
  {|let testString=
  `
<!--html-->
let a=10
12345678
12345678
<!--html-->  
`
let testString2=
  `
<!--html-->
a+10
  
<!--html-->  
|}
;;

let%expect_test "DidChange notification " =
  let did_open_notification = did_open_notification_text |> make_did_open_notifiction in
  let open Lsp.Types in
  let uri = DocumentUri.t_of_yojson (`String "file:///test.txt") in
  let state, transformed =
    Transformer.transform_client_notification ~state:default_state did_open_notification
  in
  let did_change_notifiction = make_did_change_notifiction uri (2, 5) "<newdiv" in
  let state, transformed =
    Transformer.transform_client_notification ~state did_change_notifiction
  in
  let getTextDocument notif =
    notif |> Lsp.Client_notification.of_jsonrpc |> function
    | Ok (Lsp.Client_notification.TextDocumentDidChange { contentChanges }) ->
      contentChanges
    | _ ->
      failwith "wrong notification"
  in
  let finalText =
    (state.docs |> Transformer.get_by_original_uri uri |> Option.value_exn).doc
    |> Lsp.Text_document.text
  in
  Stdio.printf "start length:%i end_length:%i" (String.length did_open_notification_text)
  @@ String.length finalText;
  [%expect {|start length:130 end_length:137|}];
  Stdio.printf "%s\n" finalText;
  [%expect
    {|
    let testString=
      `
    <!--html-->
    let a=10
    12345678
    12<newdiv345678
    <!--html-->
    `
    let testString2=
      `
    <!--html-->
    a+10

    <!--html--> |}];
  Stdio.printf "%s\n"
  @@ (transformed |> Jsonrpc.Notification.yojson_of_t |> Json.to_pretty_string);
  [%expect
    {|
    {
      "params": {
        "contentChanges": [
          {
            "range": {
              "end": { "character": 2, "line": 5 },
              "start": { "character": 2, "line": 5 }
            },
            "text": "<newdiv"
          }
        ],
        "textDocument": { "uri": "file:///test.ts", "version": 2 }
      },
      "method": "textDocument/didChange",
      "jsonrpc": "2.0"
    }

  |}]
;;

let%expect_test "DidChange notification outside chunk" =
  let did_open_notification = did_open_notification_text |> make_did_open_notifiction in
  let uri = DocumentUri.t_of_yojson (`String "file:///test.txt") in
  let state, transformed =
    Transformer.transform_client_notification ~state:default_state did_open_notification
  in
  let did_change_notifiction = make_did_change_notifiction uri (2, 0) "<newdiv" in
  let state, transformed =
    Transformer.transform_client_notification ~state did_change_notifiction
  in
  let did_change_notifiction2 =
    make_did_change_notifiction ~override:true uri (2, 0) "<rep2"
  in
  let state, transformed =
    Transformer.transform_client_notification ~state did_change_notifiction2
  in
  let finalText =
    (state.docs |> Transformer.get_by_original_uri uri |> Option.value_exn).doc
    |> Lsp.Text_document.text
  in
  Stdio.printf "start length:%i end_length:%i" (String.length did_open_notification_text)
  @@ String.length finalText;
  [%expect
    {|
    +line_count: 1
    +s_chars: 2
    +text_len: 5
    +chars: 6
    start length:130 end_length:138|}];
  Stdio.printf "%s\n" finalText;
  [%expect
    {|
    le<rep2divt testString=
      `
    <!--html-->
    let a=10
    12345678
    12345678
    <!--html-->
    `
    let testString2=
      `
    <!--html-->
    a+10

    <!--html--> |}];
  Stdio.printf "%s\n"
  @@ (transformed |> Jsonrpc.Notification.yojson_of_t |> Json.to_pretty_string);
  [%expect
    {|
    {
      "params": {
        "contentChanges": [
          {
            "range": {
              "end": { "character": 6, "line": 0 },
              "start": { "character": 2, "line": 0 }
            },
            "text": "     "
          }
        ],
        "textDocument": { "uri": "file:///test.ts", "version": 2 }
      },
      "method": "textDocument/didChange",
      "jsonrpc": "2.0"
    }

  |}]
;;

open Transformer.Sub_doc
open Transformer
open Lsp.Types
open Lsp

let chunk_rule =
  {
    regex = Re.compile @@ Re.Pcre.re {|<.*?>(.*)<\/.*?>|};
    extension = "html";
    exclusion_regex = Some Re.(compile @@ Pcre.re {|(<script>.*</script>)|});
  exclusion_exclusion=None
  }
;;

let%expect_test "chunk exclusion inside" =
  let text = "<html><body><script>console.log('hello world')</script></body></html>" in
  let chunks = get_chunks text chunk_rule in
  List.iter chunks ~f:(fun { range = pos, len } ->
    Stdio.printf "%i,%i\n" pos len;
    let text = String.sub text ~pos ~len in
    Stdio.printf "%s\n" text);
  [%expect {|
    6,6
    <body>
    55,7
    </body>|}]
;;

let%expect_test "chunk exclusion from end" =
  let text = "<html><body><script>console.log('hello world')</body></html></script>" in
  let chunks = get_chunks text chunk_rule in
  List.iter chunks ~f:(fun { range = pos, len } ->
    Stdio.printf "%i,%i\n" pos len;
    let text = String.sub text ~pos ~len in
    Stdio.printf "%s\n" text);
  [%expect {|
    6,6
    <body>|}]
;;

let%expect_test "chunk exclusion from start" =
  let text = "<script><html><body>console.log('hello world')</script></body></html>" in
  let chunks = get_chunks text chunk_rule in
  List.iter chunks ~f:(fun { range = pos, len } ->
    Stdio.printf "%i,%i\n" pos len;
    let text = String.sub text ~pos ~len in
    Stdio.printf "%s\n" text);
  [%expect {|
    55,7
    </body>|}]
;;

let%expect_test "chunk exclusion aligned with start" =
  let text = "<html><script><body>console.log('hello world')</script></body></html>" in
  let chunks = get_chunks text chunk_rule in
  List.iter chunks ~f:(fun { range = pos, len } ->
    Stdio.printf "%i,%i\n" pos len;
    let text = String.sub text ~pos ~len in
    Stdio.printf "%s\n" text);
  [%expect {|
    55,7
    </body>|}]
;;

let%expect_test "chunk exclusion from inside" =
  let text = "<script><html><body>console.log('hello world')</body></html></script>" in
  let chunks = get_chunks text chunk_rule in
  List.iter chunks ~f:(fun { range = pos, len } ->
    Stdio.printf "%i,%i\n" pos len;
    let text = String.sub text ~pos ~len in
    Stdio.printf "%s\n" text);
  [%expect {| |}]
;;

let%expect_test "chunk no exclusion_regex" =
  let chunk_rule = { chunk_rule with exclusion_regex = None } in
  let text = "<html><body><script>console.log('hello world')</script></body></html>" in
  let chunks = get_chunks text chunk_rule in
  List.iter chunks ~f:(fun { range = pos, len } ->
    Stdio.printf "%i,%i\n" pos len;
    let text = String.sub text ~pos ~len in
    Stdio.printf "%s\n" text);
  [%expect {|
    6,56
    <body><script>console.log('hello world')</script></body>|}]
;;

let testString =
  {|
open Dream
open Dream_html

open Tyxml_ppx
open Tyxml_syntax

let textBox2 param=
  <html>
% List.iter ["bye";"hi"] ~f:(fun greet ->
  <div style="">
  <p> hi this is text <%s param %>, <%s greet > </p>
% );
  </div>
</html>

(*<div class="max- ">*)
  |}
;;

let html_regex = {|(<.*?>[\s\S]*</.*?>)|} |> Re.Pcre.re |> Re.compile
let html_exclusion_regex = {|(<%[\s\S]*?%>)|} |> Re.Pcre.re |> Re.compile

let%expect_test "regex does what i expect" =
  let matches = Re.exec html_regex testString in
  (*we always drop the first group because that represents any part of the regex that matched whereas we only want groups that are defined using () *)
  let matches = matches |> Re.Group.all |> Array.to_list |> List.tl_exn in
  List.iter matches ~f:(fun x -> Stdio.printf "%s\n" x);
  [%expect {|
    <html>
    % List.iter ["bye";"hi"] ~f:(fun greet ->
      <div style="">
      <p> hi this is text <%s param %>, <%s greet > </p>
    % );
      </div>
    </html>|}]
;;

(* This will test the notification transformer code *)
let%expect_test "html regex test" =
  let testString = testString in
  let did_open_notification = make_did_open_notifiction testString in
  let state =
    {
      default_state with
      config =
        {
          default_state.config with
          exclusion_regex = Some html_exclusion_regex;
          regex = html_regex;
        };
    }
  in
  let state, transformed =
    Transformer.transform_client_notification ~state did_open_notification
  in
  Stdio.printf "start length:%i end_length:%i" (String.length testString)
  @@ String.length (getTextDocument transformed).text;
  [%expect {|start length:253 end_length:253|}];
  Stdio.printf "%s\n"
  @@ (transformed |> Jsonrpc.Notification.yojson_of_t |> Json.to_pretty_string);
  [%expect
    {|
    {
      "params": {
        "textDocument": {
          "languageId": "typescript",
          "text": "\n          \n               \n\n              \n                 \n\n                   \n  <html>\n% List.iter [\"bye\";\"hi\"] ~f:(fun greet ->\n  <div style=\"\">\n  <p> hi this is text             , <%s greet > </p>\n% );\n  </div>\n</html>\n\n                       \n  ",
          "uri": "file:///test.ts",
          "version": 1
        }
      },
      "method": "textDocument/didOpen",
      "jsonrpc": "2.0"
    }

  |}]
;;

let regex = {|([\s\S]*)|} |> Re.Pcre.regexp
let exclusion_regex = {|(<[^%].*?>)|(<%\w*)|(%>)|(^%)|} |> Re.Pcre.regexp
let exclusion_regex = {|(<(.*?)>[\s\S]*<\/.*?>)(?:[\s\S]*;;)|(<(.*?)>[\s\S]*<\/.*?>)|} |> Re.Pcre.regexp
let exclusion_exculsion_regex = {|^\s*%(.*)|<%\w* (.*)%>|} |> Re.Pcre.regexp~flags:[ `MULTILINE ]

let%expect_test "html regex test_2" =
  let testString = testString in
  let did_open_notification = make_did_open_notifiction testString in
  let getTextDocument notif =
    notif |> Lsp.Client_notification.of_jsonrpc |> function
    | Ok (Lsp.Client_notification.TextDocumentDidOpen { textDocument }) ->
      textDocument
    | _ ->
      failwith "wrong notification"
  in
  let state =
    {
      default_state with
      config = { default_state.config with exclusion_regex = Some exclusion_regex; regex; exclusion_exclusion =Some exclusion_exculsion_regex };
    }
  in
  let state, transformed =
    Transformer.transform_client_notification ~state did_open_notification
  in
  Stdio.printf "start length:%i end_length:%i" (String.length testString)
  @@ String.length (getTextDocument transformed).text;
  [%expect {|start length:253 end_length:253|}];
  Stdio.printf "%s\n"
  @@ (transformed |> Jsonrpc.Notification.yojson_of_t |> Json.to_pretty_string);
  [%expect
    {|
    {
      "params": {
        "textDocument": {
          "languageId": "typescript",
          "text": "\nopen Dream\nopen Dream_html\n\nopen Tyxml_ppx\nopen Tyxml_syntax\n\nlet textBox2 param=\n        \n  List.iter [\"bye\";\"hi\"] ~f:(fun greet ->\n                \n                          param                     \n  );\n        \n       \n\n(*<div class=\"max- \">*)\n  ",
          "uri": "file:///test.ts",
          "version": 1
        }
      },
      "method": "textDocument/didOpen",
      "jsonrpc": "2.0"
    }

  |}]
;;

(*
   let%expect_test "update_document" =
  let state = { docs = Map.empty (module String) } in
  let uri = Lsp.Types.DocumentUri.of_path "file:///path/to/document" in
  let version = 1 in
  let initial_text = {|
let testString=
  ` 
  <!--html-->
    let a=10
  <!--html-->
  `
  let testString2=
  `
  <!--html-->
    a+10

    <!--html-->
  `
  |} in
  let textDocument:TextDocumentItem.t={text=initial_text; version;uri;languageId="typescript" } in
  let document_open_notification = Lsp.Types.DidOpenTextDocumentParams.create ~textDocument in
  let initial_doc = Text_document.make document_open_notification ~position_encoding:`UTF8 in
  let initial_subdoc = { doc = initial_doc; chunks = [] } in
  let state = { docs = Map.set state.docs ~key:(Uri.to_string uri) ~data:initial_subdoc } in
  let change_range=(Range.create ~end_:(Position.create 1 1) ~start:(Position.create 1 1)) in
let content_changes = [ TextDocumentContentChangeEvent.create ~range:change_range ~text:"Goodbye, world!" ] in
  
  let expected_text = "Goodbye, world!" in
  let expected_subdoc = { doc = Text_document.make expected_text; chunks = [ { start = 9; end_ = 14 } ] } in
  let expected_state = { state with docs = Map.set state.docs ~key:(Uri.to_string uri) ~data:expected_subdoc } in
  let actual_state, actual_changes = Transformer.update_document ~state ~chunk_regex uri version content_changes in
  [%expect {|
    ((state ((docs ((file:///path/to/document ((doc ((text "Goodbye, world!"))) (chunks ((start 9) (end_ 14)))))))))
    ((content_changes (((range ((start_line 0) (start_character 0) (end_line 0) (end_character 0))) (text "Goodbye, world!"))))) |}];
  assert_equal ~printer:(fun x -> x) (Some expected_state) actual_state;
  assert_equal ~printer:(fun x -> x) content_changes actual_changes
*)
