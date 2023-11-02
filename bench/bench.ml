open Core
open Core_bench
open Lsportal.Transformer
open Lsportal

let chunk_regex = {|<!--html-->([\s\S]*?)<!--html-->|} |> Re.Pcre.re |> Re.compile

let rule : Transformer.Sub_doc.chunkRule =
  { extension = "ts"; exclusion_regex = Some ({|<div>[\s\S]*?</div>|}|>Re.Pcre.regexp); exclusion_exclusion=None; regex = chunk_regex }
;;

let test = {ts|
  let a=10
  a
  |ts}

let () =
  Command_unix.run
    (Bench.make_command
       [
         Bench.Test.create ~name:"absurdly long get chunks" (fun _ ->
           let chunks =
             Lsportal.Transformer.Sub_doc.get_chunks Data.absurdly_long_with_chunks rule
           in
           ());
       ])
;;
