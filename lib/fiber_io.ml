open Eio.Std
open Eio
open Base

(*ELI:
  This is an example of a functor, we are making modules Mycahnnel that conforms to the spec in IO.Make and then providing it
*)
module MyChannel = struct
  open Eio

  type input = Buf_read.t

  type output = Buf_write.t

  let read_line ic =
    match Eio.Buf_read.line ic with
    | str -> Some str
    | exception e ->
        traceln "exception reading line%s" (Printexc.to_string e) ;
        None

  let read_exactly ic len =
    match Eio.Buf_read.take len ic with str -> Some str | exception exn ->
      traceln "exception reading exactly%s" (Printexc.to_string exn) ;
       None

  let write oc strings =
    strings |> List.iter ~f:(Eio.Buf_write.string oc)
end

module Io = Io.Make (MyChannel)

(* this [`x|`y] thing seems like  set of type constraints *)
(*we add the close in here becuase we need to support closing of these flows via the close method*)
type source = Buf_read.t

type sink = [`Close | Flow.sink_ty] r

type t = source * sink * Eio.Mutex.t

let send ((_, oc, m) : t) packets =
  (*  traceln
  ( List.map packets ~f:(fun x ->
          x |> Jsonrpc.Packet.yojson_of_t |> Jsonrpc_fiber.Json.to_pretty_string )
    |> String.concat ) ;*)
  Eio.Mutex.use_rw ~protect:true m (fun () ->
      Buf_write.with_flow oc (fun writer ->
          packets |> List.iter ~f:(Io.write writer) ;
           ) )

let recv (ic, _, _) = ic |> Io.read

let make ic oc : t =
  (*these conversions are necissary and were a massive pain to figure out. they allow the input to be generic and polymorphic but the stored value to be a singular version*)
  ((ic |>Buf_read.of_flow ~max_size:100000 ), (oc :> sink), Eio.Mutex.create ())

let close ((ic, oc, _) : t) what =
  (match what with `Write -> Flow.close oc | `Read -> (*not sure if i need a proper way to close this*)()) ;
  ()
