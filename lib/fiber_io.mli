open Jsonrpc
open Eio
  type t

  val send : t -> Packet.t list -> unit (*FIber*)

  val recv :  t -> Packet.t option (*FIber*)

  val close : t -> [ `Read | `Write ] -> unit (*FIber*)
  val make: [>`Close|Flow.source_ty] Std.r -> [>`Close|Flow.sink_ty] Std.r ->t
