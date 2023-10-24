open Jsonrpc

(** Abstraction of some method of reading and writing jsonRpc packets to a server *)
  module type Packet_Channel =sig
    type t

      (**Sends the packets to a jsonRpc server via some method *)

    val send : t -> Packet.t list -> unit (*FIber*)

      (**Recieves packets from a jsonRpc server via some method *)

    val recv : t -> Packet.t option (*FIber*)

      (**Closes the connection to the jsonRpc server *)

    val close : t -> [ `Read | `Write ] -> unit (*FIber*)
end

(** Abstraction of some method of reading and writing string data*)
module type IO_Channel=sig
    type input
    type output

    val read_line : input -> string option
    val read_exactly : input -> int -> string option
    val write : output -> string list -> unit
  end

