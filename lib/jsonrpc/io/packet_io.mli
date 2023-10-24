open Import
(**Implimentation of the Packet_channel module type for the Jsonrpc module.
  This handles parsing the content headers and reading the data for each indifidual jsonrpc packet
   *)
module Make(Chan : Io_types.IO_Channel):sig
  val read : Chan.input -> Jsonrpc.Packet.t option 

  val write : Chan.output -> Jsonrpc.Packet.t -> unit
  end
