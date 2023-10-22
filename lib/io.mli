module Make(Chan : sig

  type input

  type output

  val read_line : input -> string option

  val read_exactly : input -> int -> string option 

  val write : output -> string list -> unit 

end ):sig
  val read : Chan.input -> Jsonrpc.Packet.t option 

  val write : Chan.output -> Jsonrpc.Packet.t -> unit
  end

