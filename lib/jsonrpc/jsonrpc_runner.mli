module Reply = Import.Reply
module Notify = Import.Notify
module Flow_io = Flow_io
module Log = Log
module Json = Import.Json

module Make : functor (Chan : Io_types.Packet_Channel) -> sig
  type nextAction = Run_rpc.Make(Chan).nextAction =
    | Stop
    | Loop


(**Start the Rpc Server. Will block until the server is stopped. 
    Starts a new Eio Switch Internally for the Server *)
  val run : 'a Core.Core(Chan).t -> unit

  exception Done of string

  type 'state t = 'state Core.Core(Chan).t = {
    chan : Chan.t;
    on_request :
      ('state, Jsonrpc.Request.t) context -> Reply.t * 'state;
    on_notification :
      ('state, Jsonrpc.Notification.t) context ->
      Notify.t * 'state;
    pending :
      (Jsonrpc.Response.t, [ `Cancelled | `Stopped ]) result
      Import.prom
      Import.Id.Table.t;
    stopped : unit Import.prom;
    name : string;
    mutable running : bool;
    mutable tick : int;
    mutable state : 'state;
    mutable pending_requests_stopped : bool;
  }

  and ('a, 'message) context = 'a t * 'message

  val fire : 'a -> 'a


  val state : 'a t -> 'a
  val stop : 'a t -> unit
  val close : 'a t -> unit

module Batch:sig
    type t
end

  val submit : 'a t -> Batch.t -> unit

    (**Creates a Rpc conncection over the provided channel.
     the on_request handler will trigger on recieving a request
     the on_notifiction handler will be called on recieving a notifiction
     if you don't register a handler they will throw an error
     (I think this is for if your rpc connection is just supposed to be a client.
     that way it will throw if it recieves a message that should be meant for a server)*)
  val create :
    ?on_request:
      (('a, Jsonrpc.Request.t) context -> Reply.t * 'a) ->
    ?on_notification:
      (('a, Jsonrpc.Notification.t) context -> Notify.t * 'a) ->
    name:string ->
    Chan.t ->
    'a ->
    'a t

      (** Useses this Rpc connection to send a notifiction out via the output stream*)
  val notification : 'a t -> Jsonrpc.Notification.t -> unit

    (** Useses this Rpc connection to send a request out via the output stream*)
  val request : 'a t -> Jsonrpc.Request.t -> Jsonrpc.Response.t

  val request_with_cancel :
    'a t ->
    Jsonrpc.Request.t ->
    unit * [> `Cancelled | `Ok of Jsonrpc.Response.t ]
end
