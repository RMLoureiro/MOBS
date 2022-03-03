type timeout_label = string  (* type that identifies the kind of a timeout *)
exception NotFound of string

(** Signature required for a Message module. *)
module type Message = sig
  (** The type of the messages. *)
  type t

  (** Convert a message to a JSON string.
    @param msg the message to be converted
  *)
  val to_json : t -> string

  (** Returns the size of a message.
    @param msg the message whose size we want
  *)
  val get_size : t -> Size.t

  (** Returns the amount of time required to process a message, in millisseconds.
    @param msg the message whose processing time we want
  *)
  val processing_time : t -> int

  (** Returns an integer that identifies the content of a message. 
    @param msg a message
  *)
  val identifier : t -> int

end

(** Module containing required operations to handle events. *)
module type Event = sig
  (** The type of the messages. *)
  type msg

  (** The type of the events processed during the simulation. *)
  type t = 
  Message of int * int * Clock.t * Clock.t * msg (** Message receival event: {b sender}, {b receipient}, {b send_timestamp}, {b receive_timestamp}, {b msg} *)
  | MintBlock of int * Clock.t               (* Block creation event: {b minter_id}, {b timestamp} *)
  | Timeout of int * Clock.t * timeout_label (* Timeout event: {b node_id}, {b timestamp}, {b label} *)

  (** Returns the timestamp when the event should be executed.
    @param event an event
  *)
  val timestamp : t -> Clock.t

  (** Return the node that should process an event.
    @param event an event
  *)
  val target : t -> int option

  (** Create a message receival event.
    @param sender the id of a node
    @param recipient the id of a node
    @param send_ts the timestamp when the message was sent
    @param recept_ts the timestamp when the message will be received
    @param msg the contents of the message
  *)
  val create_message : int -> int -> Clock.t -> Clock.t -> msg -> t

  (** Create a minting event.
    @param minter the id of a node
    @param timestamp the timestamp when the block will be created
  *)
  val create_mint : int -> Clock.t -> t

  (** Create a timeout event.
    @param node_id the id of a node
    @param delay the amount of time that must elapse until the timeout occurs
    @param label the label to be associated with the timeout
  *)
  val create_timeout : int -> Clock.t -> timeout_label -> t
end

(** Functor to create an implementation for Event, given a Message module. *)
module MakeEvent(Msg : Message) : (Event with type msg = Msg.t) = struct
  type msg = Msg.t

  type t =
  Message of int * int * Clock.t * Clock.t * msg
  | MintBlock of int * Clock.t
  | Timeout of int * Clock.t * timeout_label

  let timestamp (e:t) : Clock.t =
  match e with
  | Message(_,_,_,timestamp,_) -> timestamp
  | MintBlock(_,timestamp) -> timestamp
  | Timeout(_,timestamp,_) -> timestamp

  let target (e:t) : int option =
  match e with
  | Message(_,receiver,_,_,_) -> Some receiver
  | MintBlock(node,_) -> Some node
  | Timeout(node,_,_) -> Some node

  let create_message (sender:int) (receiver:int) (send_timestamp:Clock.t) (rcv_timestamp:Clock.t) (m:msg) =
    Message(sender, receiver, send_timestamp, rcv_timestamp, m)

  let create_mint minter timestamp =
    MintBlock(minter, timestamp)

  let create_timeout nodeID delay label =
    Timeout(nodeID, (Clock.get_timestamp ()) + delay, label)

end


(** Contains the operations required for the Event Queue. *)
module type EventQueue = sig
  (** The type of events being stored. *)
  type ev
  
  (** The type of the elements stored in the event queue.
    It is a tuple with the timestamp and the event.
    The timestamp is the key over which elements will be ordered.
  *)
  type elem = (Clock.t * ev)

  (** Returns the next event in the queue (throws exception if the queue is empty). *)
  val get_event : unit -> elem

  (** Add an event to the queue, maintaining the timestamp ordering.
    @param event the event to be added to the queue
  *)
  val add_event : ev -> unit

  (** Returns whether or not there are pending events in the queue. *)
  val has_event : unit -> bool

  (** Cancels a scheduled minting event issued by the minter.
    @param minter the id of the node that created the event
  *)
  val cancel_minting : int -> unit

  (** Clear all data from the queue. *)
  val clear : unit -> unit
end

(** Functor to create an implementation for the EventQueue, given the Event module. *)
module MakeQueue(Event : Event) : (EventQueue with type ev = Event.t) = struct
  type ev = Event.t
  type elem = (Clock.t * ev)

  (* the queue of events *)
  let event_queue:elem list ref = ref []

  let get_event () =
    match !event_queue with
    | x::l -> event_queue := l; x
    | [] -> raise (NotFound "Event queue is empty.")

  let add_event e =
    let e_timestamp = Event.timestamp e in
    let element = (e_timestamp, e) in
    let rec rec_add queue = 
      match queue with
      | [] -> element::[]
      | x::l -> 
        match x with
        | (ts, _) -> if ts > e_timestamp then element::x::l else x::(rec_add l)
    in
    event_queue := rec_add !event_queue

  let has_event () =
    match !event_queue with
    | [] -> false
    | _::_ -> true

  let rec remove_minting_task (nodeID:int) (list:elem list) : elem list =
    match list with
    | [] -> list
    | x::xs -> 
      match x with
      | (_, Event.MintBlock(id,_)) -> if id = nodeID then xs else x::remove_minting_task nodeID xs
      | _ -> x::remove_minting_task nodeID xs

  let cancel_minting nodeID = 
    event_queue := remove_minting_task nodeID !event_queue

  let clear _ = event_queue := []

end
