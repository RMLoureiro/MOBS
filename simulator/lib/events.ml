type timeout_label = string  (* type that identifies the kind of a timeout *)
exception NotFound of string

module type Message = sig
  (** the type of a message *)
  type t

  (** convert a message to JSON string *)
  val to_json : t -> string

  (** returns the size of a message *)
  val get_size : t -> Size.t

  (** returns the amount of time required to process a message, in millisseconds *)
  val processing_time : t -> int

  (** given a message, produce an integer that identifies the content contained in the message *)
  (* for example, the id of a block *)
  val identifier : t -> int

end

module type Event = sig
  (** the type of the message contents *)
  type msg

  (** the type of the events processed during the simulation *)
  type t = 
  Message of int * int * Clock.t * msg       (* nodeID(sender), nodeID(receiver), timestamp, msg *)
  | MintBlock of int * Clock.t               (* nodeID, timestamp *)
  | Timeout of int * Clock.t * timeout_label (* nodeID, timestamp, label *)

  (** given an event, returns the timestamp when it should be executed *)
  val timestamp : t -> Clock.t

  (** given an event, return the node that should process the event *)
  val target : t -> int option

  (** given the sender, receiver, timestamp and message contents, create a message event *)
  val create_message : int -> int -> Clock.t -> msg -> t

  (** given the minter, timestamp and block, create a minting event *)
  val create_mint : int -> Clock.t -> t

  (** given a node's id and a delay, creates a timeout event *)
  val create_timeout : int -> Clock.t -> timeout_label -> t
end

module MakeEvent(Msg : Message) : (Event with type msg = Msg.t) = struct
  type msg = Msg.t

  type t =
  Message of int * int * Clock.t * msg
  | MintBlock of int * Clock.t
  | Timeout of int * Clock.t * timeout_label

  let timestamp (e:t) : Clock.t =
  match e with
  | Message(_,_,timestamp,_) -> timestamp
  | MintBlock(_,timestamp) -> timestamp
  | Timeout(_,timestamp,_) -> timestamp

  let target (e:t) : int option =
  match e with
  | Message(_,receiver,_,_) -> Some receiver
  | MintBlock(node,_) -> Some node
  | Timeout(node,_,_) -> Some node

  let create_message (sender:int) (receiver:int) (timestamp:Clock.t) (m:msg) =
    Message(sender, receiver, timestamp, m)

  let create_mint minter timestamp =
    MintBlock(minter, timestamp)

  let create_timeout nodeID delay label =
    Timeout(nodeID, (Clock.get_timestamp ()) + delay, label)

end


module type EventQueue = sig
  type ev
  
  (* the type of the elements stored in the event queue *)
  (* it is a tuple with the timestamp and the event *)
  (* the timestamp is the key over which elements will be ordered *)
  type elem = (Clock.t * ev)

  (** returns the next event in the queue (throws exception if the queue is empty) *)
  val get_event : unit -> elem

  (** add an event to the queue, maintaining the timestamp ordering *)
  val add_event : ev -> unit

  (** returns whether or not there are pending events in the queue *)
  val has_event : unit -> bool

  (** cancels a scheduled minting event *)
  val cancel_minting : int -> unit

  (** cancels a scheduled timeout event *)
  val cancel_timer : int -> timeout_label -> unit
end

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

  let rec remove_timeout_event (nodeID:int) (list:elem list) (label:timeout_label) : elem list =
    match list with
    | [] -> list
    | x::xs -> 
      match x with
      | (_, Event.Timeout(id,_,l)) -> if id = nodeID && l = label then xs else x::remove_timeout_event nodeID xs label
      | _ -> x::remove_timeout_event nodeID xs label

  let cancel_timer nodeID label =
    event_queue := remove_timeout_event nodeID !event_queue label

end
