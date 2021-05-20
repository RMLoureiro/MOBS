(* the type of the elements stored in the event queue *)
(* it is a tuple with the timestamp and the event *)
(* the timestamp is the key over which elements will be ordered *)
type elem = (Clock.t_timestamp * Events.event)

(* the queue of events *)
let event_queue:elem list ref = ref []

(* exception representing that no elements are in the queue, and we perform a get operation *)
exception NotFound of string

(* returns the next event in the queue (throws exception if the queue is empty) *)
let get_event () =
  match !event_queue with
  | x::l -> event_queue := l; x
  | [] -> raise (NotFound "Event queue is empty.")

(* add an event to the queue, maintaining the timestamp ordering *)
let add_event (e:Events.event) =
  let e_timestamp = Events.get_timestamp_from_event e in
  let element = (e_timestamp, e) in
  let rec rec_add queue = 
    match queue with
    | [] -> element::[]
    | x::l -> 
      match x with
      | (ts, _) -> if ts > e_timestamp then element::x::l else x::(rec_add l)
  in
  event_queue := rec_add !event_queue









