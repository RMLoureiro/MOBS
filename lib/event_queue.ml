type elem = (Clock.t * Events.event)

(* the queue of events *)
let event_queue:elem list ref = ref []

exception NotFound of string

let get_event () =
  match !event_queue with
  | x::l -> event_queue := l; x
  | [] -> raise (NotFound "Event queue is empty.")

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









