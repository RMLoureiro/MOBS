(* the type of the elements stored in the event queue *)
(* it is a tuple with the timestamp and the event *)
(* the timestamp is the key over which elements will be ordered *)
type elem = (Clock.t * Events.event)

(* exception representing that no elements are in the queue, and we perform a get operation *)
exception NotFound of string

(** returns the next event in the queue (throws exception if the queue is empty) *)
val get_event : unit -> elem

(** add an event to the queue, maintaining the timestamp ordering *)
val add_event : Events.event -> unit