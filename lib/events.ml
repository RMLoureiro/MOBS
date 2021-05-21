type nodeID        = int    (* type that identifies a node *)
type regionID      = int    (* type that identifies a region *)
type timeout_label = string (* type that identifies the kind of a timeout *)

(** the type of the events processed during the simulation *)
type event =
  Message of nodeID * nodeID * Clock.t * Messages.message
  | AddNode of nodeID * regionID
  | AddLink of nodeID * nodeID
  | RemoveLink of nodeID * nodeID
  | Timeout of nodeID * Clock.t * timeout_label


(** given an event, returns the timestamp when it should be executed *)
let get_timestamp_from_event (e:event) =
  match e with
  | Message(_,_,timestamp,_) -> timestamp
  | AddNode(_,_) -> Clock.zero
  | AddLink(_,_) -> Clock.zero
  | RemoveLink(_,_) -> Clock.zero
  | Timeout(_,timestamp,_) -> timestamp

