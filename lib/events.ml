type nodeID        = int (* type that identifies a node *)
type regionID      = int (* type that identifies a region *)
type timeout_label = string (* type that identifies the kind of a timeout *)

(* the type of the events processed during the simulation *)
type event =
  Message of nodeID * nodeID * Clock.t_timestamp * Messages.message
  | AddNode of nodeID * regionID
  | AddLink of nodeID * nodeID
  | RemoveLink of nodeID * nodeID
  | Timeout of nodeID * Clock.t_timestamp * timeout_label


(* given an event, returns the timestamp when it should be executed *)
let get_timestamp_from_event (e:event) =
  match e with
  | Message(_,_,timestamp,_) -> timestamp
  | AddNode(_,_) -> 0
  | AddLink(_,_) -> 0
  | RemoveLink(_,_) -> 0
  | Timeout(_,timestamp,_) -> timestamp

