type links         = int list
type network_links = links list
type region        = int
type regions       = int list

(** returns the region assigned to each node for the current simulation *)
val node_regions : unit -> regions

(** returns the links between nodes for the current simulation *)
val node_links : unit -> network_links

module type Network = sig
type msg

(** send a message to another node *)
val send : int -> int -> msg -> unit
end

module Make : functor (Events: Simulator.Events.Event)(Queue: Simulator.Events.EventQueue with type ev = Events.t) -> (Network with type msg=Events.msg)