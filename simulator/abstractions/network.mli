type links         = int array
type network_links = links array
type region        = int
type regions       = int array

(** returns the region assigned to each node for the current simulation *)
val node_regions : unit -> regions

(** returns the links between nodes for the current simulation *)
val node_links : unit -> network_links

module type Network = sig
    type msg

    module MessagesExchanged : Simulator.Statistics.Stats
    module MegabytesExchanged : Simulator.Statistics.Stats

    (** send a message to another node
        used when we need more fine-tuned
        control of message exchanges *)
    val send : int -> int -> msg -> unit

    (** gossip a message to every node in the network
        used for more general purpose use cases, 
        as it assumes the nodes' bandwidth is always free *)
    val gossip : int -> msg -> unit

    (** get the list of neighbour node_ids *)
    val get_neighbours : int -> links

    val clear : unit -> unit

end

module Make : functor (Events: Simulator.Events.Event)(Queue: Simulator.Events.EventQueue with type ev = Events.t)(Message : Simulator.Events.Message with type t = Events.msg) -> (Network with type msg=Events.msg)