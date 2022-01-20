type links         = int array
type network_links = links array
type region        = int
type regions       = int array

module type Network = sig
    type msg

    module MessagesExchanged : Simulator.Statistics.Stats
    module MegabytesExchanged : Simulator.Statistics.Stats

    (** send a message to another node
        used when we need more fine-tuned
        control of message exchanges *)
    val send : int -> int -> msg -> unit

    (** send a message to all neighbors of a given node *)
    val send_to_neighbors : int -> msg -> unit

    (** gossip a message to every node in the network
        used for more general purpose use cases, 
        as it assumes the nodes' bandwidth is always free *)
    val gossip : int -> msg -> unit

    (** get the list of neighbour node_ids *)
    val get_neighbours : int -> links

    val clear : unit -> unit

    (** obtain the links assigned to each node *)
    val get_links : unit -> network_links

    (** obtain the region assigned to each node*)
    val get_regions : unit -> regions

end

module Make : functor (Events: Simulator.Events.Event)(Queue: Simulator.Events.EventQueue with type ev = Events.t)(Message : Simulator.Events.Message with type t = Events.msg) -> (Network with type msg=Events.msg)