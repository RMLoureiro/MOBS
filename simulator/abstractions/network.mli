(** The type representing the neighbors of a node. *)
type links         = int array

(** The type representing the neighbors of each node in the simulation. *)
type network_links = links array

(** The type representing the region of a node. *)
type region        = int

(** The type representing the region assigned to each node in the simulation *)
type regions       = int array

(** The type of the Network abstraction.
    Provides primitives for creating the network topology and for abstracting message
    exchanges.
*)
module type Network = sig

    (** The type of the messages being exchanged *)
    type msg

    (** Stats module to keep track of the number of messages exchanged during the simulation *)
    module MessagesExchanged : Simulator.Statistics.Stats
    
    (** Stats module to keep track of the total megabytes exchanged during the simulation *)
    module MegabytesExchanged : Simulator.Statistics.Stats

    (** Primitive for point-to-point message exchange.
        @param sender_node_id the id of the node sending the message
        @param target_node_id the id of the node receiving the message
        @param message the contents of the message being sent
    *)
    val send : int -> int -> msg -> unit

    (** Primitive to send a message to all neighbors of a given node
        @param sender_node_id the id of the node sending the message
        @param message the contents of the message being sent
    *)
    val send_to_neighbors : int -> msg -> unit

    (** Primitive to abstract the gossip protocol. Gossips a message to every
        node in the network. Used for slightly more relaxed scenarios, since it assumes that
        {b limited-bandwidth=false}.
        @param sender_node_id the id of the node sending the message
        @param message the contents of the message being sent
    *)
    val gossip : int -> msg -> unit

    (** Primitive to obtain the neighbors of a given node.
        @param node_id the id of the node whose neighbors we want
    *)
    val get_neighbours : int -> links

    (** Primitive to reset all data stored by the network. *)
    val clear : unit -> unit

    (** Primitive to get the links assigned to each node in the network. *)
    val get_links : unit -> network_links

    (** Primitive to get the region assigned to each node in the network. *)
    val get_regions : unit -> regions

end

(** Functor that creates an implementation for the Network, given the Events, EventQueue and Message modules. *)
module Make : functor (Events: Simulator.Events.Event)(Queue: Simulator.Events.EventQueue with type ev = Events.t)(Message : Simulator.Events.Message with type t = Events.msg) -> (Network with type msg=Events.msg)