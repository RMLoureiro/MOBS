
(** Proposal Component. *)
module type Proposal = sig
  type message (** Contents of a message *)

  type node    (** State of a node *)

  (** Attempt to generate a block proposal message
    @param node the state of a node
  *)
  val create_proposal : node -> message option
end

(** Validation Component. *)
module type Validation = sig
  type message (** Contents of a message *)

  type node    (** State of a node *)

  (** Check the validity of a message.
    @param node the state of a node
    @param message the message to be validated
  *)
  val validate : node -> message -> bool
end

(** Propagation Component. *)
module type Propagation = sig
  type message (** Contents of a message *)

  type node    (** State of a node *)

  (** Returns the messages that can be processed from the node's message queue.
    We don't simply use the ones in the message queue by default,
    to allow users to specify the behavior of some messages
    only being processed at certain node states.
    @param node the state of a node
    @param messages a reference to the message queue
  *)
  val receive : node -> message list ref -> message list

  (** Propagate a message through the network.
    @param node the state of a node
    @param message the message to be propagated
  *)
  val propagate : node -> message -> unit
end

(** Finalization Component. *)
module type Finalization = sig
  type message (* contents of a message *)
  
  type node    (* state of a node *)

  (** Process a received message. Reach consensus
    on a block to be added to the chain.
    @param node the state of a node
    @param message the message to be processed  
  *)
  val process : node -> message -> node
end

(** Contains the state of a node and initialization operations. *)
module type FCFNode = sig
  (** The protocol specific data stored by the node *)
  type node_data

  (** The type of values for which consensus is being reached *)
  type value

  (** Contents of a message *)
  type message 

  (** The type representing a node and its state *)
  type t = (node_data, value) Protocol.template

  (** The type of events in the simulator *)
  type ev
  
  (** Create the initial state of a node.
    @param node_id the id to be assigned to the node
    @param neighbors the node's neighbors
    @param region the node's region
  *)
  val init : int -> Abstractions.Network.links -> Abstractions.Network.region -> t

  (** Obtain the height of the chain stored in the node.
    @param node the state of the node
  *)
  val chain_height : t -> int
end

(** Functor to create a BlockchainNode implementation, given each of the individual Five-Component Framework modules.  *)
module Make
  (Event : Simulator.Events.Event)
  (Node         : FCFNode      with type ev    = Event.t and type message = Event.msg)
  (Proposal     : Proposal     with type node  = Node.t  and type message = Node.message) 
  (Validation   : Validation   with type node  = Node.t  and type message = Node.message)
  (Propagation  : Propagation  with type node  = Node.t  and type message = Node.message)
  (Finalization : Finalization with type node  = Node.t  and type message = Node.message)
  : (Protocol.BlockchainNode   with type value = Node.value and type node_data = Node.node_data and type ev = Node.ev) = struct

  type value = Node.value
  type node_data = Node.node_data
  type t = Node.t
  type ev = Node.ev

  module V = struct
    type v = value
  end

  include Protocol.MakeBaseNode(V)

  let message_queue : (Node.message list) ref = ref []

  let init = Node.init

  let chain_height = Node.chain_height

  let step (node:Node.t) : Proposal.node =
    let new_state = ref 
      (
        match Proposal.create_proposal node with
        | Some(message) -> Propagation.propagate node message; Finalization.process node message
        | None -> node
      )
    in
    let rcvd_messages = Propagation.receive node message_queue in
    List.iter (
      fun msg -> 
        if Validation.validate !new_state msg then 
          new_state := Finalization.process !new_state msg
    ) rcvd_messages;
    !new_state

  let handle (node:t) (e:ev) : t =
    match e with
    | Message(_,_,_,_,msg) -> message_queue := !message_queue@[msg]; step node
    | _ -> step node

end

