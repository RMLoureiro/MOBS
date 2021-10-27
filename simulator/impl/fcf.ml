
module type Proposal = sig
  type message (* contents of a message *)
  type node    (* state of a node *)

  (* attempt to generate a block proposal message *)
  val create_proposal : node -> message option
end

module type Validation = sig
  type message (* contents of a message *)
  type node    (* state of a node *)

  (* check the validity of a message *)
  val validate : node -> message -> bool
end

module type Propagation = sig
  type message (* contents of a message *)
  type node    (* state of a node *)

  (* gets pending messages from the node's message queue *)
  (* we don't simply use the ones in the message queue by default,
     to allow users to specify the behavior of some messages
     only being processed at certain node states *)
  val receive : node -> message list ref -> message list
  (* propagate a message through the network *)
  val propagate : node -> message -> unit
end

module type Finalization = sig
  type message (* contents of a message *)
  type node    (* state of a node *)

  (* process received messages *)
  (* reach consensus on a block to be added to the chain *)
  val process : node -> message -> node
end

module type FCFNode = sig
  (** the protocol specific data stored by the node *)
  type node_data
  (** the type of values for which consensus is being reached *)
  type value
  (* contents of a message *)
  type message 
  (** the type representing a node and its state *)
  type t = (node_data, value) Protocol.template
  (** the type of events in the simulator *)
  type ev
  
  (** create the initial state of a node *)
  val init : int -> Abstractions.Network.links -> Abstractions.Network.region -> t
  (** obtain the height of the chain stored in the node *)
  val chain_height : t -> int
end

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
    | Message(_,_,_,msg) -> message_queue := !message_queue@[msg]; step node
    | _ -> step node

end

