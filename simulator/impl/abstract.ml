type ('a, 'b) template = {
  id:int;
  region : Abstractions.Network.region;
  links  : Abstractions.Network.links;
  mutable state:'b;
  mutable data:'a;
}

module type Node = sig
  type value

  (** the type of the events being handled *)
  type ev

  (** the protocol specific data stored by the node *)
  type node_data

  (** the type representing a node and its state *)
  type t = (node_data, value) template

  (** create the initial state of a node *)
  val init : int -> Abstractions.Network.links -> Abstractions.Network.region -> t

  (** receives the state of a node and an event, returning the resulting state *)
  val handle : t -> ev -> t

  (** compares two nodes *)
  val compare : t -> t -> int

  (** get the state of the node *)
  val state : t -> value

  (** return an integer that identifies the state of the node *)
  val state_id : t -> int

  (** return a JSON string containing the relevant parameters (and values) for the Node's consensus algorithm *)
  val parameters : unit -> string

end

module type V = sig
  type v
end


module MakeBaseNode(V:V) = struct

  module Unique:(Simulator.Unique.Unique with type v = V.v) = Simulator.Unique.Make(V)

  let compare n1 n2 =
    if n1.id < n2.id then -1 else if n1.id > n2.id then 1 else 0
  
  let state n =
    n.state

  let state_id n =
    Unique.id n.state

  let parameters () =
    Parameters.Protocol.get ()

end


module type Initializer = sig
  (** the type representing a node *)
  type node

  (** the type of the events being produced *)
  type ev

  (** returns a list of events that kickstart the simulation *)
  val init : (int, node) Hashtbl.t -> ev list
end



module type Statistics = sig

  (** the type representing events in the simulator *)
  type ev

  (** the type of the values for which consensus is achieved *)
  type value

  (** receives an event and processes it, updating statistics *)
  val process : ev -> unit

  (** returns a JSON string containing the statistics and respective values *)
  val get : unit -> string

  (** node has seen consensus for a value *)
  val consensus_reached : int -> value -> unit

end


module type Protocol = sig 
  (** the main loop of the protocol *)
  val run : unit -> unit
end



module Make(Event : Simulator.Events.Event)
           (Queue : Simulator.Events.EventQueue with type ev = Event.t)
           (Node : Node with type ev = Event.t) 
           (Initializer : Initializer with type node = Node.t and type ev = Event.t)
           (Logger : Simulator.Logging.Logger with type ev = Event.t)
           (Statistics : Statistics with type ev=Event.t and type value = Node.value) : Protocol
           = struct

  module NodeMap = Map.Make(Int)

  (** create the nodes to be used in the simulation, and produce the node and link creation JSON logs *)
  let create_nodes () =
    let log_links links = 
      List.iteri (
        fun i l -> 
          (List.iter (
            fun j -> Logger.log_event (Event.AddLink(i+1, j))
          ) l)
        ) links
    in
    let num_nodes = !Parameters.General.num_nodes in
    let regions   = Abstractions.Network.node_regions () in
    let links     = Abstractions.Network.node_links () in
    let nodes     = Hashtbl.create num_nodes in
    for i = 1 to num_nodes do
      let node_links = List.nth links (i-1) in
      let node_region = List.nth regions (i-1) in
      let node = Node.init i node_links node_region in
      Hashtbl.add nodes i node;
      Logger.log_event (Event.AddNode(i, node_region))
    done;
    log_links links;
    nodes

    (** create the initial events that jumpstart the simulation loop *)
    let initial_events nodes =
      List.iter (fun e -> Queue.add_event e) (Initializer.init nodes)

    let run () =
      Logger.init ();
      Logger.log_parameters (Node.parameters ());
      let nodes            = create_nodes () in
      let _                = initial_events nodes in
      let max_timestamp    = !Parameters.General.max_timestamp in
      let timestamp_limit  = !Parameters.General.timestamp_limit in
      while Queue.has_event () && ((not timestamp_limit) || (Simulator.Clock.get_timestamp () <= max_timestamp)) do
        let ev = Queue.get_event () in
        match ev with
        | (ts, e) -> 
          Logger.log_event e;
          Statistics.process e;
          Simulator.Clock.set_timestamp ts;
          let index = Event.target e in
          match index with
          | None -> ()
          | Some i -> 
            let node_state = Hashtbl.find nodes i in
            let old_value = Node.state node_state in
            let new_state = Node.handle node_state e in
              begin
                let new_value = Node.state new_state in
                if not (old_value = new_value) then
                  begin
                    Logger.print_new_chain_head i i (Node.state_id new_state);
                    Statistics.consensus_reached i new_value
                  end
              end;
              Hashtbl.replace nodes i new_state
      done;
      Logger.log_statistics (Statistics.get ());
      Logger.terminate ()

end