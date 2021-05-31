module type Node = sig
  (** the id of a node *)
  type id = int

  (** the type of the events being handled *)
  type ev

  (** the type representing a node and its state *)
  type t

  (** create the initial state of a node *)
  val init : id -> Abstractions.Network.links -> Abstractions.Network.region -> t

  (** receives the state of a node and an event, returning the resulting state *)
  val handle : t -> ev -> t

  (** compares two nodes *)
  val compare : t -> t -> int
end



module type Initializer = sig
  (** the type representing a node *)
  type node

  (** the type of the events being produced *)
  type ev

  (** returns a list of events that kickstart the simulation *)
  val init : (int, node) Hashtbl.t -> ev list
end



module type Protocol = sig 
  (** the main loop of the protocol *)
  val run : unit -> unit
end



module Make(Event : Simulator.Events.Event)
           (Queue : Simulator.Events.EventQueue with type ev = Event.t)
           (Node : Node with type ev = Event.t) 
           (Initializer : Initializer with type node = Node.t and type ev = Event.t)
           (Logger : Simulator.Logging.Logger with type ev = Event.t) : Protocol
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
    let regions   = Abstractions.Network.node_regions in
    let links     = Abstractions.Network.node_links in
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
      let nodes            = create_nodes () in
      let _                = initial_events nodes in
      let max_height       = ref 0 in
      let end_block_height = !Parameters.General.end_block_height in
      while Queue.has_event () && !max_height < end_block_height do
        let ev = Queue.get_event () in
        match ev with
        | (ts, e) -> 
          Simulator.Clock.set_timestamp ts;
          let index = Event.target e in
          match index with
          | None -> ()
          | Some i -> 
            let new_state = Node.handle (Hashtbl.find nodes i) e in
              Hashtbl.replace nodes i new_state
      done;
      Logger.terminate ()

end