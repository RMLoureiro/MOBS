(** The base type of a node. *)
type ('a, 'b) template = {
  id:int; (** The node's id *)
  region : Abstractions.Network.region; (** The node's region *)
  links  : Abstractions.Network.links; (** The node's neighbors *)
  mutable state:'b; (** The latest value for which there was consensus *)
  mutable data:'a; (** User-defined state required by the node to perform the protocol *)
}

(** Base node for a consensus protocol. *)
module type AbstractNode = sig
  (** The type of the values for which consensus is being reached. *)
  type value

  (** The type of the events being handled. *)
  type ev

  (** The protocol specific data stored by the node. *)
  type node_data

  (** The type representing a node and its state. Consists of apllying the {b value} and {b node_data} to the {b template}. *)
  type t = (node_data, value) template

  (** Create the initial state of a node.
    @param node_id the id to be assigned to the node
    @param neighbors the node's neighbors
    @param region the node's region
  *)
  val init : int -> Abstractions.Network.links -> Abstractions.Network.region -> t

  (** Receives the state of a node and an event, returning the resulting state.
    @param node the state of a node
    @param event the event to be processed by the node
  *)
  val handle : t -> ev -> t

  (** Compares two nodes. Returns whether they are in the same state.
    @param node1 the state of a node
    @param node2 the state of a node
  *)
  val compare : t -> t -> int

  (** Get the consensus target of a node.
    @param node the state of a node
  *)
  val state : t -> value

  (** Return an integer that identifies the state of the node.
    @param node the state of a node
  *)
  val state_id : t -> int

  (** Return a JSON string containing the relevant parameters (and values) for the Node's consensus algorithm *)
  val parameters : unit -> string

end

(** Extension of the base node, for blockchain protocols. *)
module type BlockchainNode = sig
  include AbstractNode

  (** Obtain the height of the chain stored in the node.
    @param node the state of a node
  *)
  val chain_height : t -> int

end

(** Wrapper for the type of values for which consensus is being reached. *)
module type V = sig
  type v (** the type of the values for which consensus is being reached. *)
end

(** Functor that receives the wrapper V, and constructs implementation for several 
  operations of a node, without the user having to implement them by hand.
*)
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

(** Initializer module. In charge of generating the initial events to start the simulation. *)
module type Initializer = sig

  (** The type representing a node. *)
  type node

  (** The type of the events being produced. *)
  type ev

  (** Returns a list of events that kickstart the simulation.
    @param nodes an hashtable containing the state of each node
  *)
  val init : (int, node) Hashtbl.t -> ev list
end

(** The Protocol module contains the high-level loop of the simulation, and initialization operations. *)
module type Protocol = sig 
  (** The main loop of the simulation. *)
  val run : unit -> unit
end


module Make = struct

  (** Functor to create auxiliary functions used by Abstract and Blockchain Protocols. *)
  module Auxiliary(Event : Simulator.Events.Event)
                  (Queue : Simulator.Events.EventQueue with type ev = Event.t)
                  (Logger : Simulator.Logging.Logger with type ev = Event.t)
                  (Network : Abstractions.Network.Network) = struct

    (** Create the nodes to be used in the simulation, and produce the node and link creation JSON logs.
        @param init function to create the initial state of a node
    *)
    let create_nodes init =
      let log_links links = 
        Array.iteri (
          fun i l -> 
            (Array.iter (
              fun j -> Logger.log_add_link (i) j
            ) l)
          ) links
      in
      let num_nodes = !Parameters.General.num_nodes in
      print_endline "\t Assigning node regions...";
      let regions   = Network.get_regions () in
      print_endline "\t [DONE] Assigning node regions";
      print_endline "\t Creating links between nodes...";
      let links     = Network.get_links () in
      print_endline "\t [DONE] Creating links between nodes";
      print_endline "\t Initializing nodes...";
      let nodes     = Hashtbl.create num_nodes in
      for i = 0 to num_nodes-1 do
        let node_links = links.(i) in
        let node_region = regions.(i) in
        let node = init i node_links node_region in
        Hashtbl.add nodes i node;
        Logger.log_add_node i node_region
      done;
      print_endline "\t [DONE] Initializing nodes";
      log_links links;
      nodes

      (** Add the initial events that jumpstart the simulation loop to the Event Queue.
        @param events the list of events to be added to the queue
      *)
      let add_initial_events events =
        List.iter (fun e -> Queue.add_event e) events

  end

  (** According to the parametrizations, initialize data structures to keep track of which nodes are offline, and when they become online/offline. *)
  let initialize_offline_node_data () =
    let offline:((int*int) array) = Array.init (!Parameters.General.num_nodes+1) (fun _ -> (0,0)) in
    if !Parameters.General.use_topology_file then
      (
        let open Yojson.Basic.Util in
        let node_data = Parameters.General.parse_topology_file !Parameters.General.topology_filename in
        List.iter (
          fun node ->
            let node_id = node |> member "id" |> to_int in
            let offline_struct = node |> member "offline" in
            let offline_from = offline_struct |> member "from" |> to_int in
            let offline_to = offline_struct |> member "to" |> to_int in
            offline.(node_id) <- (offline_from, offline_to)
        ) node_data;
        offline
      )
    else
      (
        let num_offline_nodes = !Parameters.General.num_offline_nodes in
        let become_offline_timestamp = !Parameters.General.become_offline_timestamp in
        let become_online_timestamp = !Parameters.General.become_online_timestamp in
        let selected_nodes = ref 0 in
        let selected = ref [] in
        while !selected_nodes < num_offline_nodes do
          let i = (Random.int !Parameters.General.num_nodes) in
          if not (List.exists (fun x -> x=i) !selected) then
            (
              offline.(i) <- (become_offline_timestamp, become_online_timestamp);
              selected_nodes := !selected_nodes +1
            )
        done;
        offline
      )

  (** According to the parametrizations, initialize data structures to keep track of which nodes are malicous, and when they start acting maliciously. *)
  let initialize_malicious_node_data () =
    let malicious:((bool*int) array) = Array.init (!Parameters.General.num_nodes+1) (fun _ -> (false,0)) in
    if !Parameters.General.use_topology_file then
      (
        let open Yojson.Basic.Util in
        let node_data = Parameters.General.parse_topology_file !Parameters.General.topology_filename in
        List.iter (
          fun node ->
            let node_id = node |> member "id" |> to_int in
            let malicious_struct = node |> member "malicious" in
            let is_malicious = malicious_struct |> member "isbad" |> to_bool in
            let malicious_ts = malicious_struct |> member "start" |> to_int in
            malicious.(node_id) <- (is_malicious, malicious_ts)
        ) node_data;
        malicious
      )
    else
      (
        let num_malicious_nodes = !Parameters.General.num_bad_nodes in
        let become_malicious_timestamp = !Parameters.General.become_bad_timestamp in
        let selected_nodes = ref 0 in
        while !selected_nodes < num_malicious_nodes do
          let i = (Random.int !Parameters.General.num_nodes) in
          match malicious.(i) with
          | (false,_) -> malicious.(i) <- (true, become_malicious_timestamp); selected_nodes := !selected_nodes +1
          | _ -> ()
        done;
        malicious
      )

  
  (** Argument for Statistics module that keeps track of the average time to reach consensus. *)
  module ConsensusArg = struct
    let label = "avg-consensus-time"
    let use_intervals = true
    let format = 1
  end

  (** Argument for Statistics module that keeps track of the number of events processed per node. *)
  module EventsPerNodeArg = struct
    let label = "events-processed-per-node"
    let use_intervals = true
    let format = 0
  end

  (*********************************************************************************)

  (** Functor to create and implementation for an AbstractProtocol (protocol that simulates AbstractNodes). *)
  module Abstract(Event : Simulator.Events.Event)
           (Queue : Simulator.Events.EventQueue with type ev = Event.t)
           (Timer : Abstractions.Timer.Timer)
           (GoodNode : AbstractNode with type ev = Event.t)
           (BadNode : AbstractNode with type ev = Event.t and type value = GoodNode.value and type node_data = GoodNode.node_data)
           (Initializer : Initializer with type node = GoodNode.t and type ev = Event.t)
           (Logger : Simulator.Logging.Logger with type ev = Event.t)
           (Statistics : Simulator.Statistics.Stats)
           (Network : Abstractions.Network.Network) : Protocol
           = struct

  module NodeMap = Map.Make(Int)
  module ConsensusStats = Simulator.Statistics.Make.Average(ConsensusArg)
  module EventsPerNode = Simulator.Statistics.Make.CountPerNode(EventsPerNodeArg)

  module MakeStep(Node: AbstractNode with type ev = Event.t and type value = GoodNode.value and type node_data = GoodNode.node_data) = struct

    (** A step in the simulation. Consists of assigning an event to a node, and given permission for that node to process the event.
      @param timestamp the current timestamp
      @param event the event to be processed
      @param nodes the hashtable containing the state of all nodes being simulated
    *)
    let step ts e nodes =
      begin
        Logger.log_event e;
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
                  ConsensusStats.process i (Simulator.Clock.get_timestamp ())
                end
            end;
            Hashtbl.replace nodes i new_state
      end

    end

    (** Main loop of the simulation. Also performs initializations, logging and statistics computations. *)
    let run () =
      Simulator.Clock.reset ();
      Timer.clear ();
      Queue.clear ();
      Logger.init ();
      Logger.log_protocol 0;
      print_endline "Parsing simulation parameters...";
      Logger.log_parameters (GoodNode.parameters ());
      print_endline "[DONE] Parsing simulation parameters";
      print_endline "Creating nodes...";
      let module Aux = Auxiliary(Event)(Queue)(Logger)(Network) in
      let nodes            = Aux.create_nodes GoodNode.init in
      let malicious_data   = initialize_malicious_node_data () in
      let offline_data     = initialize_offline_node_data () in
      print_endline "[DONE] Creating nodes";
      print_endline "Creating initial events...";
      Network.clear ();
      let _                = Aux.add_initial_events (Initializer.init nodes) in
      print_endline "[DONE] Creating initial events";
      print_endline "Running simulation...";
      let max_timestamp    = !Parameters.General.max_timestamp in
      let timestamp_limit  = !Parameters.General.timestamp_limit in
      let module GoodStep  = MakeStep(GoodNode) in
      let module BadStep   = MakeStep(BadNode) in
      let step ts e = 
        let index = Event.target e in
        match index with
        | None -> ()
        | Some i -> 
          (
            match offline_data.(i) with
            | (bg,ed) -> 
              if ts <= bg || ts >= ed then
                (
                  EventsPerNode.process i 1;
                  match malicious_data.(i) with
                  | (true,t) -> if ts >= t then BadStep.step ts e nodes else GoodStep.step ts e nodes
                  | _ -> GoodStep.step ts e nodes
                )
          )
          
      in
      while Queue.has_event () && ((not timestamp_limit) || (Simulator.Clock.get_timestamp () <= max_timestamp)) do
        let ev = Queue.get_event () in
        match ev with
        | (ts, e) -> 
          match e with
          | Event.Timeout(id,time,label) -> 
            begin
              if not (Timer.expired id time label) then step ts e
            end
          | _ -> step ts e
      done;
      print_endline "\t Reached stopping condition";
      let module NetworkStats = Simulator.Statistics.Compose(Network.MessagesExchanged)(Network.MegabytesExchanged) in
      let module FinalStatistics = Simulator.Statistics.Compose(Simulator.Statistics.Compose(ConsensusStats)(Statistics))(NetworkStats) in
      Logger.log_statistics (FinalStatistics.get ());
      Logger.log_per_node_statistics (EventsPerNode.get ());
      Logger.terminate ();
      NetworkStats.clear ();
      FinalStatistics.clear ();
      EventsPerNode.clear ()

  end


  (*********************************************************************************)

  (** Functor to create and implementation for a BlockchainProtocol (protocol that simulates BlockchainNodes). *)
  module Blockchain(Event : Simulator.Events.Event)
           (Queue : Simulator.Events.EventQueue with type ev = Event.t)
           (Block : Simulator.Block.BlockSig)
           (Timer : Abstractions.Timer.Timer)
           (GoodNode : BlockchainNode with type ev = Event.t and type value = Block.block)
           (BadNode : BlockchainNode with type ev = Event.t and type value = GoodNode.value and type node_data = GoodNode.node_data)
           (Initializer : Initializer with type node = GoodNode.t and type ev = Event.t)
           (Logger : Simulator.Logging.Logger with type ev = Event.t)
           (Statistics : Simulator.Statistics.Stats)
           (Network : Abstractions.Network.Network) : Protocol
           = struct

  module NodeMap  = Map.Make(Int)
  module ConsensusStats = Simulator.Statistics.Make.Average(ConsensusArg)
  module EventsPerNode = Simulator.Statistics.Make.CountPerNode(EventsPerNodeArg)

  module MakeStep(Node: BlockchainNode with type ev = Event.t and type value = GoodNode.value and type node_data = GoodNode.node_data) = struct
    
    (** A step in the simulation. Consists of assigning an event to a node, and given permission for that node to process the event.
      @param timestamp the current timestamp
      @param event the event to be processed
      @param nodes the hashtable containing the state of all nodes being simulated
    *)
    let step ts e nodes max_height =
      begin
        Logger.log_event e;
        Simulator.Clock.set_timestamp ts;
        let index = Event.target e in
        match index with
        | None -> ()
        | Some i -> 
          let node_state = Hashtbl.find nodes i in
          let old_chain_head = Node.state node_state in
          let new_state = Node.handle node_state e in
            begin 
              if Node.chain_height new_state > !max_height then
                begin
                max_height := Node.chain_height new_state;
                let s = Printf.sprintf "\t Longest Chain Height Observed: %d" !max_height in
                print_endline s
                end
            end;
            begin
              let new_chain_head = Node.state new_state in
              if not (Block.equals old_chain_head new_chain_head) then
                begin
                  Logger.print_new_chain_head i (Block.minter new_chain_head) (Block.id new_chain_head);
                  ConsensusStats.process i (Simulator.Clock.get_timestamp ())
                end
            end;
            Hashtbl.replace nodes i new_state;
      end

    end

    (** Main loop of the simulation. Also performs initializations, logging and statistics computations. *)
    let run () =
      Simulator.Clock.reset ();
      Timer.clear ();
      Queue.clear ();
      Logger.init ();
      Logger.log_protocol 1;
      print_endline "Parsing simulation parameters...";
      Logger.log_parameters (GoodNode.parameters ());
      print_endline "[DONE] Parsing simulation parameters";
      print_endline "Creating nodes...";
      let module Aux = Auxiliary(Event)(Queue)(Logger)(Network) in
      let nodes            = Aux.create_nodes GoodNode.init in
      let malicious_data   = initialize_malicious_node_data () in
      let offline_data     = initialize_offline_node_data () in
      print_endline "[DONE] Creating nodes";
      print_endline "Creating initial events...";
      Network.clear ();
      let _                = Aux.add_initial_events (Initializer.init nodes) in
      print_endline "[DONE] Creating initial events";
      print_endline "Running simulation...";
      let max_height       = ref 0 in
      let end_block_height = !Parameters.General.end_block_height in
      let max_timestamp    = !Parameters.General.max_timestamp in
      let timestamp_limit  = !Parameters.General.timestamp_limit in
      let module GoodStep  = MakeStep(GoodNode) in
      let module BadStep   = MakeStep(BadNode) in
      let step ts e = 
        let index = Event.target e in
        match index with
        | None -> ()
        | Some i -> 
          (
            match offline_data.(i) with
            | (bg,ed) -> 
              if ts <= bg || ts >= ed then
                (
                  EventsPerNode.process i 1;
                  match malicious_data.(i) with
                  | (true,t) -> if ts >= t then BadStep.step ts e nodes max_height else GoodStep.step ts e nodes max_height
                  | _ -> GoodStep.step ts e nodes max_height
                )
          )
      in
      while Queue.has_event () && !max_height < end_block_height && ((not timestamp_limit) || (Simulator.Clock.get_timestamp () <= max_timestamp)) do
        let ev = Queue.get_event () in
        match ev with
        | (ts, e) -> 
          match e with
          | Event.Timeout(id,time,label) -> 
            begin
              if not (Timer.expired id time label) then step ts e
            end
          | _ -> step ts e
      done;
      print_endline "\t Reached stopping condition";
      let module NetworkStats = Simulator.Statistics.Compose(Network.MessagesExchanged)(Network.MegabytesExchanged) in
      let module FinalStatistics = Simulator.Statistics.Compose(Simulator.Statistics.Compose(ConsensusStats)(Statistics))(NetworkStats) in
      Logger.log_statistics (FinalStatistics.get ());
      Logger.log_per_node_statistics (EventsPerNode.get ());
      Logger.terminate ();
      NetworkStats.clear ();
      FinalStatistics.clear ();
      EventsPerNode.clear ()

  end

end