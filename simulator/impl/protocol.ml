type ('a, 'b) template = {
  id:int;
  region : Abstractions.Network.region;
  links  : Abstractions.Network.links;
  mutable state:'b;
  mutable data:'a;
}

module type AbstractNode = sig
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

module type BlockchainNode = sig
  include AbstractNode

  (** obtain the height of the chain stored in the node *)
  val chain_height : t -> int

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

module type Protocol = sig 
  (** the main loop of the protocol *)
  val run : unit -> unit
end


module Make = struct

  module Auxiliary(Event : Simulator.Events.Event)
                  (Queue : Simulator.Events.EventQueue with type ev = Event.t)
                  (Logger : Simulator.Logging.Logger with type ev = Event.t) = struct

    (** create the nodes to be used in the simulation, and produce the node and link creation JSON logs *)
    let create_nodes init =
      let log_links links = 
        Array.iteri (
          fun i l -> 
            (Array.iter (
              fun j -> Logger.log_add_link (i+1) j
            ) l)
          ) links
      in
      let num_nodes = !Parameters.General.num_nodes in
      print_endline "\t Assigning node regions...";
      let regions   = Abstractions.Network.node_regions () in
      print_endline "\t [DONE] Assigning node regions";
      print_endline "\t Creating links between nodes...";
      let links     = Abstractions.Network.node_links () in
      print_endline "\t [DONE] Creating links between nodes";
      print_endline "\t Initializing nodes...";
      let nodes     = Hashtbl.create num_nodes in
      for i = 1 to num_nodes do
        let node_links = links.(i-1) in
        let node_region = regions.(i-1) in
        let node = init i node_links node_region in
        Hashtbl.add nodes i node;
        Logger.log_add_node i  node_region
      done;
      print_endline "\t [DONE] Initializing nodes";
      log_links links;
      nodes

      (** create the initial events that jumpstart the simulation loop *)
      let add_initial_events events =
        List.iter (fun e -> Queue.add_event e) events (* TODO : instead of events it was Initializer.init nodes *)

  end

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
          let i = (Random.int !Parameters.General.num_nodes)+1 in
          if not (List.exists (fun x -> x=i) !selected) then
            (
              offline.(i) <- (become_offline_timestamp, become_online_timestamp);
              selected_nodes := !selected_nodes +1
            )
        done;
        offline
      )

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
          let i = (Random.int !Parameters.General.num_nodes)+1 in
          match malicious.(i) with
          | (false,_) -> malicious.(i) <- (true, become_malicious_timestamp); selected_nodes := !selected_nodes +1
          | _ -> ()
        done;
        malicious
      )

  

  module ConsensusArg = struct
    let label = "avg-consensus-time"
    let use_intervals = true
    let format = 1
  end

  module EventsPerNodeArg = struct
    let label = "events-processed-per-node"
    let use_intervals = true
    let format = 0
  end

  (*********************************************************************************)


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

    let run () =
      Logger.init ();
      print_endline "Parsing simulation parameters...";
      Logger.log_parameters (GoodNode.parameters ());
      print_endline "[DONE] Parsing simulation parameters";
      print_endline "Creating nodes...";
      let module Aux = Auxiliary(Event)(Queue)(Logger) in
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
                  | (true,t) -> if t >= ts then BadStep.step ts e nodes else GoodStep.step ts e nodes
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
      Logger.terminate ()

  end


  (*********************************************************************************)

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
            Hashtbl.replace nodes i new_state
      end

    end

    let run () =
      Simulator.Clock.reset ();
      Timer.clear ();
      Queue.clear ();
      Logger.init ();
      print_endline "Parsing simulation parameters...";
      Logger.log_parameters (GoodNode.parameters ());
      print_endline "[DONE] Parsing simulation parameters";
      print_endline "Creating nodes...";
      let module Aux = Auxiliary(Event)(Queue)(Logger) in
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
                  | (true,t) -> if t >= ts then BadStep.step ts e nodes max_height else GoodStep.step ts e nodes max_height
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
      FinalStatistics.clear ();
      EventsPerNode.clear ()

  end

end