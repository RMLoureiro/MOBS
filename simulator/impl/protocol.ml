module type Node = sig
  include Abstract.Node

  (** obtain the height of the chain stored in the node *)
  val chain_height : t -> int

end


module type Statistics = sig

  include Abstract.Statistics

end


module Make(Event : Simulator.Events.Event)
           (Queue : Simulator.Events.EventQueue with type ev = Event.t)
           (Block : Simulator.Block.BlockSig)
           (Node : Node with type ev = Event.t and type value = Block.block) 
           (Initializer : Abstract.Initializer with type node = Node.t and type ev = Event.t)
           (Logger : Simulator.Logging.Logger with type ev = Event.t)
           (Statistics : Statistics with type ev=Event.t and type value=Block.block) : Abstract.Protocol
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
    print_endline "\t Assigning node regions...";
    let regions   = Abstractions.Network.node_regions () in
    print_endline "\t [DONE] Assigning node regions";
    print_endline "\t Creating links between nodes...";
    let links     = Abstractions.Network.node_links () in
    print_endline "\t [DONE] Creating links between nodes";
    print_endline "\t Initializing nodes...";
    let nodes     = Hashtbl.create num_nodes in
    for i = 1 to num_nodes do
      let node_links = List.nth links (i-1) in
      let node_region = List.nth regions (i-1) in
      let node = Node.init i node_links node_region in
      Hashtbl.add nodes i node;
      Logger.log_event (Event.AddNode(i, node_region))
    done;
    print_endline "\t [DONE] Initializing nodes";
    log_links links;
    nodes

    (** create the initial events that jumpstart the simulation loop *)
    let initial_events nodes =
      List.iter (fun e -> Queue.add_event e) (Initializer.init nodes)

    let run () =
      Logger.init ();
      print_endline "Parsing simulation parameters...";
      Logger.log_parameters (Node.parameters ());
      print_endline "[DONE] Parsing simulation parameters";
      print_endline "Creating nodes...";
      let nodes            = create_nodes () in
      print_endline "[DONE] Creating nodes";
      print_endline "Creating initial events...";
      let _                = initial_events nodes in
      print_endline "[DONE] Creating initial events";
      print_endline "Running simulation...";
      let max_height       = ref 0 in
      let end_block_height = !Parameters.General.end_block_height in
      let max_timestamp    = !Parameters.General.max_timestamp in
      let timestamp_limit  = !Parameters.General.timestamp_limit in
      while Queue.has_event () && !max_height < end_block_height && ((not timestamp_limit) || (Simulator.Clock.get_timestamp () <= max_timestamp)) do
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
            let old_chain_head = Node.state node_state in
            let new_state = Node.handle node_state e in
              begin 
                if Node.chain_height new_state > !max_height then
                  begin
                  max_height := Node.chain_height new_state;
                  print_endline (String.concat "" ["\t Longest Chain Height Observed: ";(string_of_int !max_height)])
                  end
              end;
              begin
                let new_chain_head = Node.state new_state in
                if not (Block.equals old_chain_head new_chain_head) then
                  begin
                    Logger.print_new_chain_head i (Block.minter new_chain_head) (Block.id new_chain_head);
                    Statistics.consensus_reached i new_chain_head
                  end
              end;
              Hashtbl.replace nodes i new_state
      done;
      print_endline "\t Reached stopping condition";
      Logger.log_statistics (Statistics.get ());
      Logger.terminate ()

end