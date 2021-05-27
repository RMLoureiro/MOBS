
(** given the 2D array of links defined in network.mli, produce the correct JSON logs *)
let log_links links = 
  List.iteri (
    fun i l -> 
      (List.iter (
        fun j -> Simulator.Logging.log_event (Simulator.Events.AddLink(i+1, j))
      ) l)
    ) links

(* create the nodes to be used in the simulation, and produce the node and link creation JSON logs *)
let create_nodes debug =
  let regions = Abstractions.Network.node_regions in
  let links   = Abstractions.Network.node_links in
  let nodes   = ref [] in
  for i = 1 to !Parameters.General.num_nodes do
    let node_links = List.nth links (i-1) in
    let node_region = List.nth regions (i-1) in
    let node = Abstractions.Node.init i node_links node_region in
    nodes := !nodes @ [node];
    if debug then Abstractions.Node.print node;
    Simulator.Logging.log_event (Simulator.Events.AddNode(i, node_region))
  done;
  log_links links;
  nodes


let () = 
  Simulator.Logging.init ();
  let _ = create_nodes false in 
  Simulator.Logging.terminate ()