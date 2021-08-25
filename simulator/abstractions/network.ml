type links         = int list
type network_links = links array
type region        = int
type regions       = int array


(***** Auxiliary Functions *****)

(* shuffle an array by tagging each element with random bits, and then using the sort function *)
(* the usage of the Random.bits allows us have a seeded shuffle *)
let shuffle_array l = 
  let tagged_arr = Array.map (fun c -> Random.bits (), c) l in
  let _ = Array.sort compare tagged_arr in
  Array.map (fun (_, b) -> b) tagged_arr


(***** Create Region Distribution List *****)

let rec node_regions_step l i acc =
  let num_nodes = !Parameters.General.num_nodes in
  let dist_regions = !Parameters.General.region_distribution in
  let n = Array.length dist_regions in
    if i < n then
      let rdist = (dist_regions.(i)) in
        if (List.length l) < int_of_float (Float.round ((float_of_int num_nodes) *. (rdist +. acc))) then
          node_regions_step (i::l) i acc
        else
          node_regions_step (i::l) (i+1) (rdist +. acc)
    else
    l

(** uses the network parameters to return a list of integers, representing the region each node is assigned to *)
let node_regions () : regions =
  shuffle_array (Array.of_list (node_regions_step [] 0 0.0))


(***** Create Links between Nodes *****)

(* given a list of links, adds a link from i to j *)
let add_link i j links =
  Array.mapi (fun x y -> if x = i then [j]@y else y) links

(** condition for node with <nid> to be added as an outbound neighbor to node with <id> *)
let can_add_neighbor id nid outbound_links inbound_links :bool =
  let num_links        = !Parameters.General.num_links in
  let num_nodes        = !Parameters.General.num_nodes in
  let last_node        = (id+1) = num_nodes in (* ensure last node has the requried outbound links *)
  let id_outbound      = outbound_links.(id) in
  let nid_inbound      = inbound_links.(nid) in
  let already_outbound = List.exists (fun e -> e = nid) id_outbound in
  let already_inbound  = List.exists (fun e -> e = id) nid_inbound in
  let outbound_limit   = (List.length id_outbound) >= num_links in
  let inbound_limit    = (List.length nid_inbound) >= num_links in
  let same_node        = id = nid in
  let valid_nodes      = (id >= 0) && (id < num_nodes) && (nid >= 0) && (nid < num_nodes) in
  (not same_node) && (not already_outbound) && (not already_inbound) && (not outbound_limit) && ((not inbound_limit) || last_node) && valid_nodes

(** select neighbors for node with id = <id>, taking the link limitations in consideration *)
let select_neighbors id nodes (inbound_links:network_links) (outbound_links:network_links) =
  let num_links = !Parameters.General.num_links in
  let candidates = shuffle_array nodes in 
  for i = 0 to (Array.length candidates)-1 do
    begin
    let candidate = (candidates.(i)) in
    if List.length outbound_links.(id) < num_links then
      if can_add_neighbor id candidate outbound_links inbound_links then
        begin
        outbound_links.(id) <- [candidate]@outbound_links.(id);
        inbound_links.(candidate) <- [id]@inbound_links.(candidate)
        end
    end
  done

(** uses the network parameters to return a list of int lists, representing the links each node has *)
let node_links () : network_links =
  let num_nodes = !Parameters.General.num_nodes in
  let nodes = Array.init num_nodes (fun c -> c) in
  let inbound_links:(network_links) = Array.make num_nodes [] in
  let outbound_links:(network_links) = Array.make num_nodes [] in
  for i = 0 to num_nodes-1 do
    select_neighbors i nodes inbound_links outbound_links 
  done;
  Array.map (fun x -> List.map (fun y -> y+1) x) outbound_links (* node id's start at 1 *)

(*******************************)

module type Network = sig
  type msg

  (** send a message to another node
      used when we need more fine-tuned
      control of message exchanges *)
  val send : int -> int -> msg -> unit

  (** gossip a message to every node in the network
      used for more general purpose use cases, 
      as it assumes the nodes' bandwidth is always free *)
  val gossip : int -> msg -> unit

  (** get the list of neighbour node_ids *)
  val get_neighbours : int -> links
end

module Make(Events: Simulator.Events.Event) 
          (Queue: Simulator.Events.EventQueue with type ev = Events.t)
          (Message : Simulator.Events.Message with type t = Events.msg) : (Network with type msg=Events.msg) =
struct

  type msg = Events.msg


  let regions = (node_regions ())
  let links   = (node_links ())

  (***** Latency Calculation Operations *****)

  (* from SimBlock -> latency according to 20% variance pallet distribution *)
  (* https://dsg-titech.github.io/simblock/ *)
  let get_latency sender receiver = 
    let region_sender   = regions.(sender-1) in     (* id's start at 1 *)
    let region_receiver = regions.(receiver-1) in   (* id's start at 1 *)
    let mean_latency    = (!Parameters.General.latency_table.(region_sender)).(region_receiver) in
    let shape           = 0.2 *. (float_of_int mean_latency)  in
    let scale           = float_of_int (mean_latency - 5) in
    int_of_float (Float.round (scale /. ((Random.float 1.0) ** (1.0 /. shape))))

  let get_bandwidth sender receiver =
    let region_sender      = regions.(sender-1) in     (* id's start at 1 *)
    let region_receiver    = regions.(receiver-1) in   (* id's start at 1 *)
    let upload_bandwidth   = !Parameters.General.upload_bandwidth.(region_sender) in
    let download_bandwidth = !Parameters.General.download_bandwidth.(region_receiver) in
    min upload_bandwidth download_bandwidth

  let send sender receiver msg =
    let latency = get_latency sender receiver in
    let bandwidth = get_bandwidth sender receiver in
    let delay = (Message.get_size msg) / (bandwidth / 1000) + (Message.processing_time msg)  in
    let arrival_time = (Simulator.Clock.get_timestamp ()) + latency + delay in
    let msg_event = Events.Message(sender,receiver,arrival_time,msg) in
    Queue.add_event msg_event;
    if Message.get_size msg >= 800000 then 
      Queue.add_event (Events.Timeout(sender, arrival_time, "message_sent")) (* "notify" the sender when it finishes sending a large message *)

  let get_neighbours node =
    links.(node)

  let gossip sender msg =
    ()

end
