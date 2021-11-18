module type Logger = sig
  type ev

  val init : unit -> unit

  val terminate : unit -> unit

  (** [@Params event] logs an event to the JSON log *)
  val log_event : ev -> unit

  (** [@Params nodeID round] logs that a node belongs to a committee of a round *)
  val print_in_committee : int -> int -> unit

  (** [@Params nodeID round] logs that a node is a proposer of a round *)
  val print_is_proposer : int -> int -> unit

  (** [@Params nodeID blockID] print that a node has minted a block *)
  val print_create_block : int -> int -> unit

  (** [@Params nodeID ownerID blockID] print that the head of a node's chain as been updated *)
  val print_new_chain_head : int -> int -> int -> unit

  (** receives a JSON string with the parameters and logs them *)
  val log_parameters : string -> unit

  (** receives a JSON string with the statistics and logs them *)
  val log_statistics : string -> unit

  (** receives a JSON string with the per node statistics and logs them (the format differs from log_statistics) *)
  val log_per_node_statistics : string -> unit

  (** log the addition of a node *)
  val log_add_node : int -> int -> unit

  (** log the addition of a link *)
  val log_add_link : int -> int -> unit

  (** log the removal of a link *)
  val log_remove_link : int -> int -> unit

end

module Make(Message:Events.Message) (Event:Events.Event with type msg=Message.t) : (Logger with type ev = Event.t) = struct
  type ev = Event.t

  (* the name of the JSON log file produced by the simulation *)
  let log_file = ref "output.json"

  (* the JSON event representing the end of the simulation *)
  let sim_end_json () = 
    Printf.sprintf "{\"kind\":\"simulation-end\",\"content\":{\"timestamp\":%d}}]" (Clock.get_timestamp ())

  let write_to_file str = 
    let out_chan = open_out_gen [Open_append; Open_creat] 0o666 !log_file in
    Printf.fprintf out_chan "%s,\n" str;
    close_out out_chan

  let init () = 
    if Array.length Sys.argv > 2 then log_file := Sys.argv.(2);
    let out_chan = open_out !log_file in
    Printf.fprintf out_chan "%s" "[";
    close_out out_chan

  let terminate () =
    let out_chan = open_out_gen [Open_append; Open_creat] 0o666 !log_file in
    Printf.fprintf out_chan "%s" (sim_end_json ());
    close_out out_chan

  (* prints <data> to the log_file *)
  (* <data> should be in JSON format *)
  let log_json data =
    if !Parameters.General.verbose then
      match data with
      | "" -> ()
      | _ -> write_to_file data

  let log_parameters params_json =
    let str = (Printf.sprintf "{\"kind\":\"parameters\",\"content\":%s}" params_json) in
    write_to_file str

  let log_statistics stats_json =
    let str = (Printf.sprintf "{\"kind\":\"statistics\",\"content\":%s}" stats_json) in
    write_to_file str

  let log_per_node_statistics stats_json =
    let str = (Printf.sprintf "{\"kind\":\"per-node-statistics\",\"content\":%s}" stats_json) in
    write_to_file str 

  let print_in_committee node_id round =
    let data = Printf.sprintf "{\"kind\":\"node-committee\",\"content\":{\"timestamp\":%d,\"node-id\":%d,\"round\":%d}}" (Clock.get_timestamp ()) node_id round in
    log_json data

  let print_is_proposer node_id round =
    let data = Printf.sprintf "{\"kind\":\"node-proposer\",\"content\":{\"timestamp\":%d,\"node-id\":%d,\"round\":%d}}" (Clock.get_timestamp ()) node_id round in
    log_json data

  (*** operations that return the JSON for a specific event ***)
  let message_json node_id_from node_id_to timestamp msg =
    Printf.sprintf "{\"kind\":\"flow-message\",\"content\":{\"transmission-timestamp\":%d,\"reception-timestamp\":%d,\"begin-node-id\":%d,\"end-node-id\":%d,\"block-id\":%d,\"msg-data\":%s}}" (Clock.get_timestamp ()) timestamp node_id_from node_id_to (Message.identifier msg) (Message.to_json msg)

  let addnode_json node_id region_id =
    Printf.sprintf "{\"kind\":\"add-node\",\"content\":{\"timestamp\":%d,\"node-id\":%d,\"region-id\":%d}}" (Clock.get_timestamp ()) node_id region_id

  let addlink_json begin_node_id end_node_id =
    Printf.sprintf "{\"kind\":\"add-link\",\"content\":{\"timestamp\":%d,\"begin-node-id\":%d,\"end-node-id\":%d}}" (Clock.get_timestamp ()) begin_node_id end_node_id

  let removelink_json begin_node_id end_node_id =
    Printf.sprintf "{\"kind\":\"remove-link\",\"content\":{\"timestamp\":%d,\"begin-node-id\":%d,\"end-node-id\":%d}}" (Clock.get_timestamp ()) begin_node_id end_node_id

  let print_create_block nodeID blockID =
    let data = Printf.sprintf "{\"kind\":\"create-block\",\"content\":{\"timestamp\":%d,\"node-id\":%d,\"block-id\":%d}}" (Clock.get_timestamp ()) nodeID blockID in
    log_json data

  let print_new_chain_head nodeID ownerID blockID =
    let data = Printf.sprintf "{\"kind\":\"add-block\",\"content\":{\"timestamp\":%d,\"node-id\":%d,\"block-id\":%d,\"owner-id\":%d}}" (Clock.get_timestamp ()) nodeID blockID ownerID in
    log_json data

  let log_add_node nodeID regionID =
    log_json (addnode_json nodeID regionID)

  let log_add_link beginNodeID endNodeID =
    log_json (addlink_json beginNodeID endNodeID)

  let log_remove_link beginNodeID endNodeID =
    log_json (removelink_json beginNodeID endNodeID)

  let log_event (event: ev) =
    let event_json = match event with
    | Message(node_id_from, node_id_to, timestamp, msg) -> message_json node_id_from node_id_to timestamp msg 
    | MintBlock(_,_) -> ""
    | Timeout(_, _, _) -> ""
    in log_json event_json

end
