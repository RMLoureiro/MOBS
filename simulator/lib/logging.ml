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

end

module Make(Message:Events.Message) (Event:Events.Event with type msg=Message.t) : (Logger with type ev = Event.t) = struct
  type ev = Event.t

  (* the name of the JSON log file produced by the simulation *)
  let log_file = ref "output.json"

  (* the JSON event representing the end of the simulation *)
  let sim_end_json () = String.concat "" ["{\"kind\":\"simulation-end\",\"content\":{\"timestamp\":";Clock.to_string (Clock.get_timestamp ());"}}]"]

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
    match data with
    | "" -> ()
    | _ ->
      begin
        let out_chan = open_out_gen [Open_append; Open_creat] 0o666 !log_file in
        Printf.fprintf out_chan "%s," data;
        close_out out_chan
      end    

  let log_parameters params_json =
    log_json (String.concat "" ["{\"kind\":\"parameters\",\"content\":";params_json;"}"])

  let log_statistics stats_json =
    log_json (String.concat "" ["{\"kind\":\"statistics\",\"content\":";stats_json;"}"])

  let print_in_committee node_id round =
    let data = String.concat "" ["{\"kind\":\"node-committee\",\"content\":{\"timestamp\":";Clock.to_string (Clock.get_timestamp ());",\"node-id\":";string_of_int node_id; ",\"round\":";string_of_int round;"}}"] in
    log_json data

  let print_is_proposer node_id round =
    let data = String.concat "" ["{\"kind\":\"node-proposer\",\"content\":{\"timestamp\":";Clock.to_string (Clock.get_timestamp ());",\"node-id\":";string_of_int node_id; ",\"round\":";string_of_int round;"}}"] in
    log_json data

  (*** operations that return the JSON for a specific event ***)
  let message_json node_id_from node_id_to timestamp msg =
    String.concat "" ["{\"kind\":\"flow-message\",\"content\":{\"transmission-timestamp\":";Clock.to_string (Clock.get_timestamp ());",\"reception-timestamp\":";Clock.to_string timestamp;",\"begin-node-id\":";string_of_int node_id_from;",\"end-node-id\":";string_of_int node_id_to;",\"block-id\":";string_of_int (Message.identifier msg);",\"msg-data\":";Message.to_json msg;"}}"]

  let addnode_json node_id region_id =
    String.concat "" ["{\"kind\":\"add-node\",\"content\":{\"timestamp\":";Clock.to_string (Clock.get_timestamp ());",\"node-id\":";string_of_int node_id; ",\"region-id\":";string_of_int region_id;"}}"]

  let addlink_json begin_node_id end_node_id =
    String.concat "" ["{\"kind\":\"add-link\",\"content\":{\"timestamp\":";Clock.to_string (Clock.get_timestamp ());",\"begin-node-id\":"; string_of_int begin_node_id;",\"end-node-id\":";string_of_int end_node_id;"}}"]

  let removelink_json begin_node_id end_node_id =
    String.concat "" ["{\"kind\":\"remove-link\",\"content\":{\"timestamp\":";Clock.to_string (Clock.get_timestamp ());",\"begin-node-id\":"; string_of_int begin_node_id;",\"end-node-id\":";string_of_int end_node_id;"}}"]

  let print_create_block nodeID blockID =
    let data = String.concat "" ["{\"kind\":\"create-block\",\"content\":{\"timestamp\":";Clock.to_string (Clock.get_timestamp ());",\"node-id\":";string_of_int nodeID;",\"block-id\":";string_of_int blockID;"}}"] in
    log_json data

  let print_new_chain_head nodeID ownerID blockID =
    let data = String.concat "" ["{\"kind\":\"add-block\",\"content\":{\"timestamp\":";Clock.to_string (Clock.get_timestamp ());",\"node-id\":";string_of_int nodeID;",\"owner-id\":";string_of_int ownerID;",\"block-id\":";string_of_int blockID;"}}"] in
    log_json data

  let log_event (event: ev) =
    let event_json = match event with
    | Message(node_id_from, node_id_to, timestamp, msg) -> message_json node_id_from node_id_to timestamp msg 
    | AddNode(node_id, region_id) -> addnode_json node_id region_id
    | AddLink(begin_node_id, end_node_id) -> addlink_json begin_node_id end_node_id
    | RemoveLink(begin_node_id, end_node_id) -> removelink_json begin_node_id end_node_id
    | MintBlock(_,_) -> ""
    | Timeout(_, _, _) -> ""
    in log_json event_json

end
