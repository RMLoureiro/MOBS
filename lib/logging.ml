(* the name of the JSON log file produced by the simulation *)
let log_file = "output.json"

(* the JSON event representing the end of the simulation *)
let sim_end_json = String.concat "" ["{\"kind\":\"simulation-end\",\"content\":{\"timestamp\":";string_of_int (Clock.get_timestamp ());"}}]"]

(* create and initialize the JSON log file *)
let init () = 
  let out_chan = open_out log_file in
  Printf.fprintf out_chan "%s" "[";
  close_out out_chan

(* write the termination event to the JSON log file *)
let terminate () =
  let out_chan = open_out_gen [Open_append; Open_creat] 0o666 log_file in
  Printf.fprintf out_chan "%s" sim_end_json;
  close_out out_chan

(* prints <data> to the log_file *)
(* <data> should be in JSON format *)
let log_json data =
  let out_chan = open_out_gen [Open_append; Open_creat] 0o666 log_file in
  Printf.fprintf out_chan "%s," data;
  close_out out_chan




(*** operations that return the JSON for a specific event ***)

let message_json node_id_from node_id_to timestamp msg =
  String.concat "" ["{\"kind\":\"flow-message\",\"content\":{\"transmission-timestamp\":";string_of_int (Clock.get_timestamp ());",\"reception-timestamp\":";string_of_int timestamp;",\"begin-node-id\":";string_of_int node_id_from;",\"end-node-id\":";string_of_int node_id_to;",\"msg-data\":";Messages.msg_to_json msg;"}}"]

let addnode_json node_id region_id =
  String.concat "" ["{\"kind\":\"add-node\",\"content\":{\"timestamp\":";string_of_int (Clock.get_timestamp ());",\"node-id\":";string_of_int node_id; ",\"region-id\":";string_of_int region_id;"}}"]

let addlink_json begin_node_id end_node_id =
  String.concat "" ["{\"kind\":\"add-link\",\"content\":{\"timestamp\":";string_of_int (Clock.get_timestamp ());",\"begin-node-id\":"; string_of_int begin_node_id;",\"end-node-id\":";string_of_int end_node_id;"}}"]

let removelink_json begin_node_id end_node_id =
  String.concat "" ["{\"kind\":\"remove-link\",\"content\":{\"timestamp\":";string_of_int (Clock.get_timestamp ());",\"begin-node-id\":"; string_of_int begin_node_id;",\"end-node-id\":";string_of_int end_node_id;"}}"]

let timeout_json node_id = 
  String.concat "" ["{\"kind\":\"timeout\",\"content\":{\"timestamp\":";string_of_int (Clock.get_timestamp ());",\"«node-id\":"; string_of_int node_id; "}}"]
(*** ---------------------------------------------------- ***)

(* given an event, produces the JSON object that represents that event *)
let log_event (event: Events.event) =
  let event_json = match event with
  | Message(node_id_from, node_id_to, timestamp, msg) -> message_json node_id_from node_id_to timestamp msg
  | AddNode(node_id, region_id) -> addnode_json node_id region_id
  | AddLink(begin_node_id, end_node_id) -> addlink_json begin_node_id end_node_id
  | RemoveLink(begin_node_id, end_node_id) -> removelink_json begin_node_id end_node_id
  | Timeout(node_id, _, _) -> timeout_json node_id
  in log_json event_json


