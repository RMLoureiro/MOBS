(* this file will contain the simulation parameters *)
(* reads from JSON file so that changing parameters doesn't require recompiling the code *)

let parameters_file = "parameters.json"

(* general parameters and default values *)
let num_nodes = ref 10
let end_block_height = ref 5
let seed = ref 123

(* TODO : add other parameters as they are needed *)

(* network parameters and default values *)
let num_regions = ref 6
let num_links = ref 5
let latency_table = ref [
  [32; 124; 184; 198; 151; 189];
  [124; 11; 227; 237; 252; 294];
  [184; 227; 88; 325; 301; 322];
  [198; 237; 325; 85; 58; 198];
  [151; 252; 301; 58; 12; 126];
  [189; 294; 322; 198; 126; 16]]
let region_distribution = ref [0.3316; 0.4998; 0.0090; 0.1177; 0.0224; 0.0195]
let degree_distribution = ref [0.025; 0.050; 0.075; 0.10; 0.20; 0.30; 0.40; 0.50; 0.60; 0.70; 0.80; 0.85; 0.90; 0.95; 0.97; 0.97; 0.98; 0.99; 0.995; 1.0]


let get_general_param json param =
  let open Yojson.Basic.Util in
  json |> member "general" |> member param

let get_network_param json param =
  let open Yojson.Basic.Util in
  json |> member "network" |> member param

let () =
  let json = Yojson.Basic.from_file parameters_file in
  let open Yojson.Basic.Util in
  num_nodes := get_general_param json "num-nodes" |> to_int;
  end_block_height := get_general_param json "end-block-height" |> to_int;
  seed := get_general_param json "seed" |> to_int; Random.init !seed;
  num_regions := get_network_param json "num-regions" |> to_int;
  num_links := get_network_param json "num-links" |> to_int


