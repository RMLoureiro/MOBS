(* this file will contain the simulation parameters *)
(* reads from JSON file so that changing parameters doesn't require recompiling the code *)

let parameters_file = "parameters.json"

(* parameters and default values *)
let num_nodes = ref 10
let end_block_height = ref 5
let seed = ref 123


(* TODO : add other parameters as they are needed *)


let () =
  let json = Yojson.Basic.from_file parameters_file in
  let open Yojson.Basic.Util in
  num_nodes := json |> member "num-nodes" |> to_int;
  end_block_height := json |> member "end-block-height" |> to_int;
  seed := json |> member "seed" |> to_int


