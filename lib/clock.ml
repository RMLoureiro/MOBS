(* type that represents the timestamp *)
type t_timestamp = int

(* the current timestamp of the simulation clock *)
let timestamp = ref 0

(* update the timestamp of the simulation clock *)
let set_timestamp ts =
  timestamp := ts

(* get the current timestamp of the simulation clock *)
let get_timestamp () =
  !timestamp