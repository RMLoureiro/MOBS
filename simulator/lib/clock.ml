type t = int

(** the current timestamp of the simulation clock *)
let (timestamp:t ref) = ref 0

let set_timestamp ts =
  timestamp := ts

let get_timestamp () =
  !timestamp

let zero = 0

let to_string ts = string_of_int ts