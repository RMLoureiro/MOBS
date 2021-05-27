(** the id of a node *)
type id = int

(** the type representing a node and its state *)
type t

(** the initial state of a node *)
val init : id -> Network.links -> Network.region -> t

val print : t -> unit