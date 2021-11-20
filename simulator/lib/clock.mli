(** the type representing a timestamp *)
type t = int

(** sets the timestamp of the simulation clock to <t> *)
val set_timestamp : t -> unit

(** get the current timestamp of the simulation clock *)
val get_timestamp : unit -> t

(** get the timestamp for clock zero *)
val zero : t

(** converts a timestamp to string *)
val to_string : t -> string

(** when a new iteration begins, reset the clock *)
val reset : unit -> unit