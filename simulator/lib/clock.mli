(** The type representing a timestamp. *)
type t = int

(** Sets the timestamp of the simulation clock to {b ts}.
    @param ts the new timestamp for the simulation clock
*)
val set_timestamp : t -> unit

(** Get the current timestamp of the simulation clock. *)
val get_timestamp : unit -> t

(** Get the timestamp for clock zero. *)
val zero : t

(** Converts a timestamp to string.
    @param ts the timestamp in non-string form
*)
val to_string : t -> string

(** Reset the simulation clock back to zero. *)
val reset : unit -> unit