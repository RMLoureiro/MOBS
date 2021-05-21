(** create and initialize the JSON log file *)
val init : unit -> unit

(** write the termination event to the JSON log file *)
val terminate : unit -> unit

(** given an event, produces the JSON object that represents that event *)
val log_event : Events.event -> unit