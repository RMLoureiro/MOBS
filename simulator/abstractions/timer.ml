module type Timer = sig

  (** creates and schedules a timeout event for a node, given the nodeID, delay, and timeout label *)
  val set : int -> Simulator.Clock.t -> string -> unit

  val cancel : int -> Simulator.Events.timeout_label -> unit

end

module Make(Event : Simulator.Events.Event)(Queue : Simulator.Events.EventQueue with type ev = Event.t) : Timer = struct


  let set nodeID delay label =
    Queue.add_event (Event.create_timeout nodeID delay label)

  let cancel nodeID label =
    Queue.cancel_timer nodeID label

end