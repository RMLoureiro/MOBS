(*
  Note: each label can only have one associated timeout at each point in time
    -> shouldn't cause a problem, since the user can schedule the next timeout, when 
        a timeout occurs
*)

module type Timer = sig

  (** creates and schedules a timeout event for a node, given the nodeID, delay, and timeout label *)
  val set : int -> Simulator.Clock.t -> string -> unit

  (** cancels an existing timeout event, given a nodeiD and a label *)
  val cancel : int -> Simulator.Events.timeout_label -> unit

  (** checks if the timeout for nodeID, at timestamp, with label as expired *)
  val expired : int -> Simulator.Clock.t -> Simulator.Events.timeout_label -> bool

  (** clear existing timers *)
  val clear : unit -> unit

end

module Make(Event : Simulator.Events.Event)(Queue : Simulator.Events.EventQueue with type ev = Event.t) : Timer = struct

  (** for each node, store an hashtable with the next timer for each label *)
  let next_timers = Array.init (!Parameters.General.num_nodes) (fun _ -> Hashtbl.create 5)

  let set nodeID delay label =
    let timeout_event = Event.create_timeout nodeID delay label in
    Queue.add_event (timeout_event);
    Hashtbl.replace (next_timers.(nodeID)) label (Event.timestamp timeout_event)

  let cancel nodeID label =
    Hashtbl.replace (next_timers.(nodeID)) label (-1)

  let expired nodeID timestamp label =
    match Hashtbl.find_opt (next_timers.(nodeID)) label with
    | Some(ts) -> not (timestamp = ts)
    | None -> 
      (* in this case, the event is not expired because it 
      must have been created in the initialization module *)
      false 

  let clear _ =
    Array.iter (fun x -> Hashtbl.clear x) next_timers

end