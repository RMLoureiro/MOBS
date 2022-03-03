(*
  Note: each label can only have one associated timeout at each point in time
    -> shouldn't cause a problem, since the user can schedule the next timeout, when 
        a timeout occurs
*)

(** Abstraction to schedule and cancel timeout/alarm events for nodes in the simulation. *)
module type Timer = sig

  (** Creates and schedules a timeout event for a node.
    @param node_id the id of the node that is scheduling the timeout
    @param timestamp the timestamp when the timeout should occur
    @param label the label associated with the timout
  *)
  val set : int -> Simulator.Clock.t -> string -> unit

  (** Cancels an existing timeout event.
    @param node_id the id of the node whose timeout is getting canceled
    @param label the label of the respective timeout
  *)
  val cancel : int -> Simulator.Events.timeout_label -> unit

  (** Checks if a given timeout is expired.
    @param node_id the id of the node
    @param timestamp the current timestamp
    @param label the label of the timeout whose expiration is being checked
  *)
  val expired : int -> Simulator.Clock.t -> Simulator.Events.timeout_label -> bool

  (** Clear all data associated with existing timers. *)
  val clear : unit -> unit

end

(** Create an implementation for Timer, given the Event and EventQueue modules. *)
module Make(Event : Simulator.Events.Event)(Queue : Simulator.Events.EventQueue with type ev = Event.t) : Timer = struct

  (* for each node, store an hashtable with the next timer for each label *)
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