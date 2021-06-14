module type PoW = sig

  (** initialize the mining power of the nodes according to the parameters *)
  val init_mining_power : unit -> unit

  (** given a node's ID and a block, starts the minting process by nodeID to extend that block *)
  val start_minting : int -> Simulator.Block.t -> unit

  (** given a node's ID, stops its minting process *)
  val stop_minting : int -> unit

  (** given a node's ID obtain its mining power *)
  val get_mining_power : int -> int

  (** get the network's total mining power *)
  val total_mining_power : unit -> int
end

module Make(Events : Simulator.Events.Event)(Queue : Simulator.Events.EventQueue with type ev = Events.t)(Block : Simulator.Block.BlockSig) : PoW = struct

  let mining_power : int list ref = ref []

  let init_mining_power () =
    let num_nodes = !Parameters.General.num_nodes in
    for _ = 0 to num_nodes do 
      let r = Random.float 1. in
      let b = Random.bool () in
      let power = 
        match b with
        | true  -> int_of_float (float_of_int(!Parameters.General.avg_mining_power) +. (r *. float_of_int(!Parameters.General.stdev_mining_power)))
        | false -> int_of_float (float_of_int(!Parameters.General.avg_mining_power) -. (r *. float_of_int(!Parameters.General.stdev_mining_power)))
      in
      mining_power := !mining_power @ [max power 1]
    done

  let get_mining_power nodeID =
    List.nth !mining_power (nodeID - 1) (* IDs start at 1 *)

  let total_mining_power () =
    let rec sum l =
      match l with
      | [] -> 0
      | x::xs -> x + sum xs
    in
    sum !mining_power

  let start_minting nodeID parent =
    let difficulty = Block.difficulty parent in
    let p = 1. /. (float_of_int difficulty) in
    let u = Random.float 1. in
    let mining_power = float_of_int(get_mining_power nodeID) in
    let mint_duration = int_of_float((log(u) /. log(1. -. p)) /. mining_power) in
    let mint_conclusion = (Simulator.Clock.get_timestamp ()) + mint_duration in
    let mint_event = Events.create_mint nodeID mint_conclusion in
    if p > (2. ** -53.) then Queue.add_event mint_event

  let stop_minting nodeID =
    Queue.cancel_minting nodeID

end