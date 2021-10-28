type credential = Credential of node_id * block_id * num_selections * priority * params
  and node_id = Node of int
  and block_id = Block of int
  and num_selections = Selections of int
  and priority = Priority of int
  and params = Params of int list

module type PoS = sig

  val sortition : 'a Simulator.Block.t -> int -> int list -> int list

  val in_committee : int -> 'a Simulator.Block.t -> int -> int list -> credential option

  val is_proposer : int -> 'a Simulator.Block.t -> int -> int list -> credential option

  val valid_proposer : int -> 'a Simulator.Block.t -> int -> int list -> credential -> bool

  val valid_committee_member : int -> 'a Simulator.Block.t -> int -> int list -> credential -> bool

  val proposer_priority : int -> 'a Simulator.Block.t -> int -> int list -> credential -> int


end


module Make(Logger : Simulator.Logging.Logger)(Block : Simulator.Block.BlockSig) : PoS = struct

  (**
    <block> : the block whose balances will be used by sortition
    <num_selections> : the number of nodes that will be selected by sortition
    <params> : a list of integers that identify the selection (seed for the RNG-based abstraction)
    
    returns a list with the ID's of the selected nodes
  *)
  let sortition block num_selections params =
    let r_state = Random.get_state () in
    let selected_nodes : int list ref = ref [] in
    let num_selected = ref 0 in
    let balances = Array.of_list (Block.balances block) in
    let total_coins = Block.total_coins block in
    Random.full_init (Array.of_list params);
    while !num_selected < num_selections do
      let weight = Random.float total_coins in
      let weight_sum = ref 0. in
      let selected = ref (-1) in
      for i=0 to (Array.length balances)-1 do
        if !selected = -1 then
          begin
            let (id, bal) = Array.get balances i in
            weight_sum := !weight_sum +. bal;
            if weight <= !weight_sum then selected := id
          end
      done;
      if not (List.exists (fun x -> x = !selected) !selected_nodes) then
        begin
          selected_nodes := !selected_nodes @ [!selected];
          num_selected := !num_selected + 1
        end
    done;
    Random.set_state r_state;
    !selected_nodes

  let get_credential node_id head num_selections params =
    let selections = sortition head num_selections params in
    if List.exists (fun id -> id = node_id) selections then
    (
      let res = ref (-1) in
      List.iteri (fun i id -> if id=node_id then res := i) selections;
      let prio = !res in
      Some(Credential(Node(node_id), Block(Simulator.Block.id head), Selections(num_selections), Priority(prio), Params(params)))
    )
    else
      None

  let valid_proposer node_id head num_proposers params credential = 
    match credential with
    | Credential(Node(id),Block(block_id),Selections(num_selections),Priority(_),Params(p)) -> 
      node_id = id && (Simulator.Block.id head) = block_id && num_proposers = num_selections && params = p

  let valid_committee_member node_id head committee_size params credential =
    match credential with
    | Credential(Node(id),Block(block_id),Selections(num_selections),Priority(_),Params(p)) -> 
      node_id = id && (Simulator.Block.id head) = block_id && committee_size = num_selections && params = p

  (** 
    * check if the node with <node_id> was selected by sortition of size <committee_size>
    * <head> is the head of the chain (required to access the stakes of the nodes)
    * <params> is a list of integer parameters that identify the committee
  *)
  let in_committee node_id head committee_size params =
    match get_credential node_id head committee_size params with
    | Some(cred) -> Logger.print_in_committee node_id ((Block.height head)+1); Some(cred)
    | _ -> None

  (** 
    * check if the node with <node_id> was selected by sortition of size <num_proposers>
    * <head> is the head of the chain (required to access the stakes of the nodes)
    * <params> is a list of integer parameters that identify the committee
  *)
  let is_proposer node_id head num_proposers params =
    match get_credential node_id head num_proposers params with
    | Some(cred) -> Logger.print_is_proposer node_id ((Block.height head)+1); Some(cred)
    | _ -> None

  (** obtain the priority associated with a user that was selected to propose a block *)
  let proposer_priority node_id head num_proposers params credential =
    if valid_proposer node_id head num_proposers params credential then
    (
      match credential with
      | Credential(_,_,_,Priority(prio),_) -> prio
    )
    else
      -1

end
