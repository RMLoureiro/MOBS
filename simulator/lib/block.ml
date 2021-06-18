type node_id  = int
type block_id = int
type balances = (node_id * float) list

(** type of a block's representation *)
type t =
  {
    id         : block_id;
    height     : int;
    minter     : node_id;
    parent     : t option;
    balances   : balances;
    difficulty : int;
    timestamp  : Clock.t
  }

let id block = block.id

module type BlockSig = sig
  (** construct a new block, given the minter_id and parent block *)
  val create : node_id -> t -> t

  (** get the height of the block *)
  val height : t -> int

  (** get the parent block *)
  val parent : t -> t option

  (** get the id of the node that minted the block *)
  val minter : t -> node_id

  (** get the id of the block *)
  val id : t -> block_id

  (** get the id of a block option *)
  val opt_id : t option -> block_id

  (** get the balances of the nodes *)
  val balances : t -> balances

  (** get the timestamp of the block's creation *)
  val timestamp : t -> Clock.t

  (** get the difficulty of the block *)
  val difficulty : t -> int

  (** get the total difficulty of the chain *)
  val total_difficulty : t -> int

  (** get the total amount of coins *)
  val total_coins : t -> float

  (** get the genesis block, assigning a minter and a initial dificulty *)
  val genesis_pow : node_id -> int -> t

  (** get the genesis block, assigning a minter *)
  val genesis_pos : node_id -> t

  (** check if two blocks are equal *)
  val equals : t -> t -> bool
end



module Make(Logger : Logging.Logger) : BlockSig = struct

  let latest_id = ref 0

  (* TODO : add the ability to quickly hotswap how nodes are rewarded (functor receives another module?) *)
  let reward minter balances =
    let gets_reward (id,bal) =
      if id = minter then
        (id, bal +. (bal *. !Parameters.General.reward))
      else
        (id, bal)
      in
    List.map gets_reward balances

  let create minter parent =
    latest_id := !latest_id + 1;
    Logger.print_create_block minter !latest_id;
    {
      id         = !latest_id;
      height     = parent.height + 1;
      minter     = minter;
      parent     = Some parent;
      difficulty = parent.difficulty;
      balances   = reward minter parent.balances;
      timestamp  = Clock.get_timestamp ()
    }

  let height block = block.height

  let parent block = block.parent

  let minter block = block.minter

  let id block = block.id

  let opt_id block_opt =
    match block_opt with
    | Some(blk) -> id blk
    | None -> -1

  let balances block = block.balances

  let gen_balances =
    let gen_bal id =
      let r = Random.float 1.0 in
      let coins = max (r *. (float_of_int !Parameters.General.stdev_coins) +. (float_of_int !Parameters.General.avg_coins)) 0.0 in
      (id, coins)
    in 
    List.init !Parameters.General.num_nodes gen_bal

  let total_coins block = 
    let rec total bal sum =
      match bal with
      | [] -> sum
      | (_,coins)::xs -> total xs (sum+.coins)
    in
    total block.balances 0.0

  let timestamp block = block.timestamp

  let difficulty block = block.difficulty

  let total_difficulty block =
    let rec sum_difficulty b =
      match b.parent with
      | None -> b.difficulty
      | Some(parent) -> b.difficulty + (sum_difficulty parent)
    in
    sum_difficulty block

  let genesis_pow minter_id difficulty =
    {
      id         = 0;
      height     = 0;
      minter     = minter_id;
      parent     = None;
      difficulty = difficulty * !Parameters.General.interval;
      balances   = gen_balances;
      timestamp  = 0
    }

  let genesis_pos minter_id =
    {
      id         = 0;
      height     = 0;
      minter     = minter_id;
      parent     = None;
      difficulty = 10000;
      balances   = gen_balances; 
      timestamp  = 0
    }

  let equals a b =
    a.id = b.id

end


