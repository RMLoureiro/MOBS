type node_id  = int
type block_id = int
type balances = (node_id * float) list

(** type of a block's representation *)
type 'a t =
  {
    id               : block_id;
    height           : int;
    minter           : node_id;
    parent           : node_id option;
    balances         : balances;
    difficulty       : int;
    total_difficulty : int;
    timestamp        : Clock.t;
    contents         : 'a;
  }

let id block = block.id

module type BlockSig = sig
  type block_contents

  type block = block_contents t

  (** construct a new block, given the minter_id and parent block and block data *)
  val create : node_id -> block -> block_contents -> block

  (** returns the null block *)
  val null : block_contents -> block

  (** get the height of the block *)
  val height : 'a t -> int

  (** get the parent block *)
  val parent : 'a t -> node_id option

  (** get the id of the node that minted the block *)
  val minter : 'a t -> node_id

  (** get the id of the block *)
  val id : 'a t -> block_id

  (** get the id of a block option *)
  val opt_id : 'a t option -> block_id

  (** get the balances of the nodes *)
  val balances : 'a t -> balances

  (** get the timestamp of the block's creation *)
  val timestamp : 'a t -> Clock.t

  (** get the difficulty of the block *)
  val difficulty : 'a t -> int

  (** get the total difficulty of the chain *)
  val total_difficulty : 'a t -> int

  (** get the total amount of coins *)
  val total_coins : 'a t -> float

  (** get the genesis block, assigning a minter and a initial dificulty *)
  val genesis_pow : node_id -> int -> block_contents -> block

  (** get the genesis block, assigning a minter *)
  val genesis_pos : node_id -> block_contents -> block

  (** check if two blocks are equal *)
  val equals : 'a t -> 'a t -> bool
end

module type BlockContent = sig
  type t
end


module Make(Logger : Logging.Logger)(BlockContent:BlockContent) : (BlockSig with type block_contents = BlockContent.t and type block = BlockContent.t t) = struct

  type block_contents = BlockContent.t

  type block = block_contents t

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

  let create minter parent data =
    latest_id := !latest_id + 1;
    Logger.print_create_block minter !latest_id;
    {
      id               = !latest_id;
      height           = parent.height + 1;
      minter           = minter;
      parent           = Some parent.id;
      difficulty       = parent.difficulty;
      total_difficulty = parent.total_difficulty + parent.difficulty;
      balances         = reward minter parent.balances;
      timestamp        = Clock.get_timestamp ();
      contents         = data;
    }

  let null content =
    {
      id               = -1;
      height           = 0;
      minter           = -1;
      parent           = None;
      difficulty       = 0;
      total_difficulty = 0;
      balances         = [];
      timestamp        = Clock.get_timestamp ();
      contents         = content;
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

  let total_difficulty block = block.total_difficulty

  let genesis_pow minter_id difficulty content =
    {
      id               = 0;
      height           = 0;
      minter           = minter_id;
      parent           = None;
      difficulty       = difficulty * !Parameters.General.interval;
      total_difficulty = difficulty * !Parameters.General.interval;
      balances         = gen_balances;
      timestamp        = 0;
      contents         = content;
    }

  let genesis_pos minter_id content =
    {
      id               = 0;
      height           = 0;
      minter           = minter_id;
      parent           = None;
      difficulty       = 10000;
      total_difficulty = 10000;
      balances         = gen_balances; 
      timestamp        = 0;
      contents         = content;
    }

  let equals a b =
    a.id = b.id

end


