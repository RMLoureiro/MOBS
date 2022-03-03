type node_id  = int
type block_id = int
type balances = (node_id * float) list

(** Mandatory information that every block possesses. *)
type block_header = 
{
  id               : block_id; (** id of the block *)
  height           : int; (** height of the block in the chain *)
  minter           : node_id; (** id of the node who minted the block *)
  parent           : node_id option; (** parent of the block *)
  timestamp        : Clock.t; (** timestamp when the block was created *)
  balances         : balances; (** the balance of each node *)
  difficulty       : int; (** difficulty of creating the block *)
  total_difficulty : int; (** total difficulty of creating the chain up to this block *)
}

(** Type of a block: header + user-defined information *)
type 'a t =
  {
    header   : block_header;
    contents : 'a;
  }

let id (block:'a t) = block.header.id

(** Module that contains the reward function to be called when creating a new block. *)
module type BlockRewards = sig

  (** Given the minter of the block and the balances, and produce the resulting balances.
    @param minter the node creating the block
    @param balances the current balances of each node
  *)
  val reward : int -> balances -> balances

end

(** Module for non-varying balances throughout the simulation. *)
module NoRewards = struct

  let reward _ balances =
    balances

end

(** Base module for rewarding the minter of a block. *)
module BaseRewards = struct

  let reward minter balances =
    let gets_reward (id,bal) =
      if id = minter then
        (id, bal +. (bal *. !Parameters.General.reward))
      else
        (id, bal)
      in
    List.map gets_reward balances

end

(** The signature for a block module. *)
module type BlockSig = sig
  (** The user-defined contents of a block. *)
  type block_contents

  (** The type of the block. Application of {b block_contents} to {b t}. *)
  type block = block_contents t

  (** Create a new block.
    @param node_id the id of the minter
    @param block the parent block
    @param block_contents the content for the new block
  *)
  val create : node_id -> block -> block_contents -> block

  (** Returns the null block *)
  val null : block_contents -> block

  (** Returns the height of a block.
    @param block a block
  *)
  val height : 'a t -> int

  (** Returns the parent of a block.
    @param block a block
  *)
  val parent : 'a t -> node_id option

  (** Returns the id of the node that minted the block.
    @param block the block whose minter we want
  *)
  val minter : 'a t -> node_id

  (** Return the id of a block given block.
    @param block the block whose id we want
  *)
  val id : 'a t -> block_id

  (** Return the id of a block option. Returns -1 if NONE.
    @param block_opt SOME({b block}) or {b NONE} 
  *)
  val opt_id : 'a t option -> block_id

  (** Return the balances contained in a block.
    @param block a block
  *)
  val balances : 'a t -> balances

  (** Return the timestamp of the block's creation.
    @param block a block
  *)
  val timestamp : 'a t -> Clock.t

  (** Return the difficulty of the block.
    @param block a block
  *)
  val difficulty : 'a t -> int

  (** Return the total difficulty of the chain.
    @param block the head of the chain
  *)
  val total_difficulty : 'a t -> int

  (** Return the total amount of coins.
    @param block the head of the chain
  *)
  val total_coins : 'a t -> float

  (** Return the genesis block for PoW.
    @param node_id placeholder minter
    @param difficulty the initial difficulty
    @param contents the contents to be included in the genesis block
  *)
  val genesis_pow : node_id -> int -> block_contents -> block

  (** Return the genesis block for PoS.
    @param node_id placeholder minter
    @param contents the contents to be included in the genesis block
  *)
  val genesis_pos : node_id -> block_contents -> block

  (** Check if two blocks are equal (have the same ID).
    @param block1 a block
    @param block2 another block  
  *)
  val equals : 'a t -> 'a t -> bool
end

(** Wrapper for the type of a block's contents. *)
module type BlockContent = sig
  type t (** The type of a block's contents. *)
end

(** Functor for creating the implementation for a Block, given the Logger, BlockContent and BlockRewards modules. *)
module Make(Logger : Logging.Logger)(BlockContent:BlockContent)(Rewards:BlockRewards) : (BlockSig with type block_contents = BlockContent.t and type block = BlockContent.t t) = struct

  type block_contents = BlockContent.t

  type block = block_contents t

  let latest_id = ref 0

  let create minter (parent:block) (data:block_contents) =
    latest_id := !latest_id + 1;
    Logger.print_create_block minter !latest_id;
    let bal = Rewards.reward minter parent.header.balances in
    {
      header = {
        id               = !latest_id;
        height           = parent.header.height + 1;
        minter           = minter;
        parent           = Some parent.header.id;
        difficulty       = parent.header.difficulty;
        total_difficulty = parent.header.total_difficulty + parent.header.difficulty;
        balances         = bal;
        timestamp        = Clock.get_timestamp ();
      };
      contents = data;
    }

  let null content =
    {
      header = {
        id               = -1;
        height           = 0;
        minter           = -1;
        parent           = None;
        difficulty       = 0;
        total_difficulty = 0;
        balances         = [];
        timestamp        = Clock.get_timestamp ();
      };
      contents         = content;
    }

  let height block = block.header.height

  let parent block = block.header.parent

  let minter block = block.header.minter

  let id block = block.header.id

  let opt_id block_opt =
    match block_opt with
    | Some(blk) -> id blk
    | None -> -1

  let balances block = block.header.balances

  let gen_balances =
    if !Parameters.General.use_topology_file then
      (
        let open Yojson.Basic.Util in
        let ret = ref [] in
        let node_data = Parameters.General.parse_topology_file !Parameters.General.topology_filename in
        List.iter (
          fun node ->
            let node_id = node |> member "id" |> to_int in
            let stake   = node |> member "stake" |> to_number in
            ret := !ret@[(node_id,stake)];
        ) node_data;
        !ret
      )
    else
      (
      let gen_bal id =
        let r = Random.float 1.0 in
        let coins = max (r *. !Parameters.General.stdev_coins +. !Parameters.General.avg_coins) 0.0 in
        (id, coins)
      in 
      let r_state = Random.get_state () in
      let ret = List.init !Parameters.General.num_nodes gen_bal in
      Random.set_state r_state;
      ret
      )

  let total_coins block = 
    let rec total bal sum =
      match bal with
      | [] -> sum
      | (_,coins)::xs -> total xs (sum+.coins)
    in
    total block.header.balances 0.0

  let timestamp block = block.header.timestamp

  let difficulty block = block.header.difficulty

  let total_difficulty block = block.header.total_difficulty

  let genesis_pow minter_id difficulty content =
    {
      header = {
        id               = 0;
        height           = 1;
        minter           = minter_id;
        parent           = None;
        difficulty       = difficulty * !Parameters.General.interval;
        total_difficulty = difficulty * !Parameters.General.interval;
        balances         = gen_balances;
        timestamp        = 0;
      };
      contents         = content;
    }

  let genesis_pos minter_id content =
    {
      header = {
        id               = 0;
        height           = 1;
        minter           = minter_id;
        parent           = None;
        difficulty       = 10000;
        total_difficulty = 10000;
        balances         = gen_balances; 
        timestamp        = 0;
      };
      contents         = content;
    }

  let equals a b =
    a.header.id = b.header.id

end


