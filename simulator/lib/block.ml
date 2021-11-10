type node_id  = int
type block_id = int
type balances = (node_id * float) list

(** mandatory information that every block possesses *)
type block_header = 
{
  id               : block_id;
  height           : int;
  minter           : node_id;
  parent           : node_id option;
  timestamp        : Clock.t;
  balances         : balances;
  difficulty       : int;
  total_difficulty : int;
}

(** type of a block: header + arbitrary information *)
type 'a t =
  {
    header   : block_header;
    contents : 'a;
  }

let id (block:'a t) = block.header.id

module type BlockRewards = sig

  (* given the minter of the block and the balances, and produce the resulting balances *)
  val reward : int -> balances -> balances

end

module NoRewards = struct

  let reward _ balances =
    balances

end

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
        (id+1, coins)
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


