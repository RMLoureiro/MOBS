type node_id  = int
type block_id = int
type balances = (node_id * float) list

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

let latest_id = ref 0

(* TODO : change BLOCK into a functor, adding the ability to quickly hotswap how nodes are rewarded *)
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

let balances block = block.balances

let gen_balances =
  let gen_bal id =
    let r = Random.float 1.0 in
    let coins = max (r *. !Parameters.General.stdev_coins +. !Parameters.General.avg_coins) 0.0 in
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