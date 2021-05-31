type node_id  = int
type block_id = int
type balances = (node_id * float) list

type t =
{
  id        : block_id;
  height    : int;
  minter    : node_id;
  parent    : t option;
  balances  : balances;
  timestamp : Simulator.Clock.t
}

let latest_id = ref 0

(** TODO : compute block rewards and update balances (requires the block storage) *)
let create height minter parent =
  latest_id := !latest_id + 1;
  {
    id        = !latest_id;
    height    = height;
    minter    = minter;
    parent    = Some parent;
    balances  = [];
    timestamp = Simulator.Clock.get_timestamp ()
  }

let height block = block.height

let parent block = block.parent

let minter block = block.minter

let id block = block.id

let balances block = block.balances

let timestamp block = block.timestamp

let genesis = {
  id        = 0;
  height    = 0;
  minter    = 0;
  parent    = None;
  balances  = [];
  timestamp = 0
}

let equals a b =
  a.id = b.id