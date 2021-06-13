(** type of a block's representation *)
type t
type node_id  = int
type block_id = int
type balances = (node_id * float) list

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
