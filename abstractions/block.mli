(** type of a block's representation *)
type t
type node_id
type block_id = int
type balances

(** construct a new block, given the height, minter_id and parent block *)
val create : int -> node_id -> t -> t

(** get the height of the block *)
val height : t -> int

(** get the parent block *)
val parent : t -> t option

(** get the id of the node that minted the block *)
val minter : t -> node_id

(** get the id of the block *)
val id : t -> block_id

(** get the balances of the nodes *)
val balances : t -> balances

(** get the timestamp of the block's creation *)
val timestamp : t -> Simulator.Clock.t

(** get the genesis block *)
val genesis : t

(** check if two blocks are equal *)
val equals : t -> t -> bool
