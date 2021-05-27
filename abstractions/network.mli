(** a list of integer lists, where index <i> contains a list with the ids of nodes 
    that <i> can send messages to (possesses outbound links to) *)
type links = int list list

(** a list of integers, where index <i> contains the region for node with id <i> *)
type regions = int list

(** returns the region assigned to each node for the current simulation *)
val node_regions : regions

(** returns the links between nodes for the current simulation *)
val node_links : links

(** given the sender id and receiver id, returns the latency for a message *)
val get_latency : int -> int -> int

