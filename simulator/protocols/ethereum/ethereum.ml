open Implementation

let slot_duration = 8000;;
let max_slots = 32;;

let byzantine_exectuion = false;;

module BlockContents = struct
  type t = unit
end

type block = {
  hash : string;
  epoch : int;
  slot : int;
  content : string;
  mutable justified: bool;
  mutable finalized : bool;
}

(* Define the n-ary tree structure *)
type 'a ethereum_tree = 
  | Leaf of block
  | Node of block * 'a ethereum_tree list

(* Helper to extract block from block ethereum_tree *)
let get_block_from_tree t =
  match t with
  | Leaf b -> b
  | Node (b, _) -> b

let rec depth = function
| Leaf _ -> 1
| Node (_, children) -> 
    1 + (List.fold_left (fun acc child -> max acc (depth child)) 0 children)

(* Helper function to get just the deepest node *)
let get_deepest_node tree =
  let rec aux t depth =
    match t with
    | Leaf _ -> (t, depth)
    | Node (_, children) ->
        List.fold_left (fun (best_node, best_depth) child ->
          let (child_node, child_depth) = aux child (depth + 1) in
          if child_depth > best_depth then (child_node, child_depth) else (best_node, best_depth)
        ) (t, depth) children
  in
  let (deepest_leaf, _) = aux tree 1 in
  deepest_leaf

  let rec find_deepest_path tree =
  match tree with
  | Leaf block -> [block]
  | Node (block, []) -> [block]
  | Node (block, children) ->
      let deepest_child_path = 
        List.fold_left (fun best_path child ->
          let child_path = find_deepest_path child in
          if List.length child_path > List.length best_path then child_path
          else best_path
        ) [] children
      in
      block :: deepest_child_path


(* Function to add a new node at the end of the deepest path *)
let add_to_deepest tree new_block =
  let rec aux t =
    match t with
    | Leaf block -> Node (block, [Leaf new_block])
    | Node (block, children) ->
        if children = [] then Node (block, [Leaf new_block])
        else
          (* Find the child with the deepest subtree *)
          let depths = List.map (fun c -> (c, depth c)) children in
          let (deepest_child, _) = List.fold_left (fun (best_c, best_d) (c, d) -> if d > best_d then (c, d) else (best_c, best_d)) (List.hd depths) depths in
          let updated_children = List.map (fun c -> if c == deepest_child then aux c else c) children in
          Node (block, updated_children)
  in
  aux tree

let deepest_justified_on_path tree =
  let path = find_deepest_path tree in
  List.fold_left (fun acc block ->
    if block.justified then Some block else acc
  ) None path

let get_latest_checkpoint tree =
  let path = find_deepest_path tree in
  (* For each epoch, keep the block with the lowest slot *)
  let epoch_map = List.fold_left (fun acc block ->
    match List.assoc_opt block.epoch acc with
    | None -> (block.epoch, block) :: acc
    | Some b -> if block.slot < b.slot then (block.epoch, block) :: List.remove_assoc block.epoch acc else acc
  ) [] path in
  (* Find the checkpoint with the highest epoch *)
  match epoch_map with
  | [] -> None
  | _ ->
      let (_, latest_block) =
        List.fold_left (fun (max_epoch, max_block) (epoch, block) ->
          if epoch > max_epoch then (epoch, block) else (max_epoch, max_block)
        ) (fst (List.hd epoch_map), snd (List.hd epoch_map)) epoch_map
      in
      Some latest_block

let get_last_four_checkpoints tree =
  let path = find_deepest_path tree in
  let root_block = match tree with Leaf b -> b | Node (b, _) -> b in
  (* For each epoch, keep the block with the lowest slot *)
  let epoch_map = List.fold_left (fun acc block ->
    match List.assoc_opt block.epoch acc with
    | None -> (block.epoch, block) :: acc
    | Some b -> if block.slot < b.slot then (block.epoch, block) :: List.remove_assoc block.epoch acc else acc
  ) [] path in
  (* Sort epochs descending, get blocks *)
  let checkpoints =
    epoch_map
    |> List.sort (fun (e1, _) (e2, _) -> compare e2 e1)
    |> List.map snd
  in
  let rec fill acc lst =
    match acc with
    | l when List.length l = 4 -> l
    | l -> (match lst with
            | [] -> fill (root_block :: l) []
            | hd :: tl -> fill (hd :: l) tl)
  in
  match fill [] checkpoints with
  | [a; b; c; d] -> (a, b, c, d)
  | _ -> assert false


type msg = 
  Init of int (*sender*)
  | Sync of int * int * int (*sender, epoch, slot*)
  | Main of int * int * int (*sender, epoch, slot*)
  | Propose of int * int * int * block * block (*sender, epoch, slot, block, parent*)
  | Attestation of int * block ethereum_tree * block ethereum_tree (*sender, block, checkpoint as trees*)
  | JustificationFinalization of int (*sender*)

module EthereumMsg : (Simulator.Events.Message with type t = msg) = struct 
  type t = msg

  let to_json (msg:t) : string = 
    match msg with
    | Init(sender) ->  Printf.sprintf "{\"type\":\"Init\", \"node\":\"%d\"}" sender
    | Sync(sender, epoch, slot) ->  Printf.sprintf "{\"type\":\"Sync\", \"node\":\"%d\", \"epoch\":\"%d\", \"slot\":\"%d\"}" sender epoch slot
    | Main(sender, epoch, slot) ->  Printf.sprintf "{\"type\":\"Main\", \"node\":\"%d\", \"epoch\":\"%d\", \"slot\":\"%d\"}" sender epoch slot
    | Propose(sender, epoch, slot, _, _) -> Printf.sprintf "{\"type\":\"Propose\", \"node\":\"%d\", \"epoch\":\"%d\", \"slot\":\"%d\"}" sender epoch slot
    | Attestation(sender, block_tree, checkpoint_tree) ->
      let block_hash = (get_block_from_tree block_tree).hash in
      let checkpoint_hash = (get_block_from_tree checkpoint_tree).hash in
      Printf.sprintf "{\"type\":\"Attestation\", \"node\":\"%d\", \"block_hash\":\"%s\", \"checkpoint_hash\":\"%s\"}" sender block_hash checkpoint_hash
    | JustificationFinalization(sender) -> Printf.sprintf "{\"type\":\"JustificationFinalization\", \"node\":\"%d\"}" sender


  let get_size (msg:t) =
    match msg with
    | Init (_) -> Simulator.Size.Bit(32)
    | Sync (_,_,_) -> Simulator.Size.Bit(32)
    | Main (_,_,_) -> Simulator.Size.Bit(32)
    | Propose(_,_,_,_,_) -> Simulator.Size.Bit(32)
    | Attestation(_,_,_) -> Simulator.Size.Bit(32)
    | JustificationFinalization(_) -> Simulator.Size.Bit(32)

  let processing_time (_:t) =
    0

  let identifier (msg:t) =
    match msg with
    | Init(sender) -> sender * 3
    | Sync(sender, _, slot) -> sender * slot * 5
    | Main(sender, _, slot) -> sender * slot * 7
    | Propose(sender, _, slot, _, _) -> sender * slot
    | Attestation(sender, _, _) -> sender * 11
    | JustificationFinalization(sender) -> sender * 5
end


module EthereumEvent   = Simulator.Events.MakeEvent(EthereumMsg);;
module EthereumQueue   = Simulator.Events.MakeQueue(EthereumEvent);;
module EthereumNetwork = Abstractions.Network.Make(EthereumEvent)(EthereumQueue)(EthereumMsg);;
module EthereumLogger  = Simulator.Logging.Make(EthereumMsg)(EthereumEvent);;
module EthereumTimer   = Abstractions.Timer.Make(EthereumEvent)(EthereumQueue);;
module EthereumBlock   = Simulator.Block.Make(EthereumLogger)(BlockContents)(Simulator.Block.BaseRewards);;

module MBPTimeArg = struct
  let label = "median-block-propagation-time"
  let use_intervals = false
  let format = 1
end

module EthereumStatistics = Simulator.Statistics.Make.Median(MBPTimeArg);;

module EthereumNode : (Protocol.BlockchainNode with type ev=EthereumEvent.t and type value=EthereumBlock.block) = struct

  type value = EthereumBlock.block

  module V = struct
    type v = value
  end

  include Protocol.MakeBaseNode(V)

  type ev = EthereumEvent.t

  type node_data = {
    mutable tree : block ethereum_tree;
    mutable proposer : bool;
    mutable attester : bool;
    mutable slot : int; (* round *)
    mutable epoch : int; (* round *)
    mutable previous_slot : int; (* round *)
    mutable last_justified_checkpoint : int;
    mutable attestation_quorum : (int, (int * block ethereum_tree * block ethereum_tree) list) Hashtbl.t;
    mutable current_block : block ethereum_tree;
    mutable synced_this_slot : bool;
  }

  type t = (node_data, value) Protocol.template

  let init id links region : (t) =
    {
      id = id;
      region = region;
      links = links;
      state = EthereumBlock.null ();
      data  = {
        tree = Node({
          hash = "";
          slot = 0;
          epoch = 0;
          content = "";
          justified = true;
          finalized = true;
        }, []);
        proposer = false;
        attester = false;
        slot = 0;
        epoch = 0;
        previous_slot = 0;
        last_justified_checkpoint = 0; (*need to create object for epoch(int) and block*)
        attestation_quorum = Hashtbl.create 100;
        current_block = Node({
          hash = "";
          slot = 0;
          epoch = 0;
          content = "";
          justified = true;
          finalized = true;
        }, []);
        synced_this_slot = false;
      }
    }

    let receive_init (node:t) =
      let () = EthereumTimer.set node.id slot_duration "slot" in
      (* funcoes determinar proposer e attester*)
      if (node.id == 1 || node.id == 2 || node.id == 3) then
        node.data.proposer <- true;
      if(node.id == 4 || node.id == 5 || node.id == 6 || node.id == 7) then
        node.data.attester <- true;
      let () = EthereumNetwork.send node.id node.id (Main(node.id, node.data.epoch, node.data.slot)) in
      node

    let propose_block (node:t) =
      let path : block list = find_deepest_path node.data.tree in
      let parent = 
        match List.find_opt (fun (b : block) -> b.epoch = node.data.epoch - 1) path with
        | Some p -> p
        | None -> get_block_from_tree node.data.tree
      in
      let parent_hash = parent.hash in
      let new_node : block = { hash = Printf.sprintf "%x%x%x" (Random.bits ()) (Random.bits ()) (Random.bits ()); epoch = node.data.epoch; slot = node.data.slot; content = parent_hash; justified = false; finalized = false } in
      node.data.tree <- add_to_deepest node.data.tree new_node;
      EthereumNetwork.gossip node.id (Propose(node.id, node.data.epoch, node.data.slot, new_node, parent));
      node.data.proposer <- false

    let attest (node:t) =
      let last_justified_checkpoint = 
          match deepest_justified_on_path node.data.tree with 
          Some b -> Leaf b
          | None -> Leaf { hash = ""; epoch = 0; slot = 0; content = ""; justified = false; finalized = false } in
        let current_checkpoint =
          match get_latest_checkpoint node.data.tree with
          Some b -> Leaf b
          | None -> Leaf { hash = ""; epoch = 0; slot = 0; content = ""; justified = false; finalized = false } in
        if ((get_block_from_tree current_checkpoint).epoch == node.data.epoch || node.data.slot >= 11) then
          EthereumNetwork.gossip node.id (Attestation(node.id, current_checkpoint, last_justified_checkpoint))

    let receive_main (node:t) _ slot =
      let () = if node.data.synced_this_slot = false then EthereumNetwork.send node.id node.id (Sync(node.id, node.data.epoch, slot)) in
    

      let () = if (node.data.proposer) then
        propose_block node
      in
      
      let () = if (node.data.attester) then
        (*broadcast attestation*)
        attest node
      in      
      node

    let receive_sync(node:t) _ slot =
      if (node.data.previous_slot == slot) then
        begin
          let () = node.data.synced_this_slot <- true in
          let () = node.data.previous_slot <- slot in
          if (node.data.slot mod 32 = 0 && node.data.proposer) then
            EthereumNetwork.send node.id node.id (JustificationFinalization(node.id));
            node.data.attester <- false
        end;
      node

        (* Count how many votes in attestation_quorum match the given source and target *)
    let count_matching_checkpoint_vote (node : t) (source : block) (target : block) : int =
      let epoch = target.epoch in
      let att_list =
        match Hashtbl.find_opt node.data.attestation_quorum epoch with
        | Some l -> l
        | None -> []
      in
      List.fold_left (fun acc (_, block_tree, checkpoint_tree) ->
        let block = get_block_from_tree block_tree in
        let checkpoint = get_block_from_tree checkpoint_tree in
        if block.hash = target.hash && checkpoint.hash = source.hash then acc + 1 else acc
      ) 0 att_list


    let supermajority_link (_:block) (_:block) : bool =
      if (byzantine_exectuion) then
        true
    else
      false


    (* For simplicity, assume a fixed number of nodes, e.g., 6 *)
    let receive_justification_finalization(node:t) =
      let source_opt = deepest_justified_on_path node.data.tree in
      let target_opt = get_latest_checkpoint node.data.tree in
      (match source_opt, target_opt with
        | Some source, Some target ->
            let nb_checkpoint_vote = count_matching_checkpoint_vote node source target in
            ignore nb_checkpoint_vote; (* TODO: handle justification/finalization logic here *)
            target.justified <- true;
            let a, b, c, d = get_last_four_checkpoints node.data.tree in
                if a.justified && b.justified && (supermajority_link a c) then
                  a.finalized <- true
                else if b.justified && (supermajority_link b c) then
                  b.finalized <- true
                else if b.justified && c.justified && (supermajority_link b d) then
                  b.finalized <- true
                else if c.justified && (supermajority_link c d) then
                  c.finalized <- true
        | _ -> assert(false)
      );
      node

    let receive_propose (node:t) _ _ _ block parent =
      let rec add_block_to_parent tree parent_hash new_block =
        match tree with
        | Leaf b when b.hash = parent_hash -> Node (b, [Leaf new_block])
        | Leaf b -> Leaf b
        | Node (b, children) when b.hash = parent_hash -> Node (b, (Leaf new_block) :: children)
        | Node (b, children) -> Node (b, List.map (fun c -> add_block_to_parent c parent_hash new_block) children)
      in
      let rec block_exists tree hash =
        match tree with
        | Leaf b -> b.hash = hash
        | Node (b, children) -> b.hash = hash || List.exists (fun c -> block_exists c hash) children
      in
      let parent_hash = parent.hash in
      if not (block_exists node.data.tree block.hash) then begin
        if parent_hash = "" then
          node.data.tree <- add_to_deepest node.data.tree block
        else
          node.data.tree <- add_block_to_parent node.data.tree parent_hash block
      end;
      node

    let receive_attestation (node:t) sender block checkpoint =
      (* Save (sender, block, checkpoint) for later vote counting *)
      let epoch = (get_block_from_tree block).epoch in
        let current_list =
          match Hashtbl.find_opt node.data.attestation_quorum epoch with
          | Some l -> l
          | None -> []
        in
          Hashtbl.replace node.data.attestation_quorum epoch ((sender, block, checkpoint) :: current_list);
      node

    (* Increment slot and/or epochs *)
    let receive_slot_trigger (node:t) =
      let () = node.data.slot <- node.data.slot + 1 in
      if (node.data.slot >= max_slots) then
        begin
          let () = node.data.epoch <- node.data.epoch + 1 in
          node.data.slot <- 0;
          if (node.id == 1 || node.id == 2 || node.id == 3) then
            node.data.proposer <- true;
          if(node.id == 4 || node.id == 5 || node.id == 6 || node.id == 7) then
            node.data.attester <- true;
        end;
      EthereumNetwork.send node.id node.id (Main(node.id, node.data.epoch, node.data.slot));
      EthereumTimer.set node.id slot_duration "slot";
      node

    let handle (node:t) (event:ev) : t =
      match event with
        | EthereumEvent.Message(_,_,_,_,msg) -> 
          begin
          match msg with
          | Init(_) -> receive_init node
          | Sync(_, epoch, slot) -> receive_sync node epoch slot
          | Main(_, epoch, slot) -> receive_main node epoch slot
          | Propose(sender, epoch, slot, block, parent) -> receive_propose node sender epoch slot block parent
          | Attestation(sender, block, checkpoint) -> receive_attestation node sender block checkpoint
          | JustificationFinalization(_) -> receive_justification_finalization node
          end
        | EthereumEvent.Timeout(_,_,label) ->
            begin
              match label with
              | "slot" -> receive_slot_trigger node
              | _ -> node
              end
        | _ -> node

  let chain_height (node:t) = 
    EthereumBlock.height node.state

end


module EthereumInitializer : (Protocol.Initializer with type node = EthereumNode.t and type ev = EthereumEvent.t) = struct
  type node = EthereumNode.t

  type ev = EthereumEvent.t

  let init nodes = 
    let evs = ref [] in
    Hashtbl.iter (fun nid _ -> evs := !evs @ [EthereumEvent.Message(0, nid, 0, 0, Init(nid))]) nodes;
    !evs

end

module EthereumProtocol = Protocol.Make.Blockchain(EthereumEvent)(EthereumQueue)(EthereumBlock)(EthereumTimer)(EthereumNode)(EthereumNode)(EthereumInitializer)(EthereumLogger)(EthereumStatistics)(EthereumNetwork);;
