open Implementation

(* Protocol constants *)
module Constants = struct
  let slot_duration = 200
  let max_slots = 32
  let byzantine_execution = true
end

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

type 'a ethereum_tree = 
  | Leaf of block
  | Node of block * 'a ethereum_tree list

let get_block_from_tree t =
  match t with
  | Leaf b -> b
  | Node (b, _) -> b

let rec depth = function
| Leaf _ -> 1
| Node (_, children) -> 
    1 + (List.fold_left (fun acc child -> max acc (depth child)) 0 children)

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


let add_to_deepest tree new_block =
  let rec aux t =
    match t with
    | Leaf block -> Node (block, [Leaf new_block])
    | Node (block, children) ->
        if children = [] then Node (block, [Leaf new_block])
        else
          let depths = List.map (fun c -> (c, depth c)) children in
          let (deepest_child, _) = List.fold_left (fun (best_c, best_d) (c, d) -> if d > best_d then (c, d) else (best_c, best_d)) (List.hd depths) depths in
          let updated_children = List.map (fun c -> if c == deepest_child then aux c else c) children in
          Node (block, updated_children)
  in
  aux tree

let rec add_block_to_parent tree parent_hash new_block =
  match tree with
  | Leaf b when b.hash = parent_hash -> Node (b, [Leaf new_block])
  | Leaf b -> Leaf b
  | Node (b, children) when b.hash = parent_hash -> Node (b, (Leaf new_block) :: children)
  | Node (b, children) -> Node (b, List.map (fun c -> add_block_to_parent c parent_hash new_block) children)

let rec block_exists tree hash =
  match tree with
  | Leaf b -> b.hash = hash
  | Node (b, children) -> b.hash = hash || List.exists (fun c -> block_exists c hash) children

let insert_block tree ~block ~parent_hash_opt =
  match parent_hash_opt with
  | None -> add_to_deepest tree block
  | Some parent_hash ->
      if parent_hash = "" then add_to_deepest tree block
      else if block_exists tree parent_hash then add_block_to_parent tree parent_hash block
      else add_to_deepest tree block

let deepest_justified_on_path tree =
  let path = find_deepest_path tree in
  List.fold_left (fun acc block ->
    if block.justified then Some block else acc
  ) None path

let get_latest_checkpoint tree =
  let path = find_deepest_path tree in
  let epoch_map = List.fold_left (fun acc block ->
    match List.assoc_opt block.epoch acc with
    | None -> (block.epoch, block) :: acc
    | Some b -> if block.slot < b.slot then (block.epoch, block) :: List.remove_assoc block.epoch acc else acc
  ) [] path in
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
  let epoch_map = List.fold_left (fun acc block ->
    match List.assoc_opt block.epoch acc with
    | None -> (block.epoch, block) :: acc
    | Some b -> if block.slot < b.slot then (block.epoch, block) :: List.remove_assoc block.epoch acc else acc
  ) [] path in
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
  | Main of int * int * int (*sender, epoch, slot*)
  | Propose of int * int * int * block ethereum_tree * block (*sender, epoch, slot, full tree, new block*)
  | Attestation of int * int * block ethereum_tree * block ethereum_tree (*sender, epoch, block, checkpoint as trees*)
  | JustificationFinalization of int (*sender*)
  | FinalizedNode of int * int * int * block ethereum_tree * block (*sender, epoch, slot, tree, finalized block*)

module EthereumMsg : (Simulator.Events.Message with type t = msg) = struct 
  type t = msg

  let to_json (msg:t) : string = 
    match msg with
    | Init(sender) ->  Printf.sprintf "{\"type\":\"Init\", \"node\":\"%d\"}" sender
      | Main(sender, epoch, slot) ->  Printf.sprintf "{\"type\":\"Main\", \"node\":\"%d\", \"epoch\":\"%d\", \"slot\":\"%d\"}" sender epoch slot
      | Propose(sender, epoch, slot, tree, new_block) ->
        let head_hash = (get_block_from_tree tree).hash in
        Printf.sprintf "{\"type\":\"Propose\", \"node\":\"%d\", \"epoch\":\"%d\", \"slot\":\"%d\", \"head_hash\":\"%s\", \"block_hash\":\"%s\"}"
          sender epoch slot head_hash new_block.hash
    | Attestation(sender, epoch, block_tree, checkpoint_tree) ->
      let block_hash = (get_block_from_tree block_tree).hash in
      let checkpoint_hash = (get_block_from_tree checkpoint_tree).hash in
      Printf.sprintf "{\"type\":\"Attestation\", \"node\":\"%d\", \"epoch\":\"%d\", \"block_hash\":\"%s\", \"checkpoint_hash\":\"%s\"}" sender epoch block_hash checkpoint_hash
    | JustificationFinalization(sender) -> Printf.sprintf "{\"type\":\"JustificationFinalization\", \"node\":\"%d\"}" sender
    | FinalizedNode(sender, epoch, slot, _, finalized_block) ->
      Printf.sprintf "{\"type\":\"FinalizedNode\", \"node\":\"%d\", \"epoch\":\"%d\", \"slot\":\"%d\", \"finalized_hash\":\"%s\"}"
        sender epoch slot finalized_block.hash


  let get_size (msg:t) =
    match msg with
    | Init (_) -> Simulator.Size.Bit(32)
    | Main (_,_,_) -> Simulator.Size.Bit(32)
  | Propose(_,_,_,_,_) -> Simulator.Size.Bit(32)
    | Attestation(_,_,_,_) -> Simulator.Size.Bit(32)
    | JustificationFinalization(_) -> Simulator.Size.Bit(32)
  | FinalizedNode(_,_,_,_,_) -> Simulator.Size.Bit(32)

  let processing_time (_:t) =
    0

  let identifier (msg:t) =
    match msg with
    | Init(sender) -> sender * 3
    | Main(sender, _, slot) -> sender * slot * 7
  | Propose(sender, _, slot, _, _) -> sender * slot
    | Attestation(sender, epoch, _, _) -> sender * epoch * 11
    | JustificationFinalization(sender) -> sender * 5
  | FinalizedNode(sender, _, _, _, _) -> sender * 13
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
    mutable attestations_sent : int;
    mutable slot : int;
    mutable epoch : int;
    mutable attestation_quorum : (int, (int * block ethereum_tree * block ethereum_tree) list) Hashtbl.t;
    mutable current_block : block ethereum_tree;
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
          hash = "GENESIS";
          slot = 0;
          epoch = 0;
          content = "";
          justified = true;
          finalized = true;
        }, []);
        proposer = false;
        attestations_sent = 0;
        slot = 0;
        epoch = 0;
        attestation_quorum = Hashtbl.create 100;
        current_block = Node({
          hash = "GENESIS";
          slot = 0;
          epoch = 0;
          content = "";
          justified = true;
          finalized = true;
        }, []);
      }
    }

    let receive_init (node:t) =
      node.data.attestations_sent <- 0;
      if (node.id == 1 || node.id == 2 || node.id == 3) then
        begin
          node.data.proposer <- true;
        end;
      EthereumNetwork.send node.id node.id (Main(node.id, node.data.epoch, node.data.slot));
      EthereumTimer.set node.id Constants.slot_duration "increment_slot";
      node

    let receive_main (node:t) _ _ =
      if (node.data.proposer && (node.data.slot == node.id || node.id == 3)) then
        begin
          (* if byzantine && node == 3 then do nothing until slot 30 then send propose with slot = 0 *)
          let path : block list = find_deepest_path node.data.tree in
          let parent = 
            match List.find_opt (fun (b : block) -> b.epoch = node.data.epoch - 1) path with
            | Some p -> p
            | None -> get_block_from_tree node.data.tree
          in
          let parent_hash = parent.hash in
          let new_node : block = { hash = Printf.sprintf "%x%x%x" (Random.bits ()) (Random.bits ()) (Random.bits ()); epoch = node.data.epoch; slot = node.data.slot; content = parent_hash; justified = false; finalized = false } in
          node.data.tree <- insert_block node.data.tree ~block:new_node ~parent_hash_opt:(Some parent_hash);
          if (Constants.byzantine_execution && (node.id == 3) && (node.data.slot > 28)) then
            begin
              node.data.proposer <- false;
              EthereumNetwork.gossip node.id (Propose(node.id, node.data.epoch, node.data.slot, node.data.tree, new_node));
            end
          else if (node.id <> 3) then
            begin
              node.data.proposer <- false;
              EthereumNetwork.gossip node.id (Propose(node.id, node.data.epoch, node.data.slot, node.data.tree, new_node));
            end
        end;

      
      
      if (node.data.slot == 32 && node.id == 1) then EthereumNetwork.send node.id node.id (JustificationFinalization(node.id));

      if (node.data.attestations_sent < 3 && node.data.slot >= 11) then
        begin
          node.data.attestations_sent <- node.data.attestations_sent + 1;
          let root_block = get_block_from_tree node.data.tree in
          let last_justified_checkpoint = 
              match deepest_justified_on_path node.data.tree with 
              | Some b -> Leaf b
              | None -> Leaf root_block in
            let current_checkpoint =
              match get_latest_checkpoint node.data.tree with
              | Some b -> Leaf b
              | None -> Leaf root_block in
            EthereumNetwork.gossip node.id (Attestation(node.id, node.data.epoch, current_checkpoint, last_justified_checkpoint));
          end;
      node

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


    let supermajority_link (node:t) (source:block) (target:block) : bool =
      (count_matching_checkpoint_vote node source target) > 7

    (* For simplicity, assume a fixed number of nodes, in this case, 10 *)
    let receive_justification_finalization(node:t) =
      let source_opt = deepest_justified_on_path node.data.tree in
      let target_opt = get_latest_checkpoint node.data.tree in
      (match source_opt, target_opt with
        | Some source, Some target ->
            let nb_checkpoint_vote = count_matching_checkpoint_vote node source target in
            if nb_checkpoint_vote > 7 then target.justified <- true;
            let a, b, c, d = get_last_four_checkpoints node.data.tree in
                if a.justified && b.justified && (supermajority_link node a c) then
                  begin
                    a.finalized <- true;
                    EthereumNetwork.gossip node.id (FinalizedNode(node.id, node.data.epoch, node.data.slot, node.data.tree, a));
                  end
                else if b.justified && (supermajority_link node b c) then
                  begin
                    b.finalized <- true;
                    EthereumNetwork.gossip node.id (FinalizedNode(node.id, node.data.epoch, node.data.slot, node.data.tree, b));
                  end
                else if b.justified && c.justified && (supermajority_link node b d) then
                  begin
                    b.finalized <- true;
                    EthereumNetwork.gossip node.id (FinalizedNode(node.id, node.data.epoch, node.data.slot, node.data.tree, b));
                  end
                else if c.justified && (supermajority_link node c d) then
                  begin
                    c.finalized <- true;
                    EthereumNetwork.gossip node.id (FinalizedNode(node.id, node.data.epoch, node.data.slot, node.data.tree, c));
                  end
        | _ -> assert(false)
      );
      node

    let receive_propose (node:t) _ _ _ tree _ =
      if (node.data.slot < 24) then node.data.tree <- tree;
      node

    let receive_finalized (node:t) _ _ _ tree _ =
      node.data.tree <- tree;
      node

    let receive_attestation (node:t) sender epoch block checkpoint =
        let current_list =
          match Hashtbl.find_opt node.data.attestation_quorum epoch with
          | Some l -> l
          | None -> []
        in
        let filtered_list = List.filter (fun (s, _, _) -> s <> sender) current_list in
        let new_list = (sender, block, checkpoint) :: filtered_list in
        Hashtbl.replace node.data.attestation_quorum epoch new_list;
      node

    let receive_slot_trigger (node:t) =
      if (node.data.slot > Constants.max_slots) then
        begin
          node.data.epoch <- node.data.epoch + 1;
          node.data.slot <- 0;
          EthereumNetwork.send node.id node.id (Init(node.id));
        end
      else
        begin
          EthereumNetwork.send node.id node.id (Main(node.id, node.data.epoch, node.data.slot));
          EthereumTimer.set node.id Constants.slot_duration "increment_slot";
          node.data.slot <- node.data.slot + 1;
        end;
      node

    let handle (node:t) (event:ev) : t =
      match event with  
        | EthereumEvent.Message(_,_,_,_,msg) -> 
          begin
          match msg with
          | Init(_) -> receive_init node
          | Main(_, epoch, slot) -> receive_main node epoch slot
          | Propose(sender, epoch, slot, tree, new_block) -> receive_propose node sender epoch slot tree new_block
          | Attestation(sender, epoch, block, checkpoint) -> receive_attestation node sender epoch block checkpoint
          | JustificationFinalization(_) -> receive_justification_finalization node
          | FinalizedNode(sender, epoch, slot, tree, finalized_block) -> receive_finalized node sender epoch slot tree finalized_block
          end
        | EthereumEvent.Timeout(_,_,label) ->
            begin
              match label with
              | "increment_slot" -> receive_slot_trigger node
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
