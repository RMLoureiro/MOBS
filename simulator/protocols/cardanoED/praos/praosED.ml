(*

  For simulation purposes, we can assume one node is in reality a stake pool, and its stake
  is the aggregate stake delegated by all nodes in the pool

  Each epoch has an associated <nonce> which is used to find which node will be the leader for the epoch

  Slots:
  - not all slots will be attibuted a slot leader
  - slots might have multiple leaders
  - slot leader identities remain unknown until they act

  Genesis Block:
  - list of stakeholders; their public key; their stake; an nonce
  - the nonce will be used to seed the slot leader election process

  State:
  - hash of the head of the blockchain

  Block:
  - slot when it was generated
  - current state
  - block proof
  - block data (txs)
  - signature of the node that generated the block
    - encompassing all the contents (slot, state, proof, data)

  Epoch:
  - set of R adjacent slots 
    - Note : R is a parameters of the protocol
  - for all slots in the same epoch, the same stake distribution is used for selecting leaders
    - (so, it uses the stakes of the last block from the previous epoch)

  Valid Block:
  - a block is valid if it was generated by a stakeholder in the <slot leader set> of the slot
    to which the block is attributed

  Missing Slot Data (Catchup):
  - Get data chain data from multiple nodes
  - Set current chain to the longest valid chain received
  - Proceed with the normal execution of the protocol

*)

let r = 3
let slot_duration = 3000
let num_proposers = 1

open Implementation

type slot = Slot of int
and epoch = {first:slot; last:slot}

module BlockContents = struct
  type t = { slot:int; epoch:epoch; credential:Abstractions.Pos.credential } (* the previous block's hash is already included in the block *)
end

type cardano_msg =
    Block of (BlockContents.t) Simulator.Block.t * signature (* send a newly created block, signed by leader *)
  | Chain of (BlockContents.t) Simulator.Block.t list        (* send the full chain of blocks *)
  | Req                                                      (* request the chain *)

and current_state = State of int
and signature = Signature of slot * current_state * int (* slot, prev_block_id, node_id *)


module CardanoMsg : (Simulator.Events.Message with type t = cardano_msg) = struct 
  type t = cardano_msg

  let to_json (m:t) : string =
    match m with
    | Block(blk,s) -> 
      let creator_id = match s with Signature(_,_,id) -> id in
      Printf.sprintf "{\"kind\":\"%s\",\"block\":%d,\"created_by\":%d}" "BLOCK" (Simulator.Block.id blk) creator_id
    | Chain(c) -> 
      let head = (match c with | [] -> -1 | blk::_ -> Simulator.Block.id blk) in
      Printf.sprintf "{\"kind\":\"%s\",\"chain_head\":%d}" "CHAIN" head
    | Req -> Printf.sprintf "{\"kind\":\"REQ\"}"

  let get_size (msg:t) =
    match msg with
    | Block(_) -> Simulator.Size.Megabyte(1)
    | Chain(c) -> Simulator.Size.Megabyte(List.length c)
    | Req      -> Simulator.Size.Bit(32)

  let processing_time (_:t) =
    10

  let identifier (msg:t) =
    match msg with
    | Block(blk,_) -> Simulator.Block.id blk
    | Chain(c) -> 
      begin
        match c with
        | [] -> -1
        | blk::_ -> Simulator.Block.id blk
      end
    | Req -> -1

end


module CardanoEvent   = Simulator.Events.MakeEvent(CardanoMsg);;
module CardanoQueue   = Simulator.Events.MakeQueue(CardanoEvent);;
module CardanoTimer   = Abstractions.Timer.Make(CardanoEvent)(CardanoQueue);;
module CardanoNetwork = Abstractions.Network.Make(CardanoEvent)(CardanoQueue)(CardanoMsg);;
module CardanoLogger  = Simulator.Logging.Make(CardanoMsg)(CardanoEvent);;
module CardanoBlock   = Simulator.Block.Make(CardanoLogger)(BlockContents)(Simulator.Block.BaseRewards);;
module CardanoPoS     = Abstractions.Pos.Make(CardanoLogger)(CardanoBlock);;


module CardanoNode : (Protocol.BlockchainNode with type ev=CardanoEvent.t and type value=CardanoBlock.block) = struct

  type value = CardanoBlock.block

  module V = struct
    type v = value
  end

  include Protocol.MakeBaseNode(V)

  type ev = CardanoEvent.t

  let genesis_block = CardanoBlock.genesis_pos 0 {slot=0;epoch={first=Slot(0);last=Slot(r)};credential=Credential(Node(-1),Block(-1),Selections(-1),Priority(-1),Params([]))}

  type node_data = {
    mutable best_chain : value list;
    mutable slot : slot;
    mutable epoch : epoch;
    mutable epoch_seed : value;
    mutable nonce : int;
  }

  type t = (node_data,value) Protocol.template

  let init id links region : (t) =
    {
      id = id;
      region = region;
      links = links;
      state = genesis_block;
      data = {
        best_chain = [genesis_block];
        slot=Slot(0);
        epoch={first=Slot(0);last=Slot(0)};
        epoch_seed = genesis_block;
        nonce = !Parameters.General.seed
      }
    }

  let chain_height (node:t) =
    CardanoBlock.height node.state

  let rec find_epoch_seed (chain:value list) (slot:int) =
    match chain with
    | [] -> genesis_block
    | x::xs -> 
      (
        if x.contents.slot < slot then
          x
        else
          find_epoch_seed xs slot
      )
    
  let valid_chain (c:value list) (nonce:int) : bool =
    let rev_chain = List.rev c in
    let validity = ref true in
    let valid_block (blk:value) =
      let epoch_start = match blk.contents.epoch.first with Slot(x) -> x in
      let epoch_seed = find_epoch_seed c epoch_start in
      if (!validity) && CardanoPoS.valid_proposer (CardanoBlock.minter blk) epoch_seed num_proposers [blk.contents.slot;nonce] (blk.contents.credential) then
        validity := true
      else
        validity := false
    in
    List.iter valid_block rev_chain;
    !validity

  let max_valid (node:t) chain1 chain2 =
    let v1 = valid_chain chain1 node.data.nonce in
    let v2 = valid_chain chain2 node.data.nonce in
    if v1 && v2 then
    (
      if List.length chain1 > List.length chain2 then
        chain1
      else
        chain2
    )
    else if v2 then chain2
    else chain1

  let check_slot_leader (node:t) =
    let slot = match node.data.slot with Slot(x) -> x in
    match CardanoPoS.is_proposer node.id node.data.epoch_seed num_proposers [slot;node.data.nonce] with
    | Some(cred) -> 
      (
        let new_blk = CardanoBlock.create node.id node.state {slot=slot;epoch=node.data.epoch;credential=cred} in
        let node_sig = Signature(node.data.slot, State(CardanoBlock.id node.state), node.id) in
        node.data.best_chain <- [new_blk]@node.data.best_chain;
        node.state <- new_blk;
        CardanoNetwork.gossip node.id (Block(new_blk, node_sig));
        node
      )
    | None -> node

  let process_msg (node:t) (msg:cardano_msg) (sender:int) =
    match msg with
    | Block(blk,signature) -> 
      (
        let slot,(*prev_head*)_,leader = match signature with Signature(Slot(s),State(prev),node_id) -> s,prev,node_id in
        let cred = blk.contents.credential in
        let valid_leader = CardanoPoS.valid_proposer leader node.data.epoch_seed num_proposers [slot;node.data.nonce] cred in
        let longer_chain = (CardanoBlock.height blk) > (CardanoBlock.height node.state) in
        if blk.contents.slot = slot && valid_leader && (CardanoBlock.minter blk) = leader && longer_chain then
          (
            node.data.best_chain <- [blk]@node.data.best_chain;
            node.state <- blk;
            node
          )
        else node
      )
    | Chain(chain) -> 
      (
        node.data.best_chain <- max_valid node node.data.best_chain chain;
        match node.data.best_chain with
        | [] -> node
        | head::_ -> 
          (
            node.state <- head;
            let cur_slot = match node.data.slot with Slot(x) -> x in
            node.data.slot <- Slot(max cur_slot head.contents.slot);
            node
          )
      )
    | Req -> 
      CardanoNetwork.send node.id sender (Chain(node.data.best_chain)); node

  let next_slot (node:t) =
    let new_slot = match node.data.slot with Slot(x) -> x+1 in
    let epoch_end = match node.data.epoch.last with Slot(x) -> x in
    CardanoTimer.set node.id slot_duration "inc_slot";
    if new_slot > epoch_end then
      (
        node.data.slot <- Slot(new_slot);
        node.data.epoch <- {first=Slot(new_slot);last=Slot(new_slot+r)};
        node.data.epoch_seed <- find_epoch_seed node.data.best_chain new_slot;
        check_slot_leader node
      )
    else
      (
        node.data.slot <- Slot(new_slot);
        check_slot_leader node
      )

  let handle (node:t) (event:ev) =
    match event with
    | CardanoEvent.Message(sender,_,_,msg) -> process_msg node msg sender
    | CardanoEvent.Timeout(_,_,label) ->
      begin
        match label with
        | "inc_slot" -> next_slot node
        | _ -> node
      end
    | _ -> node

end

module CardanoInitializer : (Protocol.Initializer with type node=CardanoNode.t and type ev=CardanoEvent.t) = struct
  type node = CardanoNode.t

  type ev = CardanoEvent.t

  let init nodes = 
    let evs = ref [] in
    Hashtbl.iter (fun nid _ -> evs := !evs @ [CardanoEvent.Timeout(nid, 0, "inc_slot")]) nodes;
    !evs
  
end

module CardanoProtocol = Protocol.Make.Blockchain(CardanoEvent)(CardanoQueue)(CardanoBlock)(CardanoTimer)(CardanoNode)(CardanoNode)(CardanoInitializer)(CardanoLogger)(Simulator.Statistics.Empty)(CardanoNetwork);;

