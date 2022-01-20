let r = 3
let slot_duration = 3000
let num_proposers = 1

open Implementation

type slot = Slot of int

module BlockContents = struct
  type t = { slot:int} (* the previous block's hash is already included in the block *)
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

  let genesis_block = CardanoBlock.genesis_pos 0 {slot=0}

  type node_data = {
    mutable best_chain : value list;
    mutable slot : slot;
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
        slot = Slot(0);
      }
    }

  let chain_height (node:t) =
    CardanoBlock.height node.state

  let check_proposer id slot =
    id = ((slot-1) mod !Parameters.General.num_nodes)
  
  let valid_chain (c:value list) : bool =
    let rev_chain = List.rev c in
    let validity = ref true in
    let valid_block (blk:value) =
      if (!validity) && check_proposer (CardanoBlock.minter blk) blk.contents.slot then
        validity := true
      else
        validity := false
    in
    List.iter valid_block rev_chain;
    !validity

  let max_valid chain1 chain2 =
    let v1 = valid_chain chain1 in
    let v2 = valid_chain chain2 in
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
    if check_proposer node.id slot then
      (
        let new_blk = CardanoBlock.create node.id node.state {slot=slot} in
        let node_sig = Signature(node.data.slot, State(CardanoBlock.id node.state), node.id) in
        node.data.best_chain <- [new_blk]@node.data.best_chain;
        node.state <- new_blk;
        CardanoNetwork.gossip node.id (Block(new_blk, node_sig));
        node
      )
    else
      node

  let process_msg (node:t) (msg:cardano_msg) (sender:int) =
    match msg with
    | Block(blk,signature) -> 
      (
        let slot,(*prev_head*)_,leader = match signature with Signature(Slot(s),State(prev),node_id) -> s,prev,node_id in
        let valid_leader = check_proposer leader slot in
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
        node.data.best_chain <- max_valid node.data.best_chain chain;
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
    node.data.slot <- Slot(new_slot);
    check_slot_leader node

  let handle (node:t) (event:ev) =
    let _ = 
      match event with
      | CardanoEvent.Message(sender,_,_,_,msg) -> process_msg node msg sender
      | CardanoEvent.Timeout(_,_,label) ->
        begin
          match label with
          | "inc_slot" -> next_slot node
          | _ -> node
        end
      | _ -> node
    in
    let node_slot = match node.data.slot with Slot(x) -> x in
    if node_slot = node.state.contents.slot then
      next_slot node
    else
      node

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
