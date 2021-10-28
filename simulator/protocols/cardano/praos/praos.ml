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
  | Req   of int                                             (* request the chain to be sent to the ID <int> *)

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
    | Req(_) -> Printf.sprintf "{\"kind\":\"REQ\"}"

  let get_size (msg:t) =
    match msg with
    | Block(_) -> Simulator.Size.Megabyte(1)
    | Chain(c) -> Simulator.Size.Megabyte(List.length c)
    | Req(_)      -> Simulator.Size.Bit(32)

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
    | Req(_) -> -1

end


module CardanoEvent   = Simulator.Events.MakeEvent(CardanoMsg);;
module CardanoQueue   = Simulator.Events.MakeQueue(CardanoEvent);;
module CardanoTimer   = Abstractions.Timer.Make(CardanoEvent)(CardanoQueue);;
module CardanoNetwork = Abstractions.Network.Make(CardanoEvent)(CardanoQueue)(CardanoMsg);;
module CardanoLogger  = Simulator.Logging.Make(CardanoMsg)(CardanoEvent);;
module CardanoBlock   = Simulator.Block.Make(CardanoLogger)(BlockContents);;
module CardanoPoS     = Abstractions.Pos.Make(CardanoLogger)(CardanoBlock);;

module CardanoState = struct

  let genesis_block = CardanoBlock.genesis_pos 0 {slot=0;epoch={first=Slot(0);last=Slot(r)};credential=Credential(Node(-1),Block(-1),Selections(-1),Priority(-1),Params([]))}

  type value = CardanoBlock.block

  type internal_step = Idle | Scheduled of slot (* the latest slot for which the node checked if it was a proposer *)

  type node_data = {
    mutable best_chain : value list;
    mutable slot : slot;
    mutable epoch : epoch;
    mutable epoch_seed : value;
    mutable nonce : int;
    mutable internal_step : internal_step;
  }

  type t = (node_data, value) Protocol.template

  type message = cardano_msg

  type ev = CardanoEvent.t

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
        nonce = !Parameters.General.seed;
        internal_step = Idle
      }
    }

  let chain_height (node:t) =
    CardanoBlock.height node.state

end

module Aux = struct
  type message = cardano_msg
  type node    = CardanoState.t

  let rec find_epoch_seed (chain:CardanoState.value list) (slot:int) =
    match chain with
    | [] -> CardanoState.genesis_block
    | x::xs -> 
      (
        if x.contents.slot < slot then
          x
        else
          find_epoch_seed xs slot
      )

  let next_slot (node:node) =
    let new_slot = match node.data.slot with Slot(x) -> x+1 in
    let epoch_end = match node.data.epoch.last with Slot(x) -> x in
    if new_slot > epoch_end then
      (
        node.data.slot <- Slot(new_slot);
        node.data.epoch <- {first=Slot(new_slot);last=Slot(new_slot+r)};
        node.data.epoch_seed <- find_epoch_seed node.data.best_chain new_slot;
      )
    else
      (
        node.data.slot <- Slot(new_slot);
      )

end

module Proposal = struct
  type message = cardano_msg
  type node    = CardanoState.t

  let create_proposal (node:node) : (message option) =
    let proposal_required =
       match node.data.internal_step with
       | Idle -> true
       | Scheduled(Slot(v)) -> (Simulator.Clock.get_timestamp ()) / slot_duration >= v
    in
    if proposal_required then
    (
      Aux.next_slot node;
      CardanoTimer.set node.id slot_duration "inc_slot";
      node.data.internal_step <- Scheduled(node.data.slot);
      let slot = match node.data.slot with Slot(x) -> x in
      match CardanoPoS.is_proposer node.id node.data.epoch_seed num_proposers [slot;node.data.nonce] with
      | Some(cred) ->
        (
          let new_blk = CardanoBlock.create node.id node.state {slot=slot;epoch=node.data.epoch;credential=cred} in
          let node_sig = Signature(node.data.slot, State(CardanoBlock.id node.state), node.id) in
          Some(Block(new_blk, node_sig))
        )
      | None -> None
    )
    else
      None
  
end

module Validation = struct
  type message = cardano_msg
  type node    = CardanoState.t

  let valid_chain (c:CardanoState.value list) (nonce:int) : bool =
    let rev_chain = List.rev c in
    let validity = ref true in
    let valid_block (blk:CardanoState.value) =
      let epoch_start = match blk.contents.epoch.first with Slot(x) -> x in
      let epoch_seed = Aux.find_epoch_seed c epoch_start in
      if (!validity) && CardanoPoS.valid_proposer (CardanoBlock.minter blk) epoch_seed num_proposers [blk.contents.slot;nonce] blk.contents.credential then
        validity := true
      else
        validity := false
    in
    List.iter valid_block rev_chain;
    !validity

  let validate (node:node) (msg:message) : bool =
    match msg with
    | Block(blk,signature) -> 
      (
        let slot,(*prev_head*)_,leader = match signature with Signature(Slot(s),State(prev),node_id) -> s,prev,node_id in
        let valid_leader = CardanoPoS.valid_proposer leader node.data.epoch_seed num_proposers [slot;node.data.nonce] blk.contents.credential in
        let longer_chain = (CardanoBlock.height blk) > (CardanoBlock.height node.state) in
        blk.contents.slot = slot && valid_leader && (CardanoBlock.minter blk) = leader && longer_chain
      )
    | Chain(chain) -> valid_chain chain node.data.nonce
    | Req(_) -> true

end

module Propagation = struct
  type message = cardano_msg
  type node    = CardanoState.t

  let receive (_:node) (queue:message list ref) =
    let res = ref [] in
    List.iter (fun x -> res := !res@[x]) !queue;
    !res

  let propagate (node:node) (msg:message) =
    CardanoNetwork.gossip node.id msg
  
end

module Finalization = struct
  type message = cardano_msg
  type node    = CardanoState.t

  let max_valid (node:node) chain1 chain2 =
    let v1 = Validation.valid_chain chain1 node.data.nonce in
    let v2 = Validation.valid_chain chain2 node.data.nonce in
    if v1 && v2 then
    (
      if List.length chain1 > List.length chain2 then
        chain1
      else
        chain2
    )
    else if v2 then chain2
    else chain1

  let process (node:node) (msg:message) : node =
    match msg with
    | Block(blk,_) -> 
      (
        node.data.best_chain <- [blk]@node.data.best_chain;
        node.state <- blk;
        node
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
    | Req(sender) -> 
      CardanoNetwork.send node.id sender (Chain(node.data.best_chain)); node

end

module FCFCardanoNode = Implementation.Fcf.Make(CardanoEvent)(CardanoState)(Proposal)(Validation)(Propagation)(Finalization);;

module CardanoInitializer : (Protocol.Initializer with type node=FCFCardanoNode.t and type ev=FCFCardanoNode.ev) = struct
  type node = FCFCardanoNode.t

  type ev = FCFCardanoNode.ev

  let init nodes = 
    let evs = ref [] in
    Hashtbl.iter (fun nid _ -> evs := !evs @ [CardanoEvent.Timeout(nid, 0, "inc_slot")]) nodes;
    !evs
  
end

module CardanoProtocol = Protocol.Make.Blockchain(CardanoEvent)(CardanoQueue)(CardanoBlock)(CardanoTimer)(FCFCardanoNode)(FCFCardanoNode)(CardanoInitializer)(CardanoLogger)(Simulator.Statistics.Empty);;

