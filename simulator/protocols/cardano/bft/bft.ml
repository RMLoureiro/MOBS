let r = 3
let slot_duration = 3000
let num_proposers = 1

open Implementation

(* credentials not needed for this version of protocol, but we keep them to reuse as much code as possible from praos version *)
let null_cred = Abstractions.Pos.Credential(Node(-1),Block(-1),Selections(-1),Priority(-1),Params([]))

let check_proposer id slot =
   id = ((slot-1) mod !Parameters.General.num_nodes)

module Proposal = struct
  type message = Praos.cardano_msg
  type node    = Praos.CardanoState.t

  let create_proposal (node:node) : (message option) =
    let inc_slot =
      let slot = match node.data.slot with Slot(x) -> x in
      slot = node.state.contents.slot
    in
    if inc_slot then
    (
      Praos.Aux.next_slot node;
      Praos.CardanoTimer.set node.id slot_duration "inc_slot";
      let slot = match node.data.slot with Slot(x) -> x in
      if check_proposer node.id slot then
      (
        let new_blk = Praos.CardanoBlock.create node.id node.state {slot=slot;epoch=node.data.epoch;credential=null_cred} in
        let node_sig = Praos.Signature(node.data.slot, State(Praos.CardanoBlock.id node.state), node.id) in
        Some(Block(new_blk, node_sig))
      )
      else
        None
    )
    else
      None
  
end

module Validation = struct
  type message = Praos.cardano_msg
  type node    = Praos.CardanoState.t

  let valid_chain (c:Praos.CardanoState.value list) : bool =
    let rev_chain = List.rev c in
    let validity = ref true in
    let valid_block (blk:Praos.CardanoState.value) =
      if (!validity) && check_proposer (Praos.CardanoBlock.minter blk) blk.contents.slot then
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
        let valid_leader = check_proposer leader slot in
        let longer_chain = (Praos.CardanoBlock.height blk) > (Praos.CardanoBlock.height node.state) in
        blk.contents.slot = slot && valid_leader && (Praos.CardanoBlock.minter blk) = leader && longer_chain
      )
    | Chain(chain) -> valid_chain chain
    | Req(_) -> true

end

module FCFCardanoNode = Implementation.Fcf.Make(Praos.CardanoEvent)(Praos.CardanoState)(Proposal)(Validation)(Praos.Propagation)(Praos.Finalization);;

module CardanoProtocol = Protocol.Make.Blockchain(Praos.CardanoEvent)(Praos.CardanoQueue)(Praos.CardanoBlock)(Praos.CardanoTimer)(FCFCardanoNode)(FCFCardanoNode)(Praos.CardanoInitializer)(Praos.CardanoLogger)(Simulator.Statistics.Empty)(Praos.CardanoNetwork);;
