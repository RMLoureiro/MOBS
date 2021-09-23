(** 
  Main references for this implementation:
  -> (1) https://blog.nomadic-labs.com/a-look-ahead-to-tenderbake.html
  -> (2) https://arxiv.org/pdf/2001.11965.pdf
  -> (3) https://gitlab.com/nomadic-labs/tenderbake-simulator/-/tree/master/src/tenderbake

  At an high level, this is an adaptation of (3) for this simulator, 
    as the overall logic is extremely similar. The main goal was to show the
    added functionalities that this simulator offers, and not to fundamentaly
    change an already correct and validated implementation of the protocol.

  Main differences between this implementation and the one referenced above:
  -> since we aren't considering malicious nodes:
     -> The hash of a block is replaced by the internal ID that every block in the simulator possesses
     -> The signature of a node is replaced by the internal ID that every node in the simulator possesses
  -> proposer and committee members are selected through sortition based on the nodes' stakes
  -> the network is not fully linked (although it can be)
     -> each node sends messages to its neighbours
     -> each node forwards incoming messages to its neighbours
  -> mutable records are leveraged to store and update the nodes' state
  -> can easily extend the statistics module to gather arbitrary statistics about the execution of the protocol
     -> at the moment, the only one being extracted is the average time to decide on a block
  -> can leverage the visualizer to run batch simulations, to analyze the gathered statistics and to monitor
     the overall behavior of the protocol
*)

let committee_size  = Parameters.Protocol.get_int_parameter "committee-size";;
let majority_votes  = Parameters.Protocol.get_int_parameter "majority-votes";;
let round0_duration = 3000;

open Implementation

type block_contents = {
  (* Note: the level is included in the block's header, as HEIGHT *)
  mutable round : int; (* the round of the block *)
  mutable predecessor_eqc : endorsement list; (* endorsement quorum certificate of previous block *)
  mutable previously_proposed : (int * preendorsement list) option; (* whether the block has been previously proposed *)
  mutable rtimestamp : int; (* the timestamp when the current round began *)
}
and endorsement    = Endorsement    of Simulator.Block.block_header * signature
and preendorsement = Preendorsement of Simulator.Block.block_header * signature
and signature      = Signature of int

module BlockContents = struct
  type t = block_contents
end

type tenderbake_msg = unsigned_message (* TODO : can replace creator with a signature *)
and unsigned_message = {
  level : int;
  round : int;
  previous_block_hash : Simulator.Block.block_id option; (* TODO : not really an hash *)
  creator : int;
  payload : payload;
}
and payload = 
 | Propose of (block_contents Simulator.Block.t) list
 | Preendorse of preendorsement
 | Endorse of (endorsement * preendorsement list)
 | Preendorsements of (block_contents Simulator.Block.t * preendorsement list)


module TenderbakeMessage : (Simulator.Events.Message with type t = tenderbake_msg) = struct 
  type t = tenderbake_msg

  let identifier (msg:t) =
    match msg.payload with
    | Propose([]) -> -1
    | Propose(head::_) -> head.header.id
    | Preendorse(Preendorsement(header,_)) -> header.id
    | Endorse(Endorsement(header,_),_) -> header.id
    | Preendorsements(block,_) -> block.header.id

  let to_json (msg:t) : string =
    let level = msg.level in
    let round = msg.round in
    let creator = msg.creator in
    let prev_block = 
      match msg.previous_block_hash with
      | None -> -1
      | Some(blk) -> blk
    in
    let id = identifier msg in
    let kind = match msg.payload with 
      | Propose(_) -> "PROPOSE"
      | Preendorse(_) -> "PREENDORSE"
      | Endorse(_) -> "ENDORSE"
      | Preendorsements(_) -> "PREENDORSEMENTS"
    in
    let payload = Printf.sprintf "{\"kind\":\"%s\",\"block\":%d}" kind id in
    Printf.sprintf "{\"level\":%d,\"round\":%d,\"creator\":%d,\"prev_block\":%d,\"payload\":%s}" level round creator prev_block payload

  let get_size (msg:t) =
    match msg with
    | _ -> Simulator.Size.Bit(10000) (* TODO *)

  let processing_time (_:t) =
    10

end



module TenderbakeEvent   = Simulator.Events.MakeEvent(TenderbakeMessage);;
module TenderbakeQueue   = Simulator.Events.MakeQueue(TenderbakeEvent);;
module TenderbakeTimer   = Abstractions.Timer.Make(TenderbakeEvent)(TenderbakeQueue);;
module TenderbakeNetwork = Abstractions.Network.Make(TenderbakeEvent)(TenderbakeQueue)(TenderbakeMessage);;
module TenderbakeLogger  = Simulator.Logging.Make(TenderbakeMessage)(TenderbakeEvent);;
module TenderbakeBlock   = Simulator.Block.Make(TenderbakeLogger)(BlockContents);;
module TenderbakePoS     = Abstractions.Pos.Make(TenderbakeLogger)(TenderbakeBlock);;



module TenderbakeNode : (Protocol.Node with type ev=TenderbakeEvent.t and type value=TenderbakeBlock.block) = struct

  type value = TenderbakeBlock.block

  module V = struct
    type v = value
  end

  include Abstract.MakeBaseNode(V)

  type ev = TenderbakeEvent.t

  type node_data = {
    (*private_key:Signature.private_key;*)
    mutable chain          : value list;
    mutable proposal_state : proposal_state;
    mutable endorsable     : (int * (block_contents Simulator.Block.t) * preendorsement list) option;
    mutable locked         : (int * (block_contents Simulator.Block.t) * preendorsement list) option;
    mutable seen_msgs      : (int * int * int * int * int) list (* type, creator, level, round, block_id *)
  }
  and proposal_state =
  | No_proposal
  | Collecting_preendorsements of { acc : preendorsement list }
  | Collecting_endorsements of { pqc : preendorsement list; acc : endorsement list; }

  type t = (node_data, value) Abstract.template

  let init id links region : (t) =
    let initial_block =
      let initial_block_contents = 
        {
          round = 0;
          predecessor_eqc = [];
          previously_proposed = None;
          rtimestamp = 0;
        } 
      in
      TenderbakeBlock.genesis_pos 1 initial_block_contents
    in
    {
      id = id;
      region = region;
      links = links;
      state = initial_block;
      data = {
        chain = [initial_block];
        proposal_state = No_proposal;
        endorsable = None;
        locked = None;
        seen_msgs = [];
      }
    }

    let send_to_neighbours (node:t) (msg:tenderbake_msg) =
      Array.iter (
        fun neighbour -> 
          if not (neighbour = node.id) && not (neighbour = msg.creator) then 
            TenderbakeNetwork.send node.id neighbour msg
        ) node.links

        (* type, creator, level, round, block_id *)
    let check_duplicate (node:t) (msg:tenderbake_msg) =
      let round = msg.round in
      let level = msg.level in
      let creator = msg.creator in
      let tp,blk_id = 
        match msg.payload with
        | Propose([]) -> 1,-1
        | Propose(blk::_) -> 1,blk.header.id
        | Preendorse(Preendorsement(blk,_)) -> 2,blk.id
        | Endorse(Endorsement(blk,_),_) -> 3,blk.id
        | Preendorsements(blk,_) -> 4,blk.header.id
      in
      let v = (tp, creator, level, round, blk_id) in
      if List.exists (fun x -> x=v) node.data.seen_msgs then
        true
      else 
        begin
          node.data.seen_msgs <- [v]@node.data.seen_msgs;
          false
        end

    let chain_height (node:t) =
      node.state.header.height

    let round_duration round =
      match round with
      | 0 -> round0_duration
      | 1 -> round0_duration * 2
      | _ -> round0_duration * 4

    let schedule_wakeup (node:t) (round_start:int) (round_num:int) =
      let target_timestmap = round_start + (round_duration round_num) in
      let delay = target_timestmap - (Simulator.Clock.get_timestamp ()) in
      TenderbakeTimer.cancel node.id "wake"; (* cancel preexisting wakeups, if they exist *)
      TenderbakeTimer.set node.id delay "wake"

    let qc_complete (l:'a list) =
      List.length l >= majority_votes

    let is_qc_valid (block:TenderbakeBlock.block) (qc:(Simulator.Block.block_header*signature) list) =
      let check_endorsement ((e:Simulator.Block.block_header),(Signature(signature):signature)) =
        let in_committee = TenderbakePoS.check_committee signature block committee_size [block.header.height] in
        block.header.id = e.id && in_committee
      in
      List.for_all check_endorsement qc && qc_complete qc

    let is_eqc_valid block eqc =
      is_qc_valid block (List.map (fun (Endorsement(x,signature)) -> x,signature) eqc)

    let is_pqc_valid block pqc =
      is_qc_valid block (List.map (fun ((Preendorsement(x,signature))) -> x,signature) pqc)

    (* The level of the most recently decided block in the chain *)
    let decided_level (node:t) =
      match node.data.chain with _ :: block :: _ -> block.header.height | _ -> 0

    (* The most recently decided block in the chain *)
    let decided (node:t) =
      match node.data.chain with _ :: block :: _ -> Some block | _ -> None

    (* The level of the block in the head of the chain (may not be the same as the decided) *)
    let head_level (chain:TenderbakeBlock.block list) =
      match chain with block :: _ -> block.header.height | _ -> 0

    let current_round (node:t) =
      match node.data.chain with block :: _ -> block.contents.round | _ -> 0

    let last_decided_block_hash (node:t) =
      match node.data.chain with block :: _ -> block.header.parent | _ -> None (* TODO : not an hash *)

    let set_endorsable (node:t) round block pqc =
      match node.data.endorsable with
      | None -> ()
      | Some(old_round, _, _) -> 
        if round > old_round then node.data.endorsable <- Some(round, block, pqc)

    (* Check if message has corrent data *)
    let check_message_lrh (msg:tenderbake_msg) (node:t) =
      let level_of_proposal = msg.level in
      let round_of_proposal = msg.round in
      let level_is_right = Int.equal (head_level node.data.chain) level_of_proposal in
      let round_is_right =
        Int.equal (current_round node) round_of_proposal
      in
      let last_decided_block_hash_is_right =
        match
          (last_decided_block_hash node, msg.previous_block_hash)
        with
        | None, None -> true
        | Some hash0, Some hash1 -> hash0 = hash1
        | _ -> false
      in
      level_is_right && round_is_right && last_decided_block_hash_is_right

    let valid_chain (chain:TenderbakeBlock.block list) =
      let rec go eqc_opt hash_opt (blocks : TenderbakeBlock.block list) =
        match blocks with
        | [] -> true
        | block :: rest ->
            let is_eqc_correct =
              match eqc_opt with
              | None -> true
              | Some eqc -> is_eqc_valid block eqc
            in
            let is_hash_correct =
              match hash_opt with
              | None ->
                  (* [block] is the head, so there is no block that includes its
                      hash yet, nothing to check. *)
                  true
              | Some None ->
                  (* [predecessor_hash] is [None], this is only acceptible for
                      the first block. *)
                  Int.equal block.header.height 1
              | Some (Some hash) ->
                  (* Otherwise the [predecessor_hash] from the newer block must
                      match the hash of the previous block. *)
                  block.header.id = hash
            in
            let is_rest_correct =
              go (Some block.contents.predecessor_eqc) (Some block.header.parent)
                rest
            in
            is_eqc_correct && is_hash_correct && is_rest_correct
      in
      go None None chain

    let better_chain (candidate:TenderbakeBlock.block list) (node:t) =
      let candidate_level = head_level candidate in
      let current_level = head_level node.data.chain in
      if candidate_level == current_level then
        match (candidate, node.data.chain) with
        | candidate_head :: candidate_rest, _ :: node_rest -> (
            let node_predecessor_round_is_higher =
              match (candidate_rest, node_rest) with
              | candidate_predecessor :: _, node_predecessor :: _ ->
                  node_predecessor.contents.round >= candidate_predecessor.contents.round
              | [], [] -> true
              | _ -> false
            in
            match (candidate_head.contents.previously_proposed, node.data.endorsable) with
            | None, None -> node_predecessor_round_is_higher
            | ( Some (candidate_endorsable_round, _),
                Some (node_endorsable_round, _, _) ) ->
                if candidate_endorsable_round == node_endorsable_round then
                  node_predecessor_round_is_higher
                else candidate_endorsable_round > node_endorsable_round
            | Some _, None -> true
            | None, Some _ -> false)
        | _ -> false
      else candidate_level > current_level

    let send_preendorsements (node:t) (pqc:preendorsement list) =
      match node.data.chain with
      | [] -> ()
      | block :: _ -> 
        let previous_block_hash = block.header.parent in (* TODO : not an HASH *)
        let msg = {
          level = block.header.height;
          round = current_round node;
          previous_block_hash;
          payload = Preendorsements(block, pqc);
          creator = node.id;
        } in
        send_to_neighbours node msg

    let handle_endorsement (node:t) (Endorse(endorsement, _)) =      
      let (block_header, signature) = match endorsement with Endorsement(bh, s) -> bh,s in
      match node.data.chain with 
      | [] -> ()
      | block :: _ -> 
        let add_to_acc e acc =
          e::(List.filter (fun (Endorsement(_,ee_signature)) -> not (ee_signature == signature)) acc)
        in
        match node.data.proposal_state with
        | Collecting_endorsements m ->
          if (block_header.id = block.header.id) && not (qc_complete m.acc) then
            begin
              let new_acc = add_to_acc endorsement m.acc in
              node.data.proposal_state <- Collecting_endorsements{m with acc=new_acc}
            end
        | _ -> ()

    let send_endorse (node:t) (pqc:preendorsement list) =
      match node.data.chain with
      | [] -> ()
      | block :: _ -> 
        let previous_block_hash = block.header.parent in
        let endorsement = Endorsement(block.header, Signature(node.id)) in
        let msg = {
          level = block.header.height;
          round = block.contents.round;
          previous_block_hash;
          creator = node.id;
          payload = Endorse(endorsement, pqc);
        } in
        handle_endorsement node (Endorse(endorsement, pqc));
        send_to_neighbours node msg

    let handle_preendorsement (node:t) (Preendorse(preendorsement)) =
      let (block_header, signature) = match preendorsement with
        Preendorsement(block_header, signature) -> block_header, signature
      in
      match node.data.chain with
      | [] -> ()
      | block :: _ -> 
        let add_to_acc p acc =
          p::(List.filter (fun (Preendorsement(_,pp_signature)) -> not (pp_signature == signature)) acc)
        in
        let check_pqc (n:t) =
          match n.data.proposal_state with
          | No_proposal -> ()
          | Collecting_preendorsements m ->
            if qc_complete m.acc then
              begin
                n.data.proposal_state <- Collecting_endorsements{pqc = m.acc; acc=[]};
                set_endorsable n (current_round n) block m.acc;
                n.data.locked <- Some(current_round n, block, m.acc);
                send_endorse n m.acc
              end
          | Collecting_endorsements _ -> ()
        in
        let _ = match node.data.proposal_state with
          | No_proposal -> 
            node.data.proposal_state <- Collecting_preendorsements{acc=[preendorsement]};
            check_pqc node
          | Collecting_preendorsements m -> 
            begin
              let preendorsed_block_hash = block_header.id in
              if preendorsed_block_hash == block.header.id then
                begin
                  let new_acc = add_to_acc preendorsement m.acc in
                  node.data.proposal_state <- Collecting_preendorsements{acc=new_acc};
                  check_pqc node
                end
            end
          | Collecting_endorsements _ -> ()
        in
        ()

    let send_preendorse (node:t) =
      match node.data.chain with
      | [] -> ()
      | block :: _ -> 
        let previous_block_hash = block.header.parent in
        let preendorsement = Preendorsement(block.header, Signature(node.id)) in
        let msg = {
          level = block.header.height;
          round = current_round node;
          previous_block_hash;
          payload = Preendorse(preendorsement);
          creator = node.id;
        } in
        let _ = 
          match block.contents.previously_proposed with
          | None -> ()
          | Some(round, pqc) -> set_endorsable node round block pqc
        in
        handle_preendorsement node (Preendorse(preendorsement));
        send_to_neighbours node msg

    let rcv_propose (Propose(candidate_chain)) (creator:int) (node:t) =
      let candidate = List.nth candidate_chain 0 in
      let valid_proposer = TenderbakePoS.check_proposer creator candidate 1 [head_level node.data.chain;current_round node] in
      let valid_previously_proposed_pqc = 
        match candidate.contents.previously_proposed with
        | None -> true
        | Some(_, pqc) -> is_pqc_valid candidate pqc
      in
      if valid_proposer && valid_previously_proposed_pqc && valid_chain candidate_chain && better_chain candidate_chain node
      then
        begin
          schedule_wakeup node candidate.contents.rtimestamp candidate.contents.round;
          node.data.chain <- candidate_chain;
          node.data.proposal_state <- No_proposal;
          if head_level candidate_chain > head_level node.data.chain then
            begin
              node.data.endorsable <- None;
              node.data.locked <- None;
            end;
          match node.data.locked with
          | None -> send_preendorse node
          | Some(locked_round, locked_block, pqc) -> 
            begin
              if TenderbakeBlock.equals locked_block candidate then
                send_preendorse node
              else
                match candidate.contents.previously_proposed with
                | None -> send_preendorsements node pqc
                | Some(endorsable_round, _) ->
                  begin
                    if locked_round < endorsable_round then
                      send_preendorse node
                    else
                      send_preendorsements node pqc
                  end
            end
        end
      else
        ()

        (* TODO : make a function to extract signature, to factor this code *)
    let rcv_preendorse (Preendorse(preendorsement)) (node:t) =
      let signature = match preendorsement with Preendorsement(_,Signature(signature)) -> signature in
      let valid_preendorser = TenderbakePoS.check_committee signature node.state committee_size [head_level node.data.chain] in
      match node.data.proposal_state with
      | Collecting_preendorsements(_) -> 
        if valid_preendorser then
          handle_preendorsement node (Preendorse(preendorsement))
      | _ -> ()

    let rcv_endorse (Endorse(endorsement, pqc)) (node:t) =
      let signature = match endorsement with Endorsement(_,Signature(signature)) -> signature in
      let valid_endorser = TenderbakePoS.check_committee signature node.state committee_size [head_level node.data.chain] in
      match (node.data.proposal_state, node.data.chain) with
      | Collecting_endorsements _, block::_ -> 
        let valid_pqc = is_pqc_valid block pqc in
        if valid_endorser && valid_pqc then
          handle_endorsement node (Endorse(endorsement, pqc))
      | Collecting_preendorsements _, block::_ ->
        let valid_pqc = is_pqc_valid block pqc in
        if valid_endorser && valid_pqc then
          begin
            (* catchup mechanism *)
            node.data.proposal_state <- Collecting_endorsements{pqc; acc=[]};
            set_endorsable node (current_round node) block pqc;
            node.data.locked <- Some(current_round node, block, pqc);
            send_endorse node pqc;
            handle_endorsement node (Endorse(endorsement, pqc))
          end
      | _ -> ()

    let rcv_preendorsements (Preendorsements(block, pqc)) (round:int) (node:t) =
      let valid_pqc = is_pqc_valid block pqc in
      if valid_pqc then
        set_endorsable node round block pqc
    
    let process_msg (node:t) (message:tenderbake_msg) =
      (* TODO : signature check? *)
      let creator = message.creator in
      let message_valid = check_message_lrh message node in
      if message_valid then
        begin
          let _ = match message.payload with
            | Propose(_) -> rcv_propose message.payload creator node
            | Preendorse(_) -> rcv_preendorse message.payload node
            | Endorse(_) -> rcv_endorse message.payload node
            | Preendorsements(_) -> rcv_preendorsements message.payload message.round node
          in
          if not (check_duplicate node message) then send_to_neighbours node message (* propagate non-duplicate message *)
        end

    let send_proposal (node:t) =
      let proposed_level = (decided_level node) + 1 in
      let previous_block_hash = last_decided_block_hash node in
      let msg = {
          level = proposed_level;
          round = current_round node;
          previous_block_hash;
          creator = node.id;
          payload = Propose node.data.chain;
      } in
      node.data.proposal_state <- Collecting_preendorsements {acc = [] };
      send_preendorse node;
      send_to_neighbours node msg

    let increment_round (node:t) =
      match node.data.chain with
      | [] -> assert false (* not possible, since nodes start with genesis block in their chain *)
      | old_block::rest ->
        let new_block =
          match node.data.endorsable with
          | None -> 
            let data = {
              round = old_block.contents.round+1;
              rtimestamp = Simulator.Clock.get_timestamp ();
              predecessor_eqc = old_block.contents.predecessor_eqc;
              previously_proposed = None;
            } in
            {old_block with contents=data}
          | Some(round, block, pqc) -> 
            let data = {
              round = block.contents.round+1;
              rtimestamp = Simulator.Clock.get_timestamp ();
              predecessor_eqc = block.contents.predecessor_eqc;
              previously_proposed = Some(round, pqc);
            } in
            {block with contents=data}
        in
        node.data.chain <- new_block::rest;
        node.data.proposal_state <- No_proposal;
        schedule_wakeup node new_block.contents.rtimestamp (current_round node)

    let attempt_to_decide_head (node:t) =
      match node.data.chain with
      | [] -> assert false (* not possible, since all nodes start with the genesis block in their chain *)
      | block::_ -> 
        match node.data.proposal_state with
        | No_proposal | Collecting_preendorsements _ ->
          increment_round node
        | Collecting_endorsements m ->
          if qc_complete m.acc then
            begin
              let block_data = {
                round=0;
                predecessor_eqc=m.acc;
                previously_proposed=None;
                rtimestamp=Simulator.Clock.get_timestamp ()
              } in
              let new_block = 
                TenderbakeBlock.create ~reward:false node.id block block_data
              in
              let new_chain = new_block::node.data.chain in
              node.data.chain <- new_chain;
              node.data.proposal_state <- No_proposal;
              node.data.endorsable <- None;
              node.data.locked <- None;
              schedule_wakeup node new_block.contents.rtimestamp 0
            end
          else
            increment_round node

    let process_wake (node:t) =
      match node.data.chain with
      | [] -> assert false (* not possible, since all nodes start with the genesis block in their chain *)
      | block :: _ ->
        attempt_to_decide_head node;
        if TenderbakePoS.is_proposer node.id block 1 [head_level node.data.chain] then
            send_proposal node

    let handle (node:t) (event:TenderbakeEvent.t) =
      let _ = match event with
        | TenderbakeEvent.Message(_,_,_,msg) -> process_msg node msg
        | TenderbakeEvent.Timeout(_,_,label) ->
          begin
            match label with
            | "wake" -> process_wake node
            | _ -> ()
          end
        | _ -> ()
      in
      let updated_decided_state =
        match decided node with
        | Some(blk) -> blk
        | None -> node.state
      in
      node.state <- updated_decided_state; node

end



module TenderbakeInitializer : (Abstract.Initializer with type node=TenderbakeNode.t and type ev=TenderbakeEvent.t) = struct
  type node = TenderbakeNode.t

  type ev = TenderbakeEvent.t

  let init nodes = 
    let evs = ref [] in
    Hashtbl.iter (fun nid _ -> evs := !evs @ [TenderbakeEvent.Timeout(nid, 0, "wake")]) nodes;
    !evs
  
end



module TenderbakeStatistics : (Protocol.Statistics with type ev = TenderbakeEvent.t and type value = TenderbakeBlock.block) = struct
  type ev = TenderbakeEvent.t
  type value = TenderbakeBlock.block

  let consensus_reached _ _ =
    ()

  let process _ =
    ()

  let get () =
    "{}"

end



module TenderbakeProtocol = Protocol.Make(TenderbakeEvent)(TenderbakeQueue)(TenderbakeBlock)(TenderbakeTimer)(TenderbakeNode)(TenderbakeInitializer)(TenderbakeLogger)(TenderbakeStatistics);;



