let committee_size  = Parameters.Protocol.get_int_parameter "committee-size";;
let majority_votes  = Parameters.Protocol.get_int_parameter "majority-votes";;
let round0_duration = Parameters.Protocol.get_int_parameter "round0-duration";;
let block_size      = Parameters.Protocol.get_int_parameter "block-size-mb";;

open Implementation

type block_contents = {
  (* Note: the level is included in the block's header, as HEIGHT *)
  round : int; (* the round when the block was created *)
  mutable predecessor_eqc : endorsement list; (* endorsement quorum certificate of previous block *)
  mutable previously_proposed : (int * preendorsement list) option; (* whether the block has been previously proposed *)
  mutable rtimestamp : int; (* the timestamp when the current round began *)
}
and endorsement    = Endorsement    of Simulator.Block.block_header * signature
and preendorsement = Preendorsement of Simulator.Block.block_header * signature
and signature      = Abstractions.Pos.credential

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
 | Propose of (block_contents Simulator.Block.t) list * signature
 | Preendorse of preendorsement
 | Endorse of (endorsement * preendorsement list)
 | Preendorsements of (block_contents Simulator.Block.t * preendorsement list)


module TenderbakeMessage : (Simulator.Events.Message with type t = tenderbake_msg) = struct 
  type t = tenderbake_msg

  let identifier (msg:t) =
    match msg.payload with
    | Propose([],_) -> -1
    | Propose(head::_,_) -> head.header.id
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

  let get_size (m:t) =
    match m.payload with
    | Propose(_) -> Simulator.Size.Megabyte(block_size)
    | Preendorsements(_,l) -> Simulator.Size.Bit(Simulator.Size.to_bits (Simulator.Size.Megabyte(block_size)) + Simulator.Size.to_bits (Simulator.Size.Byte(132 * (List.length l))))
    | _ -> Simulator.Size.Byte(132)
    (* TODO : get more accurate data *)
    
    

  let processing_time (_:t) =
    10

end

module PEQTime = struct
  let label = "average-preendorsement-quorum-time"
  let use_intervals = false
  let format = 1
end

module EQTime = struct
  let label = "average-endorsement-quorum-time"
  let use_intervals = false
  let format = 1
end

module BPTime = struct
  let label = "average-block-propagation-time"
  let use_intervals = false
  let format = 1
end

module AveragePEQTime = Simulator.Statistics.Make.Average(PEQTime);;
module AverageEQTime  = Simulator.Statistics.Make.Average(EQTime);;
module AverageBPTime  = Simulator.Statistics.Make.Average(BPTime);;

module TenderbakeEvent   = Simulator.Events.MakeEvent(TenderbakeMessage);;
module TenderbakeQueue   = Simulator.Events.MakeQueue(TenderbakeEvent);;
module TenderbakeTimer   = Abstractions.Timer.Make(TenderbakeEvent)(TenderbakeQueue);;
module TenderbakeNetwork = Abstractions.Network.Make(TenderbakeEvent)(TenderbakeQueue)(TenderbakeMessage);;
module TenderbakeLogger  = Simulator.Logging.Make(TenderbakeMessage)(TenderbakeEvent);;
module TenderbakeBlock   = Simulator.Block.Make(TenderbakeLogger)(BlockContents)(Simulator.Block.NoRewards);;
module TenderbakePoS     = Abstractions.Pos.Make(TenderbakeLogger)(TenderbakeBlock);;



module TenderbakeNode : (Protocol.BlockchainNode with type ev=TenderbakeEvent.t and type value=TenderbakeBlock.block) = struct

  type value = TenderbakeBlock.block

  module V = struct
    type v = value
  end

  include Protocol.MakeBaseNode(V)

  type ev = TenderbakeEvent.t

  type node_data = {
    mutable round              : int;    
    mutable chain              : value list;
    mutable proposal_state     : proposal_state;
    mutable endorsable         : (int * (block_contents Simulator.Block.t) * preendorsement list) option;
    mutable locked             : (int * (block_contents Simulator.Block.t) * preendorsement list) option;
    mutable msg_buffer         : tenderbake_msg list (* type, creator, level, round, block_id *);
    mutable proposals_received : tenderbake_msg list 
  }
  and proposal_state =
  | No_proposal
  | Collecting_preendorsements of { acc : preendorsement list }
  | Collecting_endorsements of { pqc : preendorsement list; acc : endorsement list; }

  type t = (node_data, value) Protocol.template

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
        round = 0;
        chain = [initial_block];
        proposal_state = No_proposal;
        endorsable = None;
        locked = None;
        msg_buffer = [];
        proposals_received = [];
      }
    }

    let send_to_neighbours (node:t) (msg:tenderbake_msg) =
      TenderbakeNetwork.gossip node.id msg

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
      if delay > 0 then (
        TenderbakeTimer.cancel node.id "wake"; (* cancel preexisting wakeups, if they exist *)
        TenderbakeTimer.set node.id delay "wake"
      )

    let qc_complete (l:'a list) =
      List.length l >= majority_votes

    let is_qc_valid (block:TenderbakeBlock.block) (decided:TenderbakeBlock.block) (qc:(Simulator.Block.block_header*signature) list) =
      let check_endorsement ((e:Simulator.Block.block_header),(cred:signature)) =
        let signature = match cred with Credential(Node(signature),Block(_),Selections(_),Priority(_),Params(_)) -> signature in
        let in_committee = TenderbakePoS.valid_committee_member signature decided committee_size [block.header.height] cred in
        (block.header.id = e.id && in_committee) || (block.header.id = 0) (* genesis block is always valid *)
      in
      List.for_all check_endorsement qc && qc_complete qc

    let is_eqc_valid block decided eqc =
      is_qc_valid block decided (List.map (fun (Endorsement(x,signature)) -> x,signature) eqc)

    let is_pqc_valid block decided pqc =
      is_qc_valid block decided (List.map (fun ((Preendorsement(x,signature))) -> x,signature) pqc)

    (* The level of the most recently decided block in the chain *)
    let decided_level (node:t) =
      match node.data.chain with _ :: block :: _ -> block.header.height | _ -> 0

    (* The most recently decided block in the chain *)
    let decided (node:t) =
      match node.data.chain with _ :: block :: _ -> Some block | _ -> None

    let decided_from_chain (chain:value list) =
      match chain with
      | [] -> assert false
      | blk::[] -> blk
      | _::rest ->
        (
          match rest with
          | blk::_ -> blk
          | _ -> assert false
        )

    (* The level of the block in the head of the chain (may not be the same as the decided) *)
    let head_level (chain:TenderbakeBlock.block list) =
      match chain with block :: _ -> block.header.height | _ -> 0

    let current_round (node:t) =
      node.data.round

    let last_decided_block_hash (node:t) =
      match node.data.chain with block :: _ -> block.header.parent | _ -> None (* TODO : not an hash *)

    let set_endorsable (node:t) round block pqc =
      match node.data.endorsable with
      | None -> ()
      | Some(old_round, _, _) -> 
        if round > old_round then node.data.endorsable <- Some(round, block, pqc)

    (* check if message if for a future level/round *)
    let future_message (msg:tenderbake_msg) (node:t) =
      let level_of_proposal = msg.level in
      let round_of_proposal = msg.round in
      let level = head_level node.data.chain in
      let round = current_round node in
      let is_proposal = match msg.payload with Propose(_) -> true | _ -> false in
      let can_process = 
        match node.data.proposal_state with
        | No_proposal -> is_proposal
        | _ -> true
      in
      level_of_proposal > level || (level_of_proposal = level && round_of_proposal > round) || (not can_process)

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
            let last_decided = decided_from_chain blocks in
            let is_eqc_correct =
              match eqc_opt with
              | None -> true
              | Some eqc -> is_eqc_valid block last_decided eqc
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
              go (Some block.contents.predecessor_eqc) (Some block.header.parent) rest
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
        let previous_block_hash = block.header.parent in
        let msg = {
          level = block.header.height;
          round = current_round node;
          previous_block_hash;
          payload = Preendorsements(block, pqc);
          creator = node.id;
        } in
        send_to_neighbours node msg

    let handle_endorsement (node:t) (Endorsement (block_header,signature) as endorsement) =     
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
              node.data.proposal_state <- Collecting_endorsements{m with acc=new_acc};
              if List.length new_acc == majority_votes then (AverageEQTime.process node.id ((Simulator.Clock.get_timestamp ()) - (TenderbakeBlock.timestamp block)))
            end
        | _ -> ()

    let send_endorse (node:t) (pqc:preendorsement list) =
      match node.data.chain with
      | [] -> ()
      | block :: _ -> 
        match TenderbakePoS.in_committee node.id node.state committee_size [head_level node.data.chain] with
        | Some(cred) ->
          begin
            let previous_block_hash = block.header.parent in
            let endorsement = Endorsement(block.header, cred) in
            let msg = {
              level = block.header.height;
              round = node.data.round;
              previous_block_hash;
              creator = node.id;
              payload = Endorse(endorsement, pqc);
            } in
            handle_endorsement node endorsement;
            send_to_neighbours node msg
          end
        | None -> ()

    let handle_preendorsement (node:t) (Preendorsement(block_header,signature) as preendorsement) =
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
                AveragePEQTime.process node.id ((Simulator.Clock.get_timestamp ()) - (TenderbakeBlock.timestamp block));
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
        match TenderbakePoS.in_committee node.id node.state committee_size [head_level node.data.chain] with
        | Some(cred) ->
          begin
            let previous_block_hash = block.header.parent in
            let preendorsement = Preendorsement(block.header, cred) in
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
            handle_preendorsement node preendorsement;
            send_to_neighbours node msg
          end
        | None -> ()

    let rcv_propose (candidate_chain:(block_contents Simulator.Block.t) list) (cred:signature) (creator:int) (node:t) =
      let last_decided = decided_from_chain candidate_chain in
      let candidate = List.nth candidate_chain 0 in
      let valid_proposer = TenderbakePoS.valid_proposer creator last_decided 1 [head_level node.data.chain] cred in
      let valid_previously_proposed_pqc = 
        match candidate.contents.previously_proposed with
        | None -> true
        | Some(_, pqc) -> is_pqc_valid candidate last_decided pqc
      in
      if valid_proposer && valid_previously_proposed_pqc && valid_chain candidate_chain && better_chain candidate_chain node
      then
        begin
          schedule_wakeup node candidate.contents.rtimestamp node.data.round;
          node.data.chain <- candidate_chain;
          node.data.proposal_state <- No_proposal;
          if head_level candidate_chain > head_level node.data.chain then
            begin
              node.data.endorsable <- None;
              node.data.locked <- None;
            end;
          match node.data.locked with
          | None -> (
            AverageBPTime.process node.id ((Simulator.Clock.get_timestamp ()) - (TenderbakeBlock.timestamp candidate));
            node.data.proposal_state <- Collecting_preendorsements({acc=[]});
            send_preendorse node
            )
          | Some(locked_round, locked_block, pqc) -> 
            begin
              if TenderbakeBlock.equals locked_block candidate then
                (
                  AverageBPTime.process node.id ((Simulator.Clock.get_timestamp ()) - (TenderbakeBlock.timestamp candidate));
                  node.data.proposal_state <- Collecting_preendorsements({acc=[]});
                  send_preendorse node
                )
              else
                match candidate.contents.previously_proposed with
                | None -> send_preendorsements node pqc
                | Some(endorsable_round, _) ->
                  begin
                    if locked_round < endorsable_round then
                      (
                        AverageBPTime.process node.id ((Simulator.Clock.get_timestamp ()) - (TenderbakeBlock.timestamp candidate));
                        node.data.proposal_state <- Collecting_preendorsements({acc=[]});
                        send_preendorse node
                      )
                    else
                      send_preendorsements node pqc
                  end
            end
        end
      else
        ()

    let rcv_preendorse (Preendorsement(_,cred) as preendorsement) (node:t) =
      let signature = match cred with Credential(Node(signature),Block(_),Selections(_),Priority(_),Params(_)) -> signature in
      let valid_preendorser = TenderbakePoS.valid_committee_member signature node.state committee_size [head_level node.data.chain] cred in
      match node.data.proposal_state with
      | Collecting_preendorsements(_) -> 
        if valid_preendorser then
          handle_preendorsement node preendorsement
      | _ -> ()

    let rcv_endorse (Endorsement(_, cred) as endorsement) (pqc:preendorsement list) (node:t) =
      let signature = match cred with Credential(Node(signature),Block(_),Selections(_),Priority(_),Params(_)) -> signature in
      let valid_endorser = TenderbakePoS.valid_committee_member signature node.state committee_size [head_level node.data.chain] cred in
      match (node.data.proposal_state, node.data.chain) with
      | Collecting_endorsements _, block::_ -> 
        let parent_id = match (TenderbakeBlock.parent block) with Some(id) -> id | None -> -1 in
        let correct_decided = (TenderbakeBlock.id (decided_from_chain node.data.chain)) = parent_id || parent_id = -1 in
        let valid_pqc = is_pqc_valid block (decided_from_chain node.data.chain) pqc in
        if valid_endorser && valid_pqc && correct_decided then
          (
            handle_endorsement node endorsement
          )
      | Collecting_preendorsements _, block::_ ->
        let parent_id = match (TenderbakeBlock.parent block) with Some(id) -> id | None -> -1 in
        let correct_decided = (TenderbakeBlock.id (decided_from_chain node.data.chain)) = parent_id || parent_id = -1 in
        let valid_pqc = is_pqc_valid block (decided_from_chain node.data.chain) pqc in
        if valid_endorser && valid_pqc && correct_decided then
          begin
            (* catchup mechanism *)
            node.data.proposal_state <- Collecting_endorsements{pqc; acc=[]};
            set_endorsable node (current_round node) block pqc;
            node.data.locked <- Some(current_round node, block, pqc);
            send_endorse node pqc;
            handle_endorsement node endorsement
          end
      | _ -> ()

    let rcv_preendorsements (block:block_contents Simulator.Block.t) (pqc:preendorsement list) (round:int) (node:t) =
      let decided = decided_from_chain node.data.chain in
      let valid_pqc = is_pqc_valid block decided pqc in
      if valid_pqc then
        set_endorsable node round block pqc

    let process_msg (node:t) (message:tenderbake_msg) =
      (* TODO : signature check? *)
      let creator = message.creator in
      let message_valid = check_message_lrh message node in
      let future = future_message message node in
      if future then
        (
          node.data.msg_buffer <- node.data.msg_buffer@[message]
        )
      else
      if message_valid then
        (
          match message.payload with
            | Propose(chain,cred) -> rcv_propose chain cred creator node
            | Preendorse(preendorsement) -> rcv_preendorse preendorsement node
            | Endorse(endorsement, pqc) -> rcv_endorse endorsement pqc node
            | Preendorsements(blk,pqc) -> rcv_preendorsements blk pqc message.round node
        )

    let process_msg_buffer (node:t) =
      let futures  = ref [] in
      let currents = ref [] in
      let filter (message:tenderbake_msg) =
        let valid  = check_message_lrh message node in
        let future = future_message message node in
        if future then futures := !futures@[message]
        else
          begin
            if valid then
              currents := !currents@[message]
          end
      in
      List.iter filter node.data.msg_buffer;
      List.iter (fun msg -> process_msg node msg) !currents;
      node.data.msg_buffer <- !futures

    let send_proposal (node:t) (credential) =
      let proposed_level = (decided_level node) + 1 in
      let previous_block_hash = last_decided_block_hash node in
      let msg = {
          level = proposed_level;
          round = current_round node;
          previous_block_hash;
          creator = node.id;
          payload = Propose(node.data.chain,credential);
      } in
      node.data.proposal_state <- Collecting_preendorsements {acc = [] };
      send_to_neighbours node msg;
      send_preendorse node

    let increment_round (node:t) =
      match node.data.chain with
      | [] -> assert false (* not possible, since nodes start with genesis block in their chain *)
      | old_block::rest ->
        let new_block =
          match node.data.endorsable with
          | None -> 
            {old_block with contents = {old_block.contents with previously_proposed = None; rtimestamp = Simulator.Clock.get_timestamp ()}}
          | Some(round, block, pqc) -> 
            {block with contents= {old_block.contents with previously_proposed = Some(round, pqc); rtimestamp = Simulator.Clock.get_timestamp ()}}
        in
        node.data.chain <- new_block::rest;
        node.data.proposal_state <- No_proposal;
        node.data.round <- node.data.round+1;
        schedule_wakeup node new_block.contents.rtimestamp (current_round node);
        process_msg_buffer node

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
                TenderbakeBlock.create node.id block block_data
              in
              let new_chain = new_block::node.data.chain in
              node.data.chain <- new_chain;
              node.data.proposal_state <- No_proposal;
              node.data.endorsable <- None;
              node.data.locked <- None;
              node.data.round <- 0;
              schedule_wakeup node new_block.contents.rtimestamp 0;
              process_msg_buffer node
            end
          else
            increment_round node

    let process_wake (node:t) =
      match node.data.chain with
      | [] -> assert false (* not possible, since all nodes start with the genesis block in their chain *)
      | _ :: _ ->
        attempt_to_decide_head node;
        let blk = decided_from_chain node.data.chain in
        match TenderbakePoS.is_proposer node.id blk 1 [head_level node.data.chain] with
        | Some(cred) -> send_proposal node cred
        | None -> ()


    (*
    let debug (node:t) (ev:TenderbakeEvent.t) =
      let kind msg = match msg.payload with 
        | Propose(_) -> "PROPOSE"
        | Preendorse(_) -> "PREENDORSE"
        | Endorse(_) -> "ENDORSE"
        | Preendorsements(_) -> "PREENDORSEMENTS"
      in
      let ev = match ev with
      | TenderbakeEvent.Message(_,_,_,msg) -> kind msg
      | TenderbakeEvent.Timeout(_,_,label) -> label
      | _ -> ""
      in
      let ts = Simulator.Clock.get_timestamp () in
      let state = match node.data.proposal_state with
      | No_proposal -> "No Proposal"
      | Collecting_preendorsements(m) -> Printf.sprintf "Collecting Preendorsements with Count=%d" (List.length m.acc)
      | Collecting_endorsements(m) -> Printf.sprintf "Collecting Endorsements (%d preendorsements received) with Count=%d" (List.length m.pqc) (List.length m.acc)
      in
      let s = 
        Printf.sprintf "Node %d | %d | %s | Level %d | State { %s }" node.id ts ev (head_level node.data.chain) state
      in
      print_endline s
      *)

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
      process_msg_buffer node;
      node.state <- updated_decided_state; node

end


module TenderbakeInitializer : (Protocol.Initializer with type node=TenderbakeNode.t and type ev=TenderbakeEvent.t) = struct
  type node = TenderbakeNode.t

  type ev = TenderbakeEvent.t

  let init nodes = 
    let evs = ref [] in
    Hashtbl.iter (fun nid _ -> evs := !evs @ [TenderbakeEvent.Timeout(nid, 0, "wake")]) nodes;
    !evs
  
end


module TenderbakeStatistics = Simulator.Statistics.Compose(AverageBPTime)(Simulator.Statistics.Compose(AveragePEQTime)(AverageEQTime));;


module TenderbakeProtocol = Protocol.Make.Blockchain(TenderbakeEvent)(TenderbakeQueue)(TenderbakeBlock)(TenderbakeTimer)(TenderbakeNode)(TenderbakeNode)(TenderbakeInitializer)(TenderbakeLogger)(TenderbakeStatistics)(TenderbakeNetwork);;

