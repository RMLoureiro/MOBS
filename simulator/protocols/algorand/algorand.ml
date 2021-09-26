(* load protocol specific parameters *)
let lambda_step           = Parameters.Protocol.get_int_parameter "lambda-step";;
let lambda_stepvar        = Parameters.Protocol.get_int_parameter "lambda-stepvar";;
let lambda_priority       = Parameters.Protocol.get_int_parameter "lambda-priority";;
let lambda_block          = Parameters.Protocol.get_int_parameter "lambda-block";;
let committee_size        = Parameters.Protocol.get_int_parameter "committee-size";;
let num_proposers         = Parameters.Protocol.get_int_parameter "num-proposers";;
let majority_votes        = Parameters.Protocol.get_int_parameter "majority-votes";;
let block_size            = Parameters.Protocol.get_int_parameter "block-size-mb";;

open Implementation

type alg_msg = 
    Priority of int * int * int * int * signature  (* round, period, step, priority, creator_signature *)
  | Proposal of int * int * int * (certificate Simulator.Block.t) * signature (* round, period, step, block, creator_signature *)
  | SoftVote of int * int * int * int * signature  (* round, period, step, block_id, creator_signature *)
  | CertVote of int * int * int * int * signature  (* round, period, step, block_id, creator_signature *)
  | NextVote of int * int * int * int * signature  (* round, period, step, block_id, creator_signature *)
and certificate = {
  mutable certificate: signature list;
  prev_block_certificate: signature list;
}
and signature = Signature of int

module BlockContents = struct
  type t = certificate
end

module AlgorandMsg : (Simulator.Events.Message with type t = alg_msg) = struct 
  type t = alg_msg

  let json kind round period step v creator_id is_priority =
    if is_priority then
      Printf.sprintf "{\"kind\":\"%s\",\"round\":%d,\"period\":%d,\"step\":%d,\"priority\":%d,\"creator\":%d}" kind round period step v creator_id
    else
      Printf.sprintf "{\"kind\":\"%s\",\"round\":%d,\"period\":%d,\"step\":%d,\"block_id\":%d,\"creator\":%d}" kind round period step v creator_id

  let to_json (m:t) : string =
    match m with
    | Priority(round,period,step,priority,Signature(id)) -> json "priority" round period step priority id true
    | Proposal(round,period,step,blk,Signature(id))      -> json "proposal" round period step (Simulator.Block.id blk) id false
    | SoftVote(round,period,step,blk_id,Signature(id))   -> json "softvote" round period step blk_id id false
    | CertVote(round,period,step,blk_id,Signature(id))   -> json "certvote" round period step blk_id id false
    | NextVote(round,period,step,blk_id,Signature(id))   -> json "nextvote" round period step blk_id id false

  let get_size (msg:t) =
    match msg with
    | Proposal(_,_,_,_,_) -> Simulator.Size.Megabyte(block_size)
    | _ -> Simulator.Size.Bit(32*5)

  let processing_time (_:t) =
    10

  let identifier (msg:t) =
    match msg with
    | Priority(_,_,_,_,_)      -> -1
    | Proposal(_,_,_,blk,_)    -> (Simulator.Block.id blk)
    | SoftVote(_,_,_,blk_id,_) -> blk_id
    | CertVote(_,_,_,blk_id,_) -> blk_id
    | NextVote(_,_,_,blk_id,_) -> blk_id

end

module AlgorandEvent   = Simulator.Events.MakeEvent(AlgorandMsg);;
module AlgorandQueue   = Simulator.Events.MakeQueue(AlgorandEvent);;
module AlgorandTimer   = Abstractions.Timer.Make(AlgorandEvent)(AlgorandQueue);;
module AlgorandNetwork = Abstractions.Network.Make(AlgorandEvent)(AlgorandQueue)(AlgorandMsg);;
module AlgorandLogger  = Simulator.Logging.Make(AlgorandMsg)(AlgorandEvent);;
module AlgorandBlock   = Simulator.Block.Make(AlgorandLogger)(BlockContents);;
module AlgorandPoS     = Abstractions.Pos.Make(AlgorandLogger)(AlgorandBlock);;


module AlgorandStatistics = struct

  type ev = AlgorandEvent.t
  type value = AlgorandBlock.block

  (* total messages exchanged during the simulation *)
  let total_messages = ref 0

  (* each index <i> contains the timestamp where node <i> last saw consensus being reached *)
  let last_consensus_time = ref (List.init !Parameters.General.num_nodes (fun _ -> 0))

  (* each index <i> contains the list of time elapsed to finish the block_proposal step of the protocol, in each node <i> *)
  let block_proposal_time = ref (List.init !Parameters.General.num_nodes (fun _ -> []))

  (* each index <i> contains the list of time elapsed between adding blocks to the chain of node <i> *)
  let node_time_between_blocks = ref (List.init !Parameters.General.num_nodes (fun _ -> []))

  (* each index <i> contains the list of time elapsed until a majority of soft-votes is observed *)
  let majority_soft_vote_time = ref (List.init !Parameters.General.num_nodes (fun _ -> []))

  let obs_majority_softvotes nodeID =
    let current_time = (Simulator.Clock.get_timestamp ()) in
    let elapsed_time = current_time - (List.nth !last_consensus_time (nodeID-1)) in
    majority_soft_vote_time := List.mapi (fun i x -> if i=(nodeID-1) then x@[elapsed_time] else x) !majority_soft_vote_time

  let completed_step2 nodeID =
    let current_time = (Simulator.Clock.get_timestamp ()) in
    let elapsed_time = current_time - (List.nth !last_consensus_time (nodeID-1)) in
    block_proposal_time := List.mapi (fun i x -> if i=(nodeID-1) then x@[elapsed_time] else x) !block_proposal_time

  let consensus_reached nodeID _ =
    let current_time = (Simulator.Clock.get_timestamp ()) in
    let elapsed_time = current_time - (List.nth !last_consensus_time (nodeID-1)) in
    last_consensus_time := List.mapi (fun i x -> if i=(nodeID-1) then current_time else x) !last_consensus_time;
    node_time_between_blocks := List.mapi (fun i x -> if i=(nodeID-1) then x@[elapsed_time] else x) !node_time_between_blocks

  let process _ =
    ()

  let get () =
    let per_node_average avg_list = 
      fun x ->
        let sum = ref 0 in
        List.iter (fun y -> sum := !sum + y) x;
        if List.length x > 0 then
          avg_list := !avg_list@[!sum / (List.length x)]
    in
    let to_seconds ms =
      (float_of_int ms) /. 1000.0
    in
    let highest_value list =
      let index = (List.length list) - 1 in
      let v = List.nth (List.sort compare list) index in
      to_seconds v
    in
    let median_value list =
      let index = (List.length list) / 2 in
      let v = List.nth (List.sort compare list) index in
      to_seconds v
    in
    let avg_consensus_time_per_node = ref [] in
    List.iter (per_node_average avg_consensus_time_per_node) !node_time_between_blocks;
    let avg_block_proposal_time_per_node = ref [] in
    List.iter (per_node_average avg_block_proposal_time_per_node) !block_proposal_time;
    let avg_majority_sv_time_per_node = ref [] in
    List.iter (per_node_average avg_majority_sv_time_per_node) !majority_soft_vote_time;
    let avg_consensus_time = median_value !avg_consensus_time_per_node in
    let avg_block_proposal_time = highest_value !avg_block_proposal_time_per_node in
    let avg_majority_sv_time = median_value !avg_majority_sv_time_per_node in
    Printf.sprintf "{\"final-step\":%.2f,\"block-proposal\":%.2f,\"majority-softvotes\":%.2f}" avg_consensus_time avg_block_proposal_time avg_majority_sv_time

end



module AlgorandNode : (Protocol.Node with type ev=AlgorandEvent.t and type value=AlgorandBlock.block) = struct

  type value = AlgorandBlock.block

  module V = struct
    type v = value
  end

  include Abstract.MakeBaseNode(V)

  type ev = AlgorandEvent.t

  type node_data = {
    mutable received_blocks : AlgorandBlock.block list;
    mutable round : int;
    mutable period : int;
    mutable step : int;
    mutable starting_value : AlgorandBlock.block option;
    mutable cert_voted : AlgorandBlock.block option;
    mutable highest_priority : (int*int) option; (* priority, node_id *)
    mutable proposals : alg_msg list;
    mutable softvotes : alg_msg list;
    mutable certvotes : alg_msg list;
    mutable nextvotes : alg_msg list;
    mutable prev_softvotes : alg_msg list;
    mutable prev_nextvotes : alg_msg list;
    mutable unprocessed_messages : alg_msg list;
    mutable highest_block_id : int
  }

  type t = (node_data, value) Abstract.template

  let init id links region : (t) =
    let cert = {certificate=[];prev_block_certificate=[]} in
    {
      id = id;
      region = region;
      links = links;
      state = AlgorandBlock.genesis_pos 0 cert;
      data = {
        received_blocks = [];
        round = 1;
        period = 1;
        step = 0;
        starting_value = None;
        cert_voted = None;
        highest_priority = None;
        proposals = [];
        softvotes = [];
        certvotes = [];
        nextvotes = [];
        prev_softvotes = [];
        prev_nextvotes = [];
        unprocessed_messages = [];
        highest_block_id = 0
      }
    }

  (* let debug (node:t) msg =
    let s = Printf.sprintf "Node %d : %s at %d" node.id msg (Simulator.Clock.get_timestamp ()) in
    print_endline s *)

  let is_proposer (node:t) =
    AlgorandPoS.is_proposer node.id node.state num_proposers [node.data.round]
  
  let proposer_priority (node:t) =
    AlgorandPoS.proposer_priority node.id node.state num_proposers [node.data.round]

  let in_committee (node:t) =
    AlgorandPoS.in_committee node.id node.state committee_size [node.data.round]

  let add_to_chain (node:t) block =
    node.state <- block;
    let tmp = List.filter (
      fun msg ->
        match msg with
        | CertVote(_,_,_,_,_) -> true
        | _ -> false) node.data.certvotes in
    let certificate = List.map (
      fun msg -> 
        match msg with
        | CertVote(_,_,_,_,signature) -> signature
        | _ -> Signature(-1) (* Since we filter first, this case will never be used *)
      ) tmp in
    node.state.contents.certificate <- certificate

  let register_block (node:t) blk =
    match blk with
    | Some(b) -> 
      if not (List.exists (fun x -> AlgorandBlock.id x = AlgorandBlock.id b) node.data.received_blocks) then
        node.data.received_blocks <- node.data.received_blocks @ [b];
      node
    | None -> node

  let get_block_id msg =
    match msg with
    | Priority(_,_,_,_,_)      -> -1
    | Proposal(_,_,_,blk,_)    -> Simulator.Block.id blk
    | SoftVote(_,_,_,blk_id,_) -> blk_id
    | CertVote(_,_,_,blk_id,_) -> blk_id
    | NextVote(_,_,_,blk_id,_) -> blk_id
  
  let get_block (node:t) block_id =
    let rec get_blk blocks =
      match blocks with
      | [] -> None
      | x::xs -> if Simulator.Block.id x = block_id then Some x else get_blk xs
    in
    get_blk node.data.received_blocks

  let most_voted (node:t) list =
    let vote_counts = ref ((List.init (node.data.highest_block_id) (fun i -> (i+1, 0)))@[(-1,0)]) in
    let most_voted  = ref None in
    let max_votes   = ref 0 in
    let f = fun m -> 
      let bid = get_block_id m
      in
      vote_counts := List.map (fun (b,c) -> if b = bid then (b,c+1) else (b,c)) !vote_counts
    in
    List.iter f list;
    List.iter (fun (b,v) -> if v >= !max_votes then begin most_voted := Some b; max_votes := v end) !vote_counts;
    (!most_voted, !max_votes >= majority_votes)

  let find_leader_proposal (node:t) =
    let leader_proposal : AlgorandBlock.block option ref = ref None in
    let find_leader proposal =
      match proposal with
      | Proposal(_,_,_,blk,Signature(creator)) -> 
        begin
          match node.data.highest_priority with
          | Some(_,node_id) -> 
            if creator = node_id then leader_proposal := Some(blk)
          | None -> ()
        end
      | _ -> ()
    in
    List.iter find_leader node.data.proposals;
    match !leader_proposal with
    | Some blk -> Simulator.Block.id blk
    | None -> -1

  (** returns the most recent message, between <msg1> and <msg2> [@Pre: messages must have the same subtype] *)
  let most_recent msg1 msg2 =
    let compare_time (r1,p1,s1) (r2,p2,s2) =
      if r1 > r2 then true
      else if r2 > r1 then false
      else if p1 > p2 then true
      else if p2 > p1 then false
      else if s1 > s2 then true
      else false
    in
    let res = match (msg1,msg2) with
    | (Proposal(r1,p1,s1,_,_),Proposal(r2,p2,s2,_,_)) -> compare_time (r1, p1, s1) (r2, p2, s2)
    | (SoftVote(r1,p1,s1,_,_),SoftVote(r2,p2,s2,_,_)) -> compare_time (r1, p1, s1) (r2, p2, s2)
    | (CertVote(r1,p1,s1,_,_),CertVote(r2,p2,s2,_,_)) -> compare_time (r1, p1, s1) (r2, p2, s2)
    | (NextVote(r1,p1,s1,_,_),NextVote(r2,p2,s2,_,_)) -> compare_time (r1, p1, s1) (r2, p2, s2)
    | _ -> false
    in
    if res then msg1 else msg2

  let get_creator msg =
    match msg with
    | Priority(_,_,_,_,Signature(creator)) -> creator
    | Proposal(_,_,_,_,Signature(creator)) -> creator
    | SoftVote(_,_,_,_,Signature(creator)) -> creator
    | CertVote(_,_,_,_,Signature(creator)) -> creator
    | NextVote(_,_,_,_,Signature(creator)) -> creator

  let rec contains_msg list msg =
    match list with
    | [] -> false
    | x::xs -> ((get_creator x) = (get_creator msg)) || contains_msg xs msg

  let old_msg (node:t) msg =
    let (r,p) = 
      match msg with
      | Priority(round,period,_,_,_) -> (round,period)
      | Proposal(round,period,_,_,_) -> (round,period)
      | SoftVote(round,period,_,_,_) -> (round,period)
      | CertVote(round,period,_,_,_) -> (round,period)
      | NextVote(round,period,_,_,_) -> (round,period)
    in
    r < node.data.round || ((r = node.data.round) && (p < node.data.period))

  let future_msg (node:t) msg =
    let (r,p) = 
      match msg with
      | Priority(round,period,_,_,_) -> (round,period)
      | Proposal(round,period,_,_,_) -> (round,period)
      | SoftVote(round,period,_,_,_) -> (round,period)
      | CertVote(round,period,_,_,_) -> (round,period)
      | NextVote(round,period,_,_,_) -> (round,period)
    in
    r > node.data.round || ((r = node.data.round) && (p > node.data.period))

  let add_no_duplicate (node:t) msg =
    (* Note: if there is a message from the same creator,
      only keeps the message from the latest round/period/step *)
    let keep_most_recent elem =
      if get_creator elem = get_creator msg then most_recent msg elem else elem
    in
    match msg with
    | Priority(_,_,_,priority,Signature(creator_id)) ->
      begin
        match node.data.highest_priority with
        | None      -> node.data.highest_priority <- Some(priority, creator_id); (node, false)
        | Some(p,_) -> 
          if p < priority then 
            begin
              node.data.highest_priority <- Some(priority, creator_id);
              (node,false)
            end
          else (node,true)
      end
    | Proposal(_,_,_,_,_) -> 
      if contains_msg node.data.proposals msg then
        begin node.data.proposals <- List.map keep_most_recent node.data.proposals; (node, true) end
      else
        begin node.data.proposals <- node.data.proposals @ [msg]; (node, false) end
    | SoftVote(_,_,_,_,_) -> 
      if contains_msg node.data.softvotes msg then
        begin node.data.softvotes <- List.map keep_most_recent node.data.softvotes; (node, true) end
      else
        begin node.data.softvotes <- node.data.softvotes @ [msg]; (node, false) end
    | CertVote(_,_,_,_,_) -> 
      if contains_msg node.data.certvotes msg then
        begin node.data.certvotes <- List.map keep_most_recent node.data.certvotes; (node, true) end
      else
        begin node.data.certvotes <- node.data.certvotes @ [msg]; (node, false) end
    | NextVote(_,_,_,_,_) -> 
      if contains_msg node.data.nextvotes msg then
        begin node.data.nextvotes <- List.map keep_most_recent node.data.nextvotes; (node, true) end
      else
        begin node.data.nextvotes <- node.data.nextvotes @ [msg]; (node, false) end

  let send_to_neighbours (node:t) msg =
    AlgorandNetwork.gossip node.id msg;
    let (new_state, _) = add_no_duplicate node msg in
    new_state

  let inc_step (node:t) lambda =
    node.data.step <- node.data.step +1;
    AlgorandTimer.set node.id lambda "step";
    node

  (* TODO : update the RPS if we have seen majority of softvotes, nextvotes, certvotes, etc? *)
  let process_pending_msgs (node:t) =
    let future = ref [] in
    let process = fun msg ->
      if old_msg node msg then ()
      else
        (
          if future_msg node msg then
            future := !future@[msg]
          else
            let (new_state,duplicate) =
              begin
                match msg with
                | Priority(_,_,_,_,_) -> add_no_duplicate node msg
                | Proposal(_,_,_,blk,_) ->
                  begin
                    node.data.highest_block_id <- max node.data.highest_block_id (Simulator.Block.id blk);
                    let s = register_block node (Some(blk)) in
                    add_no_duplicate s msg
                  end
                | SoftVote(_,_,_,_,_) -> add_no_duplicate node msg
                | CertVote(_,_,_,_,_) -> add_no_duplicate node msg
                | NextVote(_,_,_,_,_) -> add_no_duplicate node msg
              end
            in
            match duplicate with
            | true -> ()
            | false -> let _ = register_block new_state (get_block node (get_block_id msg)) in ()
        )
    in
    List.iter process node.data.unprocessed_messages;
    node.data.unprocessed_messages <- !future;
    node

  let advance_period (node:t) most_next_voted_id =
    let starting_id = 
      match most_next_voted_id with 
      | Some(i) -> i
      | None -> -1
    in
    node.data.period <- node.data.period +1;
    node.data.step <- 0;
    node.data.cert_voted <- None;
    node.data.prev_nextvotes <- node.data.nextvotes;
    node.data.prev_softvotes <- node.data.softvotes;
    node.data.highest_priority <- None;
    node.data.nextvotes <- [];
    node.data.softvotes <- [];
    node.data.certvotes <- [];
    node.data.proposals <- [];
    node.data.starting_value <- get_block node starting_id;
    AlgorandTimer.cancel node.id "step";
    AlgorandTimer.set node.id 1 "step";
    process_pending_msgs node
  
  let advance_round (node:t) =
    node.data.round <- node.data.round + 1;
    node.data.period <- 1;
    node.data.step <- 0;
    node.data.nextvotes <- [];
    node.data.softvotes <- [];
    node.data.certvotes <- [];
    node.data.highest_priority <- None;
    node.data.prev_nextvotes <- [];
    node.data.prev_softvotes <- [];
    node.data.starting_value <- None;
    node.data.cert_voted <- None;
    node.data.proposals <- [];
    node.data.received_blocks <- [];
    AlgorandTimer.cancel node.id "step";
    AlgorandTimer.set node.id 1 "step";
    process_pending_msgs node

  let halting_condition (node:t) =
    let (most_next_voted, got_majority) = most_voted node node.data.certvotes in
    match (most_next_voted, got_majority) with
    | (Some bid, true) -> 
      begin
        match get_block node bid with
        | Some b -> add_to_chain node b; (advance_round node, true)
        | None -> (node, false)
      end
    | _ -> (node, false)

  let create_and_propose_block (node:t) =
    let cert = {certificate=[];prev_block_certificate=node.state.contents.certificate} in
    let blk = AlgorandBlock.create node.id node.state cert in
    let priority = proposer_priority node in
    node.data.starting_value <- Some (blk);
    node.data.highest_priority <- Some(priority, node.id);
    node.data.highest_block_id <- max node.data.highest_block_id (Simulator.Block.id blk);
    let s = register_block node (Some(blk)) in
    let ns = send_to_neighbours s (Priority(node.data.round, node.data.period, node.data.step, priority, Signature(node.id))) in
    send_to_neighbours ns (Proposal(node.data.round, node.data.period, node.data.step, blk, Signature(node.id)))

  (* step 0 *)
  let valueProposal (node:t) =
    let new_state =
      begin
        match node.data.period with 
        | 1 -> 
            if is_proposer node then
                create_and_propose_block node
            else
                node
        | _ ->
          begin
            match most_voted node node.data.prev_nextvotes with
            | (Some id, true) -> 
              begin 
                let blk = get_block node id in
                match blk with
                | Some(b) -> 
                  if in_committee node then 
                    send_to_neighbours node (Proposal(node.data.round, node.data.period, node.data.step, b, Signature(node.id)))
                  else
                    node
                | None -> if is_proposer node then create_and_propose_block node else node
              end
            | _ ->
              if is_proposer node then create_and_propose_block node else node
          end
      end
    in
    inc_step new_state (lambda_priority + lambda_stepvar)

  (* step 1 *)
  (* if already received the highest priority block, then run next step; else wait for the lambdablock timout *)
  let identifyPriority (node:t) =
    let lambda =
      if find_leader_proposal node != -1 then 0 else lambda_block
    in
    inc_step node lambda

  (* step 2 *)
  let filteringStep (node:t) =
    let new_state = 
      if in_committee node then
        begin
          if node.data.period >= 2 then
            begin
              match most_voted node node.data.prev_nextvotes with
              | (Some bid, true) ->
                begin
                  match get_block node bid with
                  | Some(_) -> send_to_neighbours node (SoftVote(node.data.round, node.data.period, node.data.step, bid, Signature(node.id)))
                  | None -> send_to_neighbours node (SoftVote(node.data.round, node.data.period, node.data.step, find_leader_proposal node, Signature(node.id)))
                end
              | _ -> send_to_neighbours node (SoftVote(node.data.round, node.data.period, node.data.step, find_leader_proposal node, Signature(node.id)))
            end
          else
            begin
              send_to_neighbours node (SoftVote(node.data.round, node.data.period, node.data.step, find_leader_proposal node, Signature(node.id)))
            end
        end
      else
        node
    in
    AlgorandStatistics.completed_step2 node.id;
    inc_step new_state lambda_step

  (* step 3 *)
  let certifyingStep (node:t) =
    let new_state =
      if in_committee node then
        begin
        match most_voted node node.data.softvotes with 
        | (Some bid, true) -> 
          begin
            match get_block node bid with
            | Some(_) ->
              begin
                node.data.cert_voted <- get_block node bid;
                send_to_neighbours node (CertVote(node.data.round, node.data.period, node.data.step, bid, Signature(node.id)))
              end
            | None -> node
          end
        | _ -> node
        end
      else node
    in
    inc_step new_state lambda_step

  (* step 4 *)
  let finishingStep1 (node:t) =
    let new_state =
      if in_committee node then 
        begin
          match node.data.cert_voted with
          | (Some blk) ->
            send_to_neighbours node (NextVote(node.data.round, node.data.period, node.data.step, Simulator.Block.id blk, Signature(node.id)))
          | None  -> 
            begin
              match most_voted node node.data.nextvotes with
              | (Some -1, true) -> 
                if node.data.period >= 2 then
                  send_to_neighbours node (NextVote(node.data.round, node.data.period, node.data.step, -1, Signature(node.id)))
                else
                  node
              | _ -> 
                let b_id =
                match node.data.starting_value with
                | Some blk -> Simulator.Block.id blk
                | None -> -1
                in
                send_to_neighbours node (NextVote(node.data.round, node.data.period, node.data.step, b_id, Signature(node.id)))
            end
        end
      else node
    in
    inc_step new_state lambda_step
    
  (* step 5 *)
  let finishingStep2 (node:t) =
    if in_committee node then
      begin
        begin
          match most_voted node node.data.softvotes with
          | (Some -1, _)  -> ()
          | (Some bid, true) -> 
            let _ = send_to_neighbours node (NextVote(node.data.round, node.data.period, node.data.step, bid, Signature(node.id))) in ()
          | _ -> ()
        end;
        begin
          if (node.data.period >= 2) && (node.data.cert_voted = None) then
            begin
              match most_voted node node.data.prev_nextvotes with
              | (Some -1, true) -> let _ = send_to_neighbours node (NextVote(node.data.round, node.data.period, node.data.step, -1, Signature(node.id))) in ()
              | _ -> ()
            end
        end;
      end;
    match halting_condition node with
    | (new_state, true) -> new_state
    | (_, false) -> 
      begin
        match most_voted node node.data.nextvotes with
        | (Some bid, true) -> advance_period node (Some bid)
        | _ -> inc_step node lambda_step
      end

  let run_step (node:t) =
    match node.data.step with
    | 0 -> valueProposal node
    | 1 -> identifyPriority node
    | 2 -> filteringStep node
    | 3 -> certifyingStep node
    | 4 -> finishingStep1 node
    | _ -> finishingStep2 node

  let check_conditions (node:t) =
    (* if at any point the halting condition is verified, move to the next round *)
    match halting_condition node with
      | (new_state, true) -> new_state
      | _ -> 
        (
        match node.data.step with
        | 2 -> (* if we have already received the proposal with highest priority, we run the step, without waiting for timer *)
          if find_leader_proposal node != -1 then 
            begin
              AlgorandTimer.cancel node.id "step";
              run_step node
            end
          else node
        | 3 -> (* if we already received a majority of softvotes, we can run the step imediately, without waiting for timer *)
          begin
            match most_voted node node.data.softvotes with 
            | (Some _, true) -> AlgorandTimer.cancel node.id "step"; AlgorandStatistics.obs_majority_softvotes node.id; run_step node
            | _ -> node
          end
        | _ -> node 
        )

  let store_future_message (node:t) msg =
    node.data.unprocessed_messages <- node.data.unprocessed_messages@[msg];
    node

  let process_msg (node:t) msg =
    if old_msg node msg then 
      node
    else
      (
        if future_msg node msg then
          store_future_message node msg
        else
          (
            let (new_state, duplicate) =
              begin
                match msg with
                | Priority(_,_,_,_,_) -> add_no_duplicate node msg
                | Proposal(_,_,_,blk,_) ->
                  begin
                    let parent_certificate = blk.contents.prev_block_certificate in
                    let first_block = blk.header.parent = Some(0) in
                    if (List.length parent_certificate >= majority_votes) || (first_block) then
                    (
                      node.data.highest_block_id <- max node.data.highest_block_id (Simulator.Block.id blk);
                      let s = register_block node (Some(blk)) in
                      add_no_duplicate s msg
                    )
                    else
                      (node, true) (* it might not be duplicate, but it is invalid which is also ignored *)
                  end
                | SoftVote(_,_,_,_,_) -> add_no_duplicate node msg
                | CertVote(_,_,_,_,_) -> add_no_duplicate node msg
                | NextVote(_,_,_,_,_) -> add_no_duplicate node msg
              end
            in
            match duplicate with
            | true -> check_conditions new_state
            | false -> 
              let new_state2 = register_block new_state (get_block node (get_block_id msg)) in
              check_conditions new_state2
          )
      )

  let handle (node:t) (event:ev) : t =
    match event with
    | AlgorandEvent.Message(_,_,_,msg) -> process_msg node msg
    | AlgorandEvent.Timeout(_,_,label) ->
        begin
          match label with
          | "step" -> run_step node
          | _ -> node
        end
    | _ -> node

  let chain_height (node:t) =
    AlgorandBlock.height node.state

end


module AlgorandInitializer : (Abstract.Initializer with type node=AlgorandNode.t and type ev=AlgorandEvent.t) = struct
  type node = AlgorandNode.t

  type ev = AlgorandEvent.t

  let init nodes = 
    let evs = ref [] in
    Hashtbl.iter (fun nid _ -> evs := !evs @ [AlgorandEvent.Timeout(nid, 0, "step")]) nodes;
    !evs
  
end

module AlgorandProtocol = Protocol.Make(AlgorandEvent)(AlgorandQueue)(AlgorandBlock)(AlgorandTimer)(AlgorandNode)(AlgorandNode)(AlgorandInitializer)(AlgorandLogger)(AlgorandStatistics);;