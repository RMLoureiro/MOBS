(* load protocol specific parameters *)
let lambda_step           = Parameters.Protocol.get_int_parameter "lambda-step";;
let lambda_stepvar        = Parameters.Protocol.get_int_parameter "lambda-stepvar";;
let lambda_priority       = Parameters.Protocol.get_int_parameter "lambda-priority";;
let lambda_block          = Parameters.Protocol.get_int_parameter "lambda-block";;
let committee_size        = Parameters.Protocol.get_int_parameter "committee-size";;
let num_proposers         = Parameters.Protocol.get_int_parameter "num-proposers";;
let majority_votes        = Parameters.Protocol.get_int_parameter "majority-votes";;
let block_size            = Parameters.Protocol.get_int_parameter "block-size-bits";;

type alg_msg = 
    Priority of int * int * int * int * int               (* round, period, step, priority, creator_id *)
  | Proposal of int * int * int * Simulator.Block.t * int (* round, period, step, block,    creator_id *)
  | SoftVote of int * int * int * int * int               (* round, period, step, block_id, creator_id *)
  | CertVote of int * int * int * int * int               (* round, period, step, block_id, creator_id *)
  | NextVote of int * int * int * int * int               (* round, period, step, block_id, creator_id *)

module AlgorandMsg : (Simulator.Events.Message with type t = alg_msg) = struct 
  type t = alg_msg

  let json kind round period step v creator_id is_priority =
    if is_priority then
      String.concat "" ["{\"kind\":\"";kind;"\",\"round\":";string_of_int round;",\"period\":";string_of_int period; ",\"step\":";string_of_int step; ",\"priority\":"; string_of_int v; ",\"creator_id\":"; string_of_int creator_id;"}"]
    else
      String.concat "" ["{\"kind\":\"";kind;"\",\"round\":";string_of_int round;",\"period\":";string_of_int period; ",\"step\":";string_of_int step; ",\"block_id\":"; string_of_int v; ",\"creator_id\":"; string_of_int creator_id;"}"]

  let to_json (m:t) : string =
    match m with
    | Priority(round,period,step,priority,creator_id) -> json "priority" round period step priority creator_id true
    | Proposal(round,period,step,blk,creator_id)      -> json "proposal" round period step (Simulator.Block.id blk) creator_id false
    | SoftVote(round,period,step,blk_id,creator_id)   -> json "softvote" round period step blk_id creator_id false
    | CertVote(round,period,step,blk_id,creator_id)   -> json "certvote" round period step blk_id creator_id false
    | NextVote(round,period,step,blk_id,creator_id)   -> json "nextvote" round period step blk_id creator_id false

  let get_size (msg:t) =
    match msg with
    | Proposal(_,_,_,_,_) -> block_size
    | _ -> 32*5

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
module AlgorandBlock   = Simulator.Block.Make(AlgorandLogger);;
module AlgorandPoS     = Abstractions.Pos.Make(AlgorandLogger)(AlgorandBlock);;


module AlgorandStatistics = struct

  type ev = AlgorandEvent.t
  type value = Simulator.Block.t

  (* total messages exchanged during the simulation *)
  let total_messages = ref 0

  (* each index <i> contains the timestamp where node <i> last saw consensus being reached *)
  let last_consensus_time = ref (List.init !Parameters.General.num_nodes (fun _ -> 0))

  (* each index <i> contains the list of time elapsed to finish the block_proposal step of the protocol, in each node <i> *)
  let block_proposal_time = ref (List.init !Parameters.General.num_nodes (fun _ -> []))

  (* each index <i> contains the list of time elapsed between adding blocks to the chain of node <i> *)
  let node_time_between_blocks = ref (List.init !Parameters.General.num_nodes (fun _ -> []))

  let completed_step2 nodeID =
    let current_time = (Simulator.Clock.get_timestamp ()) in
    let elapsed_time = current_time - (List.nth !last_consensus_time (nodeID-1)) in
    block_proposal_time := List.mapi (fun i x -> if i=(nodeID-1) then x@[elapsed_time] else x) !block_proposal_time

  let consensus_reached nodeID _ =
    let current_time = (Simulator.Clock.get_timestamp ()) in
    let elapsed_time = current_time - (List.nth !last_consensus_time (nodeID-1)) in
    last_consensus_time := List.mapi (fun i x -> if i=(nodeID-1) then current_time else x) !last_consensus_time;
    node_time_between_blocks := List.mapi (fun i x -> if i=(nodeID-1) then x@[elapsed_time] else x) !node_time_between_blocks

  let process event =
    match event with
    | AlgorandEvent.Message(_,_,_,_) -> total_messages := !total_messages +1
    | _ -> ()

  let get () =
    let avg_consensus_time =
      let avg_consensus_time_per_node = ref [] in
      List.iter (fun x -> let sum = ref 0 in List.iter (fun y -> sum := !sum + y) x; avg_consensus_time_per_node := !avg_consensus_time_per_node@[!sum / (List.length x)]) !node_time_between_blocks;
      let sum = ref 0 in
      List.iter (fun x -> sum := !sum + x) !avg_consensus_time_per_node;
      !sum / (List.length !avg_consensus_time_per_node)
    in
    let avg_block_proposal_time =
      let avg_block_proposal_time_per_node = ref [] in
      List.iter (fun x -> let sum = ref 0 in List.iter (fun y -> sum := !sum + y) x; avg_block_proposal_time_per_node := !avg_block_proposal_time_per_node@[!sum / (List.length x)]) !block_proposal_time;
      let sum = ref 0 in
      List.iter (fun x -> sum := !sum + x) !avg_block_proposal_time_per_node;
      !sum / (List.length !avg_block_proposal_time_per_node)
    in
    String.concat "" ["{\"final-step\":";string_of_int avg_consensus_time;",\"block-proposal\":";string_of_int avg_block_proposal_time;"}"]

end








module AlgorandNode : (Protocol.Node with type ev=AlgorandEvent.t and type id=int and type value=Simulator.Block.t) = struct

  type value = Simulator.Block.t

  type id = int

  type ev = AlgorandEvent.t

  type t = {
    id     : id;
    region : Abstractions.Network.region;
    links  : Abstractions.Network.links;
    mutable received_blocks : Simulator.Block.t list;
    mutable chain  : Simulator.Block.t;
    mutable round : int;
    mutable period : int;
    mutable step : int;
    mutable starting_value : Simulator.Block.t option;
    mutable cert_voted : Simulator.Block.t option;
    mutable highest_priority : (int*int) option; (* priority, node_id *)
    mutable proposals : alg_msg list;
    mutable softvotes : alg_msg list;
    mutable certvotes : alg_msg list;
    mutable nextvotes : alg_msg list;
    mutable prev_softvotes : alg_msg list;
    mutable prev_nextvotes : alg_msg list;
    mutable highest_block_id : int
  }

  let init id links region =
    {
      id = id;
      region = region;
      links = links;
      received_blocks = [];
      chain = AlgorandBlock.genesis_pos 0;
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
      highest_block_id = 0
    }

  let add_to_chain node block =
    node.chain <- block

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
    | Priority(_,_,_,_,creator) -> creator
    | Proposal(_,_,_,_,creator) -> creator
    | SoftVote(_,_,_,_,creator) -> creator
    | CertVote(_,_,_,_,creator) -> creator
    | NextVote(_,_,_,_,creator) -> creator

  let rec contains_msg list msg =
    match list with
    | [] -> false
    | x::xs -> ((get_creator x) = (get_creator msg)) || contains_msg xs msg

  let add_no_duplicate node msg =
    (* Note: if there is a message from the same creator,
      only keeps the message from the latest round/period/step *)
    let keep_most_recent elem =
      if get_creator elem = get_creator msg then most_recent msg elem else elem
    in
    match msg with
    | Priority(_,_,_,priority,creator_id) ->
      begin
        match node.highest_priority with
        | None      -> node.highest_priority <- Some(priority, creator_id); (node, false)
        | Some(p,_) -> 
          if p < priority then 
            begin
              node.highest_priority <- Some(priority, creator_id);
              (node,false)
            end
          else (node,true)
      end
    | Proposal(_,_,_,_,_) -> 
      if contains_msg node.proposals msg then
        begin node.proposals <- List.map keep_most_recent node.proposals; (node, true) end
      else
        begin node.proposals <- node.proposals @ [msg]; (node, false) end
    | SoftVote(_,_,_,_,_) -> 
      if contains_msg node.softvotes msg then
        begin node.softvotes <- List.map keep_most_recent node.softvotes; (node, true) end
      else
        begin node.softvotes <- node.softvotes @ [msg]; (node, false) end
    | CertVote(_,_,_,_,_) -> 
      if contains_msg node.certvotes msg then
        begin node.certvotes <- List.map keep_most_recent node.certvotes; (node, true) end
      else
        begin node.certvotes <- node.certvotes @ [msg]; (node, false) end
    | NextVote(_,_,_,_,_) -> 
      if contains_msg node.nextvotes msg then
        begin node.nextvotes <- List.map keep_most_recent node.nextvotes; (node, true) end
      else
        begin node.nextvotes <- node.nextvotes @ [msg]; (node, false) end

  let send_to_neighbours node msg =
    List.iter (fun neighbour -> AlgorandNetwork.send node.id neighbour msg) node.links;
    let (new_state, _) = add_no_duplicate node msg in
    new_state

  let register_block node blk =
    match blk with
    | Some(b) -> 
      if not (List.exists (fun x -> x = b) node.received_blocks) then
        node.received_blocks <- node.received_blocks @ [b];
      node
    | None -> node

  let get_block_id msg =
    match msg with
    | Priority(_,_,_,_,_)      -> -1
    | Proposal(_,_,_,blk,_)    -> Simulator.Block.id blk
    | SoftVote(_,_,_,blk_id,_) -> blk_id
    | CertVote(_,_,_,blk_id,_) -> blk_id
    | NextVote(_,_,_,blk_id,_) -> blk_id
  
  let get_block node block_id =
    let rec get_blk blocks =
      match blocks with
      | [] -> None
      | x::xs -> if Simulator.Block.id x = block_id then Some x else get_blk xs
    in
    get_blk node.received_blocks

  let most_voted node list =
    let vote_counts = ref ((List.init (node.highest_block_id) (fun i -> (i+1, 0)))@[(-1,0)]) in
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

  let find_leader_proposal node =
    let leader_proposal : Simulator.Block.t option ref = ref None in
    let find_leader proposal =
      match proposal with
      | Proposal(_,_,_,blk,creator) -> 
        begin
          match node.highest_priority with
          | Some(_,node_id) -> 
            if creator = node_id then leader_proposal := Some(blk)
          | None -> ()
        end
      | _ -> ()
    in
    List.iter find_leader node.proposals;
    match !leader_proposal with
    | Some blk -> Simulator.Block.id blk
    | None -> -1

  let is_proposer node =
    AlgorandPoS.is_proposer node.id node.chain num_proposers [node.round]
  
  let proposer_priority node =
    AlgorandPoS.proposer_priority node.id node.chain num_proposers [node.round]

  let in_committee node =
    AlgorandPoS.in_committee node.id node.chain committee_size [node.round]

  let create_and_propose_block node =
    let blk = AlgorandBlock.create node.id node.chain in
    let priority = proposer_priority node in
    node.starting_value <- Some (blk);
    node.highest_priority <- Some(priority, node.id);
    let ns = send_to_neighbours node (Priority(node.round, node.period, node.step, priority, node.id)) in
    send_to_neighbours ns (Proposal(node.round, node.period, node.step, blk, node.id))

  let advance_period node most_next_voted_id =
    let starting_id = 
      match most_next_voted_id with 
      | Some(i) -> i
      | None -> -1
    in
    node.period <- node.period +1;
    node.step <- 0;
    node.cert_voted <- None;
    node.prev_nextvotes <- node.nextvotes;
    node.prev_softvotes <- node.softvotes;
    node.highest_priority <- None;
    node.nextvotes <- [];
    node.softvotes <- [];
    node.certvotes <- [];
    node.proposals <- [];
    node.starting_value <- get_block node starting_id;
    AlgorandTimer.set node.id 1 "step"; (* TODO : is it OK to immediately begin the next period *)
    node
  
  let advance_round node =
    node.round <- node.round + 1;
    node.period <- 1;
    node.step <- 0;
    node.nextvotes <- [];
    node.softvotes <- [];
    node.certvotes <- [];
    node.highest_priority <- None;
    node.prev_nextvotes <- [];
    node.prev_softvotes <- [];
    node.starting_value <- None;
    node.cert_voted <- None;
    node.proposals <- [];
    node.received_blocks <- [];
    AlgorandTimer.cancel node.id "step";
    AlgorandTimer.set node.id 1 "step"; (* TODO : is it OK to immediately begin the next round? *)
    node

  let inc_step node lambda =
    node.step <- node.step +1;
    AlgorandTimer.set node.id lambda "step";
    node

  let halting_condition node =
    let (most_next_voted, got_majority) = most_voted node node.certvotes in
    match (most_next_voted, got_majority) with
    | (Some bid, true) -> 
      begin
        match get_block node bid with
        | Some b -> add_to_chain node b; (advance_round node, true)
        | None -> (node, false)
      end
    | _ -> (node, false)

  (* step 0 *)
  let valueProposal node =
    let new_state =
      begin
        match node.period with 
        | 1 -> 
            if is_proposer node then
                create_and_propose_block node
            else
                node
        | _ -> (* TODO : double check if this case needs to change because of the new priority functionalities *)
          begin
            match most_voted node node.prev_nextvotes with
            | (Some id, true) -> 
              begin 
                let blk = get_block node id in
                match blk with
                | Some(b) -> 
                  if in_committee node then 
                    send_to_neighbours node (Proposal(node.round, node.period, node.step, b, node.id))
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
  let identifyPriority node =
    let lambda =
      if find_leader_proposal node != -1 then 1 else lambda_block
    in
    inc_step node lambda

  (* step 2 *)
  let filteringStep node =
    let new_state = 
      if in_committee node then
        begin
          if node.period >= 2 then
            begin
              match most_voted node node.prev_nextvotes with
              | (Some bid, true) ->
                begin
                  match get_block node bid with
                  | Some(_) -> send_to_neighbours node (SoftVote(node.round, node.period, node.step, bid, node.id))
                  | None -> send_to_neighbours node (SoftVote(node.round, node.period, node.step, find_leader_proposal node, node.id))
                end
              | _ -> send_to_neighbours node (SoftVote(node.round, node.period, node.step, find_leader_proposal node, node.id))
            end
          else
            begin
              send_to_neighbours node (SoftVote(node.round, node.period, node.step, find_leader_proposal node, node.id))
            end
        end
      else
        node
    in
    AlgorandStatistics.completed_step2 node.id;
    inc_step new_state lambda_step

  (* step 3 *)
  let certifyingStep node =
    let new_state =
      if in_committee node then
        begin
        match most_voted node node.softvotes with 
        | (Some bid, true) -> 
          begin
            match get_block node bid with
            | Some(_) ->
              begin
                node.cert_voted <- get_block node bid;
                send_to_neighbours node (CertVote(node.round, node.period, node.step, bid, node.id))
              end
            | None -> node
          end
        | _ -> node
        end
      else node
    in
    inc_step new_state lambda_step

  (* step 4 *)
  let finishingStep1 node =
    let new_state =
      if in_committee node then 
        begin
          match node.cert_voted with
          | (Some blk) ->
            send_to_neighbours node (NextVote(node.round, node.period, node.step, Simulator.Block.id blk, node.id))
          | None  -> 
            begin
              match most_voted node node.nextvotes with
              | (Some -1, true) -> 
                if node.period >= 2 then
                  send_to_neighbours node (NextVote(node.round, node.period, node.step, -1, node.id))
                else
                  node
              | _ -> 
                let b_id =
                match node.starting_value with
                | Some blk -> Simulator.Block.id blk
                | None -> -1
                in
                send_to_neighbours node (NextVote(node.round, node.period, node.step, b_id, node.id))
            end
        end
      else node
    in
    inc_step new_state lambda_step
    
  (* step 5 *)
  let finishingStep2 node =
    if in_committee node then
      begin
        begin
          match most_voted node node.softvotes with
          | (Some -1, _)  -> ()
          | (Some bid, true) -> 
            let _ = send_to_neighbours node (NextVote(node.round, node.period, node.step, bid, node.id)) in ()
          | _ -> ()
        end;
        begin
          if (node.period >= 2) && (node.cert_voted = None) then
            begin
              match most_voted node node.prev_nextvotes with
              | (Some -1, true) -> let _ = send_to_neighbours node (NextVote(node.round, node.period, node.step, -1, node.id)) in ()
              | _ -> ()
            end
        end;
      end;
    match halting_condition node with
    | (new_state, true) -> new_state
    | (_, false) -> 
      begin
        match most_voted node node.nextvotes with
        | (Some bid, true) -> advance_period node (Some bid)
        | _ -> inc_step node lambda_step
      end

  let run_step node =
    match node.step with
    | 0 -> valueProposal node
    | 1 -> identifyPriority node
    | 2 -> filteringStep node
    | 3 -> certifyingStep node
    | 4 -> finishingStep1 node
    | _ -> finishingStep2 node

  let check_conditions node =
    match node.step with
    | 2 -> (* if we have already received the proposal with highest priority, we run the step, without waiting for timer *)
      if find_leader_proposal node != -1 then 
        begin
          AlgorandTimer.cancel node.id "step";
          run_step node
        end
      else node
    | 3 -> (* if we already received a majority of softvotes, we can run the step imediately, without waiting for timer *)
      begin
        match most_voted node node.softvotes with 
        | (Some _, true) -> AlgorandTimer.cancel node.id "step"; run_step node
        | _ -> node
      end
    | _ -> (* if at any point the halting condition is verified, move to the next round *)
      begin
        match halting_condition node with
        | (new_state, _) -> new_state
      end

  let old_msg node msg =
    let (r,p) = 
      match msg with
      | Priority(round,period,_,_,_) -> (round,period)
      | Proposal(round,period,_,_,_) -> (round,period)
      | SoftVote(round,period,_,_,_) -> (round,period)
      | CertVote(round,period,_,_,_) -> (round,period)
      | NextVote(round,period,_,_,_) -> (round,period)
    in
    r < node.round || p < node.period-1

  (* TODO : NextVotes and SoftVotes require the check if there are for this period or the previous one *)
  let process_msg node msg =
    match old_msg node msg with
    | true -> node
    | false ->
      let (new_state,duplicate) =
        begin
          match msg with
          | Priority(_,_,_,_,_) -> add_no_duplicate node msg
          | Proposal(_,_,_,blk,_) ->
            begin
              node.highest_block_id <- max node.highest_block_id (Simulator.Block.id blk);
              let s = register_block node (Some(blk)) in
              add_no_duplicate s msg
            end
          | SoftVote(_,_,_,_,_) -> add_no_duplicate node msg
          | CertVote(_,_,_,_,_) -> add_no_duplicate node msg
          | NextVote(_,_,_,_,_) -> add_no_duplicate node msg
        end
      in
      match duplicate with
      | true  -> new_state
      | false -> 
        let new_state2 = send_to_neighbours new_state msg in
        let new_state3 = register_block new_state2 (get_block node (get_block_id msg)) in
        check_conditions new_state3

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

  let compare n1 n2 =
    if n1.id < n2.id then -1 else if n1.id > n2.id then 1 else 0
  
  let state n =
    Some n.chain

  let state_id n =
    Simulator.Block.id n.chain

  let chain_height node = 
    AlgorandBlock.height node.chain

  let parameters () =
    String.concat "" ["{\"lambda-step\":";string_of_int lambda_step;",\"lambda-stepvar\":";string_of_int lambda_stepvar;",\"lambda-priority\":";string_of_int lambda_priority;",\"lambda-block\":";string_of_int lambda_block;",\"number-of-proposers\":";string_of_int num_proposers;",\"committee-size\":";string_of_int committee_size;",\"majority-size\":";string_of_int majority_votes;",\"block-size-bits\":";string_of_int block_size;"}"]

end

module AlgorandInitializer : (Abstract.Initializer with type node=AlgorandNode.t and type ev=AlgorandEvent.t) = struct
  type node = AlgorandNode.t

  type ev = AlgorandEvent.t

  let init nodes = 
    let evs = ref [] in
    Hashtbl.iter (fun nid _ -> evs := !evs @ [AlgorandEvent.Timeout(nid, 0, "step")]) nodes;
    !evs
  
end

module AlgorandProtocol = Protocol.Make(AlgorandEvent)(AlgorandQueue)(AlgorandBlock)(AlgorandNode)(AlgorandInitializer)(AlgorandLogger)(AlgorandStatistics);;
