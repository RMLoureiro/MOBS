(* load protocol specific parameters *)
let lambda         = 2000;;
let committee_size = 20;;
let num_proposers  = 2;;
let majority_votes = 15;;


type alg_msg = 
  Proposal of int * int * int * Simulator.Block.t * int (* round, period, step, block,    creator_id *)
  | SoftVote of int * int * int * int * int             (* round, period, step, block_id, creator_id *)
  | CertVote of int * int * int * int * int             (* round, period, step, block_id, creator_id *)
  | NextVote of int * int * int * int * int             (* round, period, step, block_id, creator_id *)

module AlgorandMsg : (Simulator.Events.Message with type t = alg_msg) = struct 
  type t = alg_msg

  let json round period step blk_id creator_id =
    String.concat "" ["{\"round\":";string_of_int round;",\"period\":";string_of_int period; ",\"step\":";string_of_int step; ",\"block_id\":"; string_of_int blk_id; ",\"creator_id\":"; string_of_int creator_id;"}"]

  let to_json (m:t) : string =
    match m with
    | Proposal(round,period,step,blk,creator_id)    -> json round period step (Simulator.Block.id blk) creator_id
    | SoftVote(round,period,step,blk_id,creator_id) -> json round period step blk_id creator_id
    | CertVote(round,period,step,blk_id,creator_id) -> json round period step blk_id creator_id
    | NextVote(round,period,step,blk_id,creator_id) -> json round period step blk_id creator_id

end

module AlgorandEvent   = Simulator.Events.MakeEvent(AlgorandMsg);;
module AlgorandQueue   = Simulator.Events.MakeQueue(AlgorandEvent);;
module AlgorandNetwork = Abstractions.Network.Make(AlgorandEvent)(AlgorandQueue);;
module AlgorandLogger  = Simulator.Logging.Make(AlgorandMsg)(AlgorandEvent);;
module AlgorandPoS     = Abstractions.Pos.Make(AlgorandLogger);;

module AlgorandNode : (Protocol.Node with type ev=AlgorandEvent.t and type id = int) = struct

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
    mutable proposals : alg_msg list;
    mutable softvotes : alg_msg list;
    mutable certvotes : alg_msg list;
    mutable nextvotes : alg_msg list;
    mutable prev_softvotes : alg_msg list;
    mutable prev_nextvotes : alg_msg list
  }

  let init id links region =
    {
      id = id;
      region = region;
      links = links;
      received_blocks = [];
      chain = Simulator.Block.genesis_pos 0;
      round = 1;
      period = 1;
      step = 1;
      starting_value = None;
      cert_voted = None;
      proposals = [];
      softvotes = [];
      certvotes = [];
      nextvotes = [];
      prev_softvotes = [];
      prev_nextvotes = [];
    }
  
  let send_to_neighbours node msg =
    List.iter (fun neighbour -> AlgorandNetwork.send node.id neighbour msg) node.links

  let add_to_chain node block =
    node.chain <- block

  let rec contains_msg list msg =
    match list with
    | [] -> false
    | x::xs -> x=msg || contains_msg xs msg

  let add_no_duplicate node msg =
    match msg with
    | Proposal(_,_,_,_,_)    -> 
      if contains_msg node.proposals msg then (node, true) else begin node.proposals <- node.proposals @ [msg]; (node, false) end
    | SoftVote(_,_,_,_,_) -> 
      if contains_msg node.softvotes msg then (node, true) else begin node.softvotes <- node.softvotes @ [msg]; (node, false) end
    | CertVote(_,_,_,_,_) -> 
      if contains_msg node.certvotes msg then (node, true) else begin node.certvotes <- node.certvotes @ [msg]; (node, false) end
    | NextVote(_,_,_,_,_) -> 
      if contains_msg node.nextvotes msg then (node, true) else begin node.nextvotes <- node.nextvotes @ [msg]; (node, false) end

  (* TODO : only register if the block hasn't been registered before *)
  let register_block node blk =
    match blk with
    | Some(b) -> node.received_blocks <- node.received_blocks @ [b]; node
    | None -> node

  let get_block_id msg =
    match msg with
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
    let vote_counts = ref ((List.init (List.length node.received_blocks) (fun i -> (Simulator.Block.id (List.nth node.received_blocks i), 0)))@[(-1,0)]) in
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
    let min_hash : string option ref = ref None in
    let find_leader proposal =
      match proposal with
      | Proposal(_,_,_,bid,creator) -> 
        begin
          let cred = String.concat "" [string_of_int creator; string_of_int node.round; string_of_int node.period] in
          let hash = Sha256.to_hex (Sha256.string cred) in
          match !min_hash with
          | Some h ->
            if hash < h then
              begin
                leader_proposal := Some bid;
                min_hash := Some hash
              end
          | None ->
            leader_proposal := Some bid;
            min_hash := Some hash
        end
      | _ -> ()
    in
    List.iter find_leader node.proposals;
    match !leader_proposal with
    | Some blk -> Simulator.Block.id blk
    | None -> -1

  let is_proposer node =
    AlgorandPoS.is_proposer node.id node.chain num_proposers [node.round]
  
  let in_committee node =
    AlgorandPoS.in_committee node.id node.chain committee_size [node.round]

  let create_and_propose_block node =
    node (* TODO *)

  let advance_period node most_next_voted_id =
    let starting_id = 
      match most_next_voted_id with 
      | Some(i) -> i
      | None -> -1
    in
    node.period <- node.period +1;
    node.step <- 1;
    node.cert_voted <- None;
    node.prev_nextvotes <- node.nextvotes;
    node.prev_softvotes <- node.softvotes;
    node.nextvotes <- [];
    node.softvotes <- [];
    node.certvotes <- [];
    node.starting_value <- get_block node starting_id;
    node
    (* TODO : timer? *)
  
  let advance_round node =
    node.round <- node.round + 1;
    node.period <- 1;
    node.step <- 1;
    node.nextvotes <- [];
    node.softvotes <- [];
    node.certvotes <- [];
    node.prev_nextvotes <- [];
    node.prev_softvotes <- [];
    node.starting_value <- None;
    node.cert_voted <- None;
    node.received_blocks <- [];
    node
    (* TODO : timer? *)

  let inc_step node =
    node.step <- node.step +1;
    node
    (* TODO : timer? *)

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

  (* step 1 *)
  let valueProposal node =
    let new_state =
      begin
        match node.period with 
        | 1 -> if is_proposer node then create_and_propose_block node else node
        | _ ->
          begin
            match most_voted node node.prev_nextvotes with
            | (Some id, true) -> 
              begin 
                let blk = get_block node id in
                match blk with
                | Some(b) -> begin if in_committee node then 
                  send_to_neighbours node (Proposal(node.round, node.period, node.step, b, node.id))
                end; node
                | None -> node
              end
            | _ ->
              if is_proposer node then create_and_propose_block node else node
          end

      end
    in
    inc_step new_state

  (* step 2 *)
  let filteringStep node =
    let new_state = 
      if in_committee node then
        begin
          if node.period >= 2 then
            begin
              match most_voted node node.prev_nextvotes with
              | (Some bid, true) -> send_to_neighbours node (SoftVote(node.round, node.period, node.step, bid, node.id)); node
              | _ -> send_to_neighbours node (SoftVote(node.round, node.period, node.step, find_leader_proposal node, node.id)); node
            end
          else
            begin
              send_to_neighbours node (SoftVote(node.round, node.period, node.step, find_leader_proposal node, node.id)); node
            end
        end
      else
        node
    in
    inc_step new_state

  (* step 3 *)
  let certifyingStep node =
    let new_state =
      if in_committee node then
        begin
        match most_voted node node.certvotes with 
        | (Some bid, true) -> 
          node.cert_voted <- get_block node bid;
          send_to_neighbours node (CertVote(node.round, node.period, node.step, bid, node.id));
          node
        | _ -> node
        end
      else node
    in
    inc_step new_state

  (* step 4 *)
  let finishingStep1 node =
    let new_state =
      if in_committee node then 
        begin
          match node.cert_voted with
          | (Some blk) ->
            send_to_neighbours node (NextVote(node.round, node.period, node.step, Simulator.Block.id blk, node.id));
            node
          | None  -> 
            begin
              match most_voted node node.nextvotes with
              | (Some -1, true) -> 
                if node.period >= 2 then
                  send_to_neighbours node (NextVote(node.round, node.period, node.step, -1, node.id));
                node
              | _ -> 
                let b_id =
                match node.starting_value with
                | Some blk -> Simulator.Block.id blk
                | None -> -1
                in
                send_to_neighbours node (NextVote(node.round, node.period, node.step, b_id, node.id));
                node
            end
        end
      else node
    in
    inc_step new_state
    
  (* step 5 *)
  let finishingStep2 node =
    if in_committee node then
      begin
        match most_voted node node.softvotes with
        | (Some bid, true) -> 
          send_to_neighbours node (NextVote(node.round, node.period, node.step, bid, node.id))
        | _ ->
          if node.period >= 2 then send_to_neighbours node (NextVote(node.round, node.period, node.step, -1, node.id))
      end;
    match halting_condition node with
    | (new_state, true) -> new_state
    | (_, false) -> 
      begin
        match most_voted node node.nextvotes with
        | (Some bid, true) -> advance_period node (Some bid)
        | _ -> inc_step node
      end

  let run_step node =
    match node.step with
    | 1 -> valueProposal node
    | 2 -> filteringStep node
    | 3 -> certifyingStep node
    | 4 -> finishingStep1 node
    | _ -> finishingStep2 node

  let old_msg node msg =
    let (r,p) = 
      match msg with
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
          | Proposal(_,_,_,_,_) -> add_no_duplicate node msg
          | SoftVote(_,_,_,_,_) -> add_no_duplicate node msg
          | CertVote(_,_,_,_,_) ->
            let (s,d)  = add_no_duplicate node msg in
            let (ns,_) = halting_condition s in (ns, d)
          | NextVote(_,_,_,_,_) -> 
            let (s,d) = add_no_duplicate node msg in
            let (most_next_voted, got_majority) = most_voted node node.nextvotes in
            if got_majority then (advance_period s most_next_voted, d) else (s, d)
        end
      in
      match duplicate with
      | true  -> new_state
      | false -> send_to_neighbours new_state msg; register_block new_state (get_block node (get_block_id msg))

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

  let chain_height node = 
    Simulator.Block.height node.chain

end

module AlgorandInitializer : (Protocol.Initializer with type node=AlgorandNode.t and type ev=AlgorandEvent.t) = struct
  type node = AlgorandNode.t

  type ev = AlgorandEvent.t

  let init nodes = 
    let evs = ref [] in
    Hashtbl.iter (fun nid _ -> evs := !evs @ [AlgorandEvent.Timeout(nid, 0, "step")]) nodes;
    !evs
  
end

module AlgorandProtocol = Protocol.Make(AlgorandEvent)(AlgorandQueue)(AlgorandNode)(AlgorandInitializer)(AlgorandLogger);;
