open Implementation

module BlockContents = struct
  type t = unit
end

type msg = 
  Init of int (*sender*)
  | Start of int * int * int  (*sender coordinator round*)
  | Preference of int * int * int (*sender round value*)
  | Accept of int * int * int * int (*sender coordinator round value*)
  | Ack of int * int * int (*sender round value*)
  | Consensus of int * int (*sender value*)

module CTMsg : (Simulator.Events.Message with type t = msg) = struct 
  type t = msg

  let to_json (msg:t) : string = 
    match msg with
    | Init(sender) ->  Printf.sprintf "{\"type\":\"Init\", \"node\":\"%d\"}" sender
    | Start(sender, _,round) ->  Printf.sprintf "{\"type\":\"Start\", \"node\":\"%d\", \"Round\":\"%d\"}" sender round
    | Preference(sender, _, value) ->  Printf.sprintf "{\"type\":\"ReceivePreference\", \"node\":\"%d\", \"Value\":\"%d\"}" sender value
    | Accept(sender, _, _,value) ->  Printf.sprintf "{\"type\":\"Accept\", \"node\":\"%d\", \"Value\":\"%d\"}" sender value
    | Ack(sender, _, value) ->  Printf.sprintf"{\"type\":\"Accepted\",\"receiver\":\"%d\", \"Value\": \"%d\"}" sender value
    | Consensus(sender, value) ->  Printf.sprintf "{\"type\":\"Consensus Reached\",\"N\":\"%d\", \"Value\": \"%d\"}" sender value



  let get_size (msg:t) =
    match msg with
    | Init (_) -> Simulator.Size.Bit(32)
    | Start (_,_,_) -> Simulator.Size.Bit(32)
    | Preference (_,_,_) -> Simulator.Size.Bit(32)
    | Accept (_,_,_,_) -> Simulator.Size.Bit(32)
    | Ack (_,_,_) -> Simulator.Size.Bit(32)
    | Consensus (_,_) -> Simulator.Size.Bit(32)

  let processing_time (_:t) =
    0

  let identifier (msg:t) =
    match msg with
    | Init(sender) -> sender
    | Start(sender, _,round) -> sender * round
    | Preference(sender, _, value) -> sender * value
    | Accept(sender, _ , _, value) -> sender * value + 1
    | Ack(sender, _, value) -> sender * value + 3
    | Consensus(sender, value) -> sender * value + 5
end


module CTEvent   = Simulator.Events.MakeEvent(CTMsg);;
module CTQueue   = Simulator.Events.MakeQueue(CTEvent);;
module CTNetwork = Abstractions.Network.Make(CTEvent)(CTQueue)(CTMsg);;
module CTLogger  = Simulator.Logging.Make(CTMsg)(CTEvent);;
module CTTimer   = Abstractions.Timer.Make(CTEvent)(CTQueue);;
module CTBlock   = Simulator.Block.Make(CTLogger)(BlockContents)(Simulator.Block.BaseRewards);;

module MBPTimeArg = struct
  let label = "median-block-propagation-time"
  let use_intervals = false
  let format = 1
end

module CTStatistics = Simulator.Statistics.Make.Median(MBPTimeArg);;

module CTNode : (Protocol.BlockchainNode with type ev=CTEvent.t and type value=CTBlock.block) = struct

  type value = CTBlock.block

  module V = struct
    type v = value
  end

  include Protocol.MakeBaseNode(V)

  type ev = CTEvent.t

  type node_data = {
    mutable value : int;
    mutable quorum_preference : int list;
    mutable quorum_accept : int list;
    mutable network_size : int;
    mutable accepted : int;
    mutable round : int;
  }

  type t = (node_data, value) Protocol.template

  let init id links region : (t) =
    {
      id = id;
      region = region;
      links = links;
      state = CTBlock.null ();
      data  = {
        value = 0;
        round = 0;
        quorum_preference = [];
        quorum_accept = [];
        network_size = 10;
        accepted = 0;
      }
    }

    (* COORDIANTOR METHODS *)
    let receive_init (node:t) =
      node.data.round <- node.data.round + 1;
      CTNetwork.send_to_neighbors node.id (Start(node.id, node.id, node.data.round));
      node

    let receive_preference (node:t) sender round value =
      if (not (List.mem sender node.data.quorum_preference) && node.data.accepted = 0 && node.data.round = round) then
        begin
          node.data.value <- value;
          node.data.quorum_preference <- node.data.quorum_preference @ [sender];
          if (List.length node.data.quorum_preference > node.data.network_size / 2) then
            begin
              node.data.accepted <- 1;
              CTNetwork.send_to_neighbors node.id (Accept(node.id, node.id, round, value))
            end;
        end;
          node 

    let receive_ack (node:t) sender round value =
      if (node.data.value = value && (not (List.mem sender node.data.quorum_accept)) && node.data.round = round) then
        begin
          node.data.quorum_accept <- node.data.quorum_accept @ [sender];
          if (List.length node.data.quorum_accept > node.data.network_size / 2) then
            CTNetwork.send node.id node.id (Consensus(node.id, value))
        end;
      node

    let receive_consensus (node:t) =
      node.data.accepted <- 0;
      node.data.quorum_accept <- [];
      node.data.quorum_preference <- [];
      node.data.round <- node.data.round + 1;
      CTNetwork.send_to_neighbors node.id (Start(node.id, node.id, node.data.round));
      node

    (* GENERAL METHODS *)
    let receive_start (node:t) coordinator round =
      if (node.data.round < round) then
        begin
          node.data.accepted <- 0;
          node.data.round <- round;
          node.data.value <- Random.int(10000000);
          CTNetwork.send_to_neighbors node.id (Start(node.id, coordinator, round));
          CTNetwork.send node.id coordinator (Preference(node.id, round, node.data.value));
        end;
      node

      let receive_accept (node:t) coordinator round value =
        if (node.data.accepted = 0 && node.data.round <= round) then
          begin
            node.data.accepted <- 1;
            node.data.round<- round;
            node.data.value <- value;
            CTNetwork.send_to_neighbors node.id (Accept(node.id, coordinator, round, value));
            CTNetwork.send node.id coordinator (Ack(node.id, round, node.data.value));
          end;
        node

    let handle (node:t) (event:ev) : t =
      match event with
        | CTEvent.Message(_,_,_,_,msg) -> 
          begin
          match msg with
          | Init(_) -> receive_init node
          | Start(_, coordinator, round) -> receive_start node coordinator round
          | Preference(sender, round, value) -> receive_preference node sender round value
          | Accept(_, coordinator, round,  value) -> receive_accept node coordinator round value
          | Ack(sender, round, value) -> receive_ack node sender round value
          | Consensus(_, _) -> receive_consensus node
          end
        | _ -> node


  let chain_height (node:t) = 
    CTBlock.height node.state

end


module CTInitializer : (Protocol.Initializer with type node=CTNode.t and type ev=CTEvent.t) = struct
  type node = CTNode.t

  type ev = CTEvent.t

  let init _ = 
    [CTEvent.Message(0, 1, 0, 0, Init(1))]

end

module CTProtocol = Protocol.Make.Blockchain(CTEvent)(CTQueue)(CTBlock)(CTTimer)(CTNode)(CTNode)(CTInitializer)(CTLogger)(CTStatistics)(CTNetwork);;
