open Implementation

module BlockContents = struct
  type t = unit
end

type msg = 
  Init of int (*sender*)
  | Prepare of int * int (*sender n*)
  | Promise of int * int * int (*sender n previous_accept*)
  | Accept of int * int * int (*sender n value*)
  | Accepted of int * int * int (*sender n value*)
  | Response of int * int * int (*sender n value*)

module PaxosMsg : (Simulator.Events.Message with type t = msg) = struct 
  type t = msg

  let to_json (msg:t) : string = 
    match msg with
    | Init(receiver) ->  Printf.sprintf "{\"type\":\"Init\", node:\"%d\"}" receiver
    | Prepare(receiver, n) -> Printf.sprintf "{\"type\":\"Prepare\",\"receiver\":\"%d\" N: \"%d\"}" receiver n
    | Promise(receiver, n, previous_accept) -> Printf.sprintf "{\"type\":\"Promise\",\"receiver\":\"%d\" N: \"%d\", PreviousAccept: \"%d\"}" receiver n previous_accept
    | Accept(receiver, n, value) -> Printf.sprintf "{\"type\":\"Accept\",\"receiver\":\"%d\" N: \"%d\", Value: \"%d\"}" receiver n value
    | Accepted(receiver, n, value) -> Printf.sprintf "{\"type\":\"Accepted\",\"receiver\":\"%d\" N: \"%d\", Value: \"%d\"}" receiver n value
    | Response(_, n, value) -> Printf.sprintf "{\"type\":\"Consensus Reached\",\"N\":\"%d\", Value: \"%d\"}" n value


  let get_size (msg:t) =
    match msg with
    | Init (_) -> Simulator.Size.Bit(32)
    | Prepare (_) -> Simulator.Size.Bit(32)
    | Promise (_,_,_) -> Simulator.Size.Bit(32)
    | Accept (_,_,_) -> Simulator.Size.Bit(32)
    | Accepted (_,_,_) -> Simulator.Size.Bit(32)
    | Response (_,_,_) -> Simulator.Size.Bit(32)



  let processing_time (_:t) =
    0

  let identifier (msg:t) =
    match msg with
    | Init(receiver) -> receiver
    | Prepare(receiver, n) -> receiver * n
    | Promise(receiver, n, previous_accept) -> receiver * n * previous_accept
    | Accept(receiver, n, value) -> receiver * n * value
    | Accepted(receiver, n, value) -> receiver * n * value + 1
    | Response(receiver, n, value) -> receiver * n * value * 3
end


module PaxosEvent   = Simulator.Events.MakeEvent(PaxosMsg);;
module PaxosQueue   = Simulator.Events.MakeQueue(PaxosEvent);;
module PaxosNetwork = Abstractions.Network.Make(PaxosEvent)(PaxosQueue)(PaxosMsg);;
module PaxosLogger  = Simulator.Logging.Make(PaxosMsg)(PaxosEvent);;
module PaxosTimer   = Abstractions.Timer.Make(PaxosEvent)(PaxosQueue);;
module PaxosBlock   = Simulator.Block.Make(PaxosLogger)(BlockContents)(Simulator.Block.BaseRewards);;

module MBPTimeArg = struct
  let label = "median-block-propagation-time"
  let use_intervals = false
  let format = 1
end

module PaxosStatistics = Simulator.Statistics.Make.Median(MBPTimeArg);;

module PaxosNode : (Protocol.BlockchainNode with type ev=PaxosEvent.t and type value=PaxosBlock.block) = struct

  type value = PaxosBlock.block

  module V = struct
    type v = value
  end

  include Protocol.MakeBaseNode(V)

  type ev = PaxosEvent.t

  type node_data = {
    mutable n : int;
    mutable value : int;
    mutable quorum_promise : int list;
    mutable quorum_accept : int list;
    mutable network_size : int;
    mutable accepting : int;
  }

  type t = (node_data, value) Protocol.template

  let init id links region : (t) =
    {
      id = id;
      region = region;
      links = links;
      state = PaxosBlock.null ();
      data  = {
        n = 0;
        value = 0;
        quorum_promise = [];
        quorum_accept = [];
        network_size = 10;
        accepting = 0;
      }
    }

    let receive_init (node:t) =
      node.data.value <- Random.int(1000);
      node.data.n <- 1;
      PaxosNetwork.send_to_neighbors node.id (Prepare(node.id, node.data.n));
      node

    let receive_prepare (node:t) sender n =
      if (node.data.n < n) then
        begin
          node.data.n <- n;
          node.data.value <- 0;
          PaxosNetwork.send node.id sender (Promise(node.id, n, -1));
          PaxosNetwork.send_to_neighbors node.id (Prepare(sender, n));
        end
      else if (node.data.n > n) then
        PaxosNetwork.send node.id sender (Promise(node.id, node.data.n, node.data.value));
      node

    let receive_promise (node:t) sender n previous_accept =
      if(node.data.accepting < node.data.n && node.data.n = n) then
        begin
          if (previous_accept = -1) then
            begin
              node.data.quorum_promise <- node.data.quorum_promise @ [sender];
              if (List.length node.data.quorum_promise > node.data.network_size / 2) then
                begin
                  node.data.accepting <- n;
                  PaxosNetwork.send_to_neighbors node.id (Accept(sender, n, node.data.value));
                end;
            end;
        end;
          (*TODO*)
      node

    let receive_accept (node:t) sender n value =
      if (node.data.accepting < node.data.n && node.data.n = n) then
        begin
          node.data.accepting <- n;
          node.data.value <- value;
          PaxosNetwork.send node.id sender (Accepted(node.id, n, value));
          PaxosNetwork.send_to_neighbors node.id (Accept(sender, n, value));
        end;
      node
    
    
    let receive_accepted (node:t) sender n value =
      if (node.data.n = node.data.accepting) then
        begin
          node.data.quorum_accept <- node.data.quorum_accept @ [sender];
          if (List.length node.data.quorum_accept > node.data.network_size / 2) then
            begin
              node.data.accepting <- 0;
              node.data.n <- n + 1;
              PaxosNetwork.send node.id node.id (Response(node.id, n, value));
            end;
        end;
      node

    let receive_response (node:t) _ _ =
      node.data.quorum_promise <- [];
      node.data.quorum_accept <- [];
      node.data.value <- Random.int(1000);
      PaxosNetwork.send_to_neighbors node.id (Prepare(node.id, node.data.n));
      node

    let handle (node:t) (event:ev) : t =
      match event with
        | PaxosEvent.Message(_,_,_,_,msg) -> 
          begin
          match msg with
          | Init(_) -> receive_init node
          | Prepare(sender, n) -> receive_prepare node sender n
          | Promise(sender, n, previous_accept) -> receive_promise node sender n previous_accept
          | Accept(sender, n, value) -> receive_accept node sender n  value
          | Accepted(sender, n, value) -> receive_accepted node sender n value
          | Response(sender, n, _) -> receive_response node sender n
          end
        | _ -> node


  let chain_height (node:t) = 
    PaxosBlock.height node.state

end


module PaxosInitializer : (Protocol.Initializer with type node=PaxosNode.t and type ev=PaxosEvent.t) = struct
  type node = PaxosNode.t

  type ev = PaxosEvent.t

  let init _ = 
    [PaxosEvent.Message(0, 1, 0, 0, Init(1))]

end

module PaxosProtocol = Protocol.Make.Blockchain(PaxosEvent)(PaxosQueue)(PaxosBlock)(PaxosTimer)(PaxosNode)(PaxosNode)(PaxosInitializer)(PaxosLogger)(PaxosStatistics)(PaxosNetwork);;
