open Implementation

let slot_duration = 3000;

module BlockContents = struct
  type t = unit
end

type msg = 
  InitClient of int (*sender*)
  | Request of int * int(*sender value*)
  | PrePrepare of int * int * int * int (*sender n value*)
  | Prepare of int * int * int * int * int
  | Commit of int * int * int * int * int
  | Reply of int * int * int * int
  | Accept of int * int
  | ViewChange of int * int
  | NewView of int * int
  | ApplyNewView of int * int

module PbftMsg : (Simulator.Events.Message with type t = msg) = struct 
  type t = msg

  let to_json (msg:t) : string = 
    match msg with
    | InitClient(receiver) ->  Printf.sprintf "{\"type\":\"InitClient\", \"node\":\"%d\"}" receiver
    | Request(receiver, value) ->  Printf.sprintf "{\"type\":\"Request\", \"node\":\"%d\", \"Value\":\"%d\" }" receiver value
    | PrePrepare(client, n, view, value) ->  Printf.sprintf "{\"type\":\"PrePrepare\", \"Client\":\"%d\", \"N\": \"%d\", \"View\":\"%d\", \"Value\": \"%d\"}" client n view value
    | Prepare(client, sender, n, view, value) ->  Printf.sprintf "{\"type\":\"Prepare\", \"Client\":\"%d\", \"Client\":\"%d\", \"N\": \"%d\", \"View\":\"%d\", \"Value\": \"%d\"}" client sender n view value
    | Commit(client, sender, n, view, value) ->  Printf.sprintf "{\"type\":\"Commit\", \"Client\":\"%d\", \"Client\":\"%d\", \"N\": \"%d\", \"View\":\"%d\", \"Value\": \"%d\"}" client sender n view value
    | Reply(sender, n, view, value) ->  Printf.sprintf "{\"type\":\"Reply\", \"Client\":\"%d\", \"N\": \"%d\", \"View\":\"%d\", \"Value\": \"%d\"}" sender n view value
    | Accept(sender, value) ->  Printf.sprintf "{\"type\":\"Accept\", \"node\":\"%d\", \"Value\": \"%d\"}" sender value
    | ViewChange(sender, view) ->  Printf.sprintf "{\"type\":\"ViewChange\", \"node\":\"%d\", \"View\": \"%d\"}" sender view
    | NewView(sender, view) ->  Printf.sprintf "{\"type\":\"NewView\", \"node\":\"%d\", \"View\": \"%d\"}" sender view
    | ApplyNewView(sender, view) ->  Printf.sprintf "{\"type\":\"ApplyNewView\", \"node\":\"%d\", \"View\": \"%d\"}" sender view

  let get_size (msg:t) =
    match msg with
    | InitClient (_) -> Simulator.Size.Bit(32)
    | Request (_,_) -> Simulator.Size.Bit(32)
    | PrePrepare (_,_,_,_) -> Simulator.Size.Bit(32)
    | Prepare (_,_,_,_,_) -> Simulator.Size.Bit(32)
    | Commit (_,_,_,_,_) -> Simulator.Size.Bit(32)
    | Reply (_,_,_,_) -> Simulator.Size.Bit(32)
    | Accept (_,_) -> Simulator.Size.Bit(32)
    | ViewChange (_,_) -> Simulator.Size.Bit(32)
    | NewView (_,_) -> Simulator.Size.Bit(32)
    | ApplyNewView (_,_) -> Simulator.Size.Bit(32)

  let processing_time (_:t) =
    0

  let identifier (msg:t) =
    match msg with
    | InitClient(sender) -> sender
    | Request(sender, value) -> sender * value
    | PrePrepare(sender, n, view,_) -> sender * n * view
    | Prepare(client, sender, n, view, value) -> client * sender * n * view * value
    | Commit(client, sender, n, view, value) -> client * sender * n * view * value + 1
    | Reply(sender, n, view, value) ->  sender * n * view * value
    | Accept(sender, value) -> sender * value + 1
    | ViewChange(sender, value) -> sender * value + 3
    | NewView(sender, view) -> sender * view + 5
    | ApplyNewView(sender, view) -> sender * view + 11
end


module PbftEvent   = Simulator.Events.MakeEvent(PbftMsg);;
module PbftQueue   = Simulator.Events.MakeQueue(PbftEvent);;
module PbftNetwork = Abstractions.Network.Make(PbftEvent)(PbftQueue)(PbftMsg);;
module PbftLogger  = Simulator.Logging.Make(PbftMsg)(PbftEvent);;
module PbftTimer   = Abstractions.Timer.Make(PbftEvent)(PbftQueue);;
module PbftBlock   = Simulator.Block.Make(PbftLogger)(BlockContents)(Simulator.Block.BaseRewards);;

module MBPTimeArg = struct
  let label = "median-block-propagation-time"
  let use_intervals = false
  let format = 1
end

module PbftStatistics = Simulator.Statistics.Make.Median(MBPTimeArg);;

module PbftNode : (Protocol.BlockchainNode with type ev=PbftEvent.t and type value=PbftBlock.block) = struct

  type value = PbftBlock.block

  module V = struct
    type v = value
  end

  include Protocol.MakeBaseNode(V)

  type ev = PbftEvent.t

  type node_data = {
    mutable view : int;
    mutable value : int;
    mutable view_change : int;
    mutable quorum_prepare : int list;
    mutable quorum_commit : int list;
    mutable quorum_accept : int list;
    mutable quorum_view_change : int list;
    mutable network_size : int;
    mutable client : bool;
    mutable n : int;
    mutable accepting : bool;
  }

  type t = (node_data, value) Protocol.template

  let init id links region : (t) =
    {
      id = id;
      region = region;
      links = links;
      state = PbftBlock.null ();
      data  = {
        view = 0;
        view_change = 0;
        value = 0;
        quorum_prepare = [];
        quorum_commit = [];
        quorum_accept = [];
        quorum_view_change = [];
        network_size = !Parameters.General.num_nodes;
        client = false;
        n = 0;
        accepting = false;
      }
    }

    let receive_init_client (node:t) =
      node.data.client <- true;
      node.data.view <- node.data.view + 1;
      node.data.value <- Random.int(100000);
      PbftTimer.set node.id slot_duration "new_view";
      PbftNetwork.send node.id node.id (Request(node.id, node.data.value));
      node

    let receive_request (node:t) =
      node.data.n <- node.data.n + 1 + Random.int(10);
      node.data.quorum_accept <- [];
      node.data.accepting <- true;
      PbftNetwork.gossip node.id (PrePrepare(node.id, node.data.n, node.data.view, node.data.value));
      node

    let receive_reply (node:t) sender n view value =
      if (node.data.n = n && node.data.view = view && node.data.value = value) then
        begin
          PbftTimer.cancel node.id "new_view";
          if (not (List.mem sender node.data.quorum_accept)) then
            begin
              node.data.quorum_accept <- node.data.quorum_accept @ [sender];
              if ((List.length node.data.quorum_accept) > (node.data.network_size / 3) && node.data.accepting) then
                begin
                  node.data.accepting <- false;
                  PbftNetwork.send node.id node.id (Accept(node.id, value));
                end;
            end;
        end;
      node

    let receive_accept (node:t) _ _ =
      node.data.value <- Random.int(100000);
      PbftTimer.set node.id slot_duration "new_view";
      PbftNetwork.send node.id node.id (Request(node.id, node.data.value));
      node

    let receive_pre_prepare (node:t) sender n view value =
      if ((node.data.n < n && node.data.view <= view) ||
          not(node.data.n = n && node.data.view = view && node.data.value <> value)) then
            begin
              node.data.n <- n;
              node.data.value <- value;
              node.data.view <- view;
              node.data.quorum_prepare <- [];
              node.data.quorum_accept <- [];
              node.data.quorum_commit <- [];
              PbftNetwork.gossip node.id (Prepare(sender, node.id, node.data.n, node.data.view, node.data.value));
            end;
      node

    let receive_prepare (node:t) sender client n view value =
      if (node.data.n = n && node.data.view = view && node.data.value = value) then
        begin
            if(not (List.mem sender node.data.quorum_prepare)) then
            begin
              node.data.quorum_prepare <- node.data.quorum_prepare @ [sender];
              if ((List.length node.data.quorum_prepare) > (node.data.network_size / 3)) then
                begin
                  node.data.quorum_commit <- [node.id];
                  PbftNetwork.gossip node.id (Commit(client, node.id, node.data.n, node.data.view, node.data.value));
                end;
            end;
        end;
      node

    let receive_commit (node:t) sender client n view value =
      if (node.data.n = n && node.data.view = view && node.data.value = value) then
        if(not (List.mem sender node.data.quorum_commit)) then
          begin
            node.data.quorum_commit <- node.data.quorum_commit @ [sender];
            if ((List.length node.data.quorum_commit) > (node.data.network_size / 3)) then
              begin
                PbftNetwork.send node.id client (Reply(node.id, node.data.n, node.data.view, node.data.value));
              end;
        end;
      node

    let receive_view_change_trigger (node:t) =
      node.data.view_change <- node.data.view + 1;
      PbftNetwork.gossip node.id (ViewChange(node.id, node.data.view_change));
      node

    let receive_view_change (node:t) client view =
      if (node.data.view < view && node.data.view_change <> view) then
        begin
          node.data.view_change <- view;
          PbftNetwork.send node.id client (NewView(node.id, node.data.view_change));
        end;
      node

    let receive_new_view (node:t) sender view =
      if (node.data.view_change = view && not (List.mem sender node.data.quorum_view_change)) then
        begin
          node.data.quorum_view_change <- node.data.quorum_view_change @ [sender];
          if ((List.length node.data.quorum_view_change) > (node.data.network_size / 3)) then
            begin
              node.data.view_change <- 0;
              node.data.view <- view;
              node.data.quorum_view_change <- [];
              PbftTimer.set node.id slot_duration "new_view";
              PbftNetwork.gossip node.id (ApplyNewView(node.id, node.data.view));
            end;
        end;
      node

    let receive_apply_new_view (node:t) _ view =
      if (node.data.view_change = node.data.view || node.data.view < view) then
        begin
          node.data.view_change <- 0;
          node.data.view <- view;
        end;
      node

    let handle (node:t) (event:ev) : t =
      match event with
        | PbftEvent.Message(_,_,_,_,msg) -> 
          begin
            match msg with
              | InitClient(_) -> receive_init_client node
              | Request(_,_) -> receive_request node
              | PrePrepare(sender, n, view, value) -> receive_pre_prepare node sender n view value
              | Prepare(client, sender, n, view, value) -> receive_prepare node sender client n view value
              | Commit(client, sender, n, view, value) -> receive_commit node sender client n view value
              | Reply(sender, n, view, value) -> receive_reply node sender n view value
              | Accept(sender, value) -> receive_accept node sender value
              | ViewChange(sender, view) -> receive_view_change node sender view
              | NewView(sender, view) -> receive_new_view node sender view
              | ApplyNewView(sender, view) -> receive_apply_new_view node sender view
          end
        | PbftEvent.Timeout(_,_,label) ->
          begin
            match label with
            | "new_view" -> receive_view_change_trigger node
            | _ -> node
            end
        | _ -> node

  let chain_height (node:t) = 
    PbftBlock.height node.state

end


module PbftInitializer : (Protocol.Initializer with type node=PbftNode.t and type ev=PbftEvent.t) = struct
  type node = PbftNode.t

  type ev = PbftEvent.t

  let init _ =  
    Random.self_init ();
    [PbftEvent.Message(0, 1, 0, 0, InitClient(1));]

end

module PbftProtocol = Protocol.Make.Blockchain(PbftEvent)(PbftQueue)(PbftBlock)(PbftTimer)(PbftNode)(PbftNode)(PbftInitializer)(PbftLogger)(PbftStatistics)(PbftNetwork);;
