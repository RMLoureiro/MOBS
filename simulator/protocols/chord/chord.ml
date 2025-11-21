open Implementation

module BlockContents = struct
  type t = unit
end

type msg = 
  RebalanceNetwork of int (*to send message to*)
  | SendRebalanceNetwork of int*int
  | Join of int          (* from, new_node *)
  | JoinResponse of int*int      (* from, successor*)
  | UpdateSuccessor of int
  | UpdatePredecessor of int
  | Uft of int*int*int         (* blockID, from *)
  | UpdateLink of int*int

module ChordMsg : (Simulator.Events.Message with type t = msg) = struct 
  type t = msg

  let to_json (msg:t) : string = 
    match msg with
    | RebalanceNetwork(receiver) -> Printf.sprintf "{\"type\":\"RebalanceNetwork\",\"receiver\":\"%d\"}" receiver
    | SendRebalanceNetwork(receiver,_) -> Printf.sprintf "{\"type\":\"SendRebalanceNetwork\",\"receiver\":\"%d\"}" receiver
    | Join(new_node) -> Printf.sprintf "{\"type\":\"Join\",\"new_node\":\"%d\"}" new_node
    | JoinResponse(sender, successor) -> Printf.sprintf "{\"type\":\"JoinResponse\",\"sender\":\"%d\", \"successor\": \"%d\"}" sender successor
    | UpdateSuccessor(sender) -> Printf.sprintf "{\"type\":\"UpdateSuccessor\",\"new_successor\":\"%d\"}" sender
    | UpdatePredecessor(sender) -> Printf.sprintf "{\"type\":\"UpdatePredecessor\",\"new_successor\":\"%d\"}" sender
    | Uft(id,_,ttw) -> Printf.sprintf "{\"type\":\"Uft\",\"block_id\":\"%d\",\"TTW\":\"%d\"}" id ttw
    | UpdateLink(self, successor) -> Printf.sprintf "{\"type\":\"UpdateLink\",\"self\":\"%d\", \"succesor\": \"%d\"}" self successor


  let get_size (msg:t) =
    match msg with
    | RebalanceNetwork (_) -> Simulator.Size.Bit(32)
    | SendRebalanceNetwork (_,_) -> Simulator.Size.Bit(32)
    | Join(_) -> Simulator.Size.Bit(32)
    | JoinResponse(_,_) ->  Simulator.Size.Bit(32)
    | UpdateSuccessor(_) -> Simulator.Size.Bit(32)
    | UpdatePredecessor(_) -> Simulator.Size.Bit(32)
    | Uft(_,_,_) -> Simulator.Size.Bit(32)
    | UpdateLink(_,_) ->  Simulator.Size.Bit(32)

  let processing_time (_:t) =
    0

  let identifier (msg:t) =
    match msg with
    | RebalanceNetwork(receiver) -> receiver
    | SendRebalanceNetwork(receiver, _) -> receiver
    | Join(id) -> id
    | JoinResponse(id,succesor) -> succesor + id
    | UpdateSuccessor(id) -> id
    | UpdatePredecessor(id) -> id
    | Uft(id,_,ttw)  -> id + ttw
    | UpdateLink(a,b) ->  a + b
    
end


module ChordEvent   = Simulator.Events.MakeEvent(ChordMsg);;
module ChordQueue   = Simulator.Events.MakeQueue(ChordEvent);;
module ChordNetwork = Abstractions.Network.Make(ChordEvent)(ChordQueue)(ChordMsg);;
module ChordLogger  = Simulator.Logging.Make(ChordMsg)(ChordEvent);;
module ChordTimer   = Abstractions.Timer.Make(ChordEvent)(ChordQueue);;
module ChordBlock   = Simulator.Block.Make(ChordLogger)(BlockContents)(Simulator.Block.BaseRewards);;

module MBPTimeArg = struct
  let label = "median-block-propagation-time"
  let use_intervals = false
  let format = 1
end

module ChordStatistics = Simulator.Statistics.Make.Median(MBPTimeArg);;

module ChordNode : (Protocol.BlockchainNode with type ev=ChordEvent.t and type value=ChordBlock.block) = struct

  type value = ChordBlock.block

  module V = struct
    type v = value
  end

  include Protocol.MakeBaseNode(V)

  type ev = ChordEvent.t

  type node_data = {
    mutable successor : int;
    mutable predecessor : int;
    mutable finger_list : int list;
    mutable init: bool;
  }

  type t = (node_data, value) Protocol.template

  let modulo x y =
    let result = x mod y in
    if result >= 0 then result
    else result + y

  let init id links region : (t) =
    {
      id = id;
      region = region;
      links = links;
      state = ChordBlock.null ();
      data  = {
        successor = -10;
        predecessor = -10;
        finger_list = [];
        init = false;
      }
    }

    let distance target start =
      modulo((target + 1) - (start + 1)) 20

    let process_join (node:t) new_node =
      if(new_node <> node.data.predecessor) then
        begin
          if ((distance node.id new_node < distance node.id node.data.predecessor) || (node.data.predecessor = - 20)) then begin
            ChordNetwork.send node.id new_node (JoinResponse(node.id, node.data.predecessor));
            node.data.predecessor <- new_node;
          end
          else if (node.data.predecessor >= 0) then
            begin
              ChordNetwork.send node.id node.data.predecessor (Join(new_node));
            end
          else ChordNetwork.send_to_neighbors node.id (RebalanceNetwork(new_node));

        end
      else ChordNetwork.send node.id new_node (UpdateSuccessor(node.id));
      node

    let process_join_response (node:t) sender predecessor =
      ChordNetwork.send node.id node.id (UpdateSuccessor(sender));

      if(predecessor >= 0) then
        begin
          ChordNetwork.send node.id predecessor (UpdateSuccessor(node.id));
        end;
      node

    let update_successor (node:t) sender =
      if ((distance sender node.id < distance node.data.successor node.id) || node.data.successor = -10) then ChordNetwork.send node.id node.id (UpdateLink(node.id, sender));
      node

    let update_predecessor (node:t) sender =
      if (sender <> node.id) then node.data.predecessor <- sender;
      node

    let process_uft (node:t) sender ttw =
      begin
        if List.exists (fun x -> x = sender) node.data.finger_list then begin
          if ttw > 0 then
            assert false;
            (* send_to_neighbours node (Uft(id, node.id, ttw - 1)) *)
        end
        else
          node.data.finger_list <- node.data.finger_list @ [sender];
          node
      end

    let rebalance_network (node:t) target =
      begin
        if (node.data.init = false) then
          ChordNetwork.send_to_neighbors node.id (SendRebalanceNetwork(node.id, 5));
          node.data.init <- true;
        if (target <> node.id) then 
          begin
            if(((distance target node.id < distance node.data.successor node.id) || (node.data.successor = - 20)) && (node.data.successor <> target)) then
              begin
                ChordNetwork.send node.id target (Join(node.id));
                node
              end
            else
              node
          end
        else
          node
      end

    let send_rebalance_network(node:t) target ttw =
      ChordNetwork.send_to_neighbors node.id (RebalanceNetwork(target));
      if(ttw > 0) then
        begin
          ChordNetwork.send_to_neighbors node.id (SendRebalanceNetwork(target, (ttw-1)));
        end
      else if((distance node.data.successor node.id > 1) || (node.data.successor = -10)) then
        ChordNetwork.send_to_neighbors node.id (SendRebalanceNetwork(node.id, 5));
      node

    let update_link(node:t) successor =
      node.data.successor <- successor;
      ChordNetwork.send node.id node.data.successor (UpdatePredecessor(node.id));
      node


    let handle (node:t) (event:ev) : t =
      match event with
        | ChordEvent.Message(_,_,_,_,msg) -> 
          begin
          match msg with
          | RebalanceNetwork(target) -> rebalance_network node target
          | SendRebalanceNetwork(target,ttw) -> send_rebalance_network node target ttw
          | Join(new_node)  -> process_join node new_node
          | JoinResponse(sender ,successor) -> process_join_response node sender successor
          | UpdateSuccessor(sender) -> update_successor node sender
          | UpdatePredecessor(sender) -> update_predecessor node sender
          | Uft(_,sender,ttw)  -> process_uft node sender ttw
          | UpdateLink(_, successor) -> update_link node successor
          end
        | _ -> node

    
  let chain_height (node:t) = 
    ChordBlock.height node.state

end


module ChordInitializer : (Protocol.Initializer with type node=ChordNode.t and type ev=ChordEvent.t) = struct
  type node = ChordNode.t

  type ev = ChordEvent.t

  let init nodes = 
    let index = (Random.int ((Hashtbl.length nodes))) in
    [ChordEvent.Message(0, 1, 0, 0, SendRebalanceNetwork(index, 5))]
  
end

module ChordProtocol = Protocol.Make.Blockchain(ChordEvent)(ChordQueue)(ChordBlock)(ChordTimer)(ChordNode)(ChordNode)(ChordInitializer)(ChordLogger)(ChordStatistics)(ChordNetwork);;
