module SimpleMsg : (Simulator.Events.Message with type t = Simulator.Block.t) = struct 
  type t = Simulator.Block.t

  let to_json (blk:t) : string =
    String.concat "" ["{\"block_id\":\"";string_of_int (Simulator.Block.id blk);"\"}"]

  let get_size (_:t) =
    4280000

  let processing_time (_:t) =
    2

  let identifier (msg:t) =
    Simulator.Block.id msg

end

module SimpleEvent   = Simulator.Events.MakeEvent(SimpleMsg);;
module SimpleQueue   = Simulator.Events.MakeQueue(SimpleEvent);;
module SimpleNetwork = Abstractions.Network.Make(SimpleEvent)(SimpleQueue)(SimpleMsg);;
module SimpleLogger  = Simulator.Logging.Make(SimpleMsg)(SimpleEvent);;
module SimpleBlock   = Simulator.Block.Make(SimpleLogger);;
module SimplePow     = Abstractions.Pow.Make(SimpleEvent)(SimpleQueue)(SimpleBlock);;
let _ = SimplePow.init_mining_power ();;

module SimpleNode : (Protocol.Node with type ev=SimpleEvent.t and type value=Simulator.Block.t) = struct
  type value = Simulator.Block.t
  
  module V = struct
    type v = value
  end

  module Unique:(Simulator.Unique.Unique with type v = V.v) = Simulator.Unique.Make(V)

  type id = int

  type ev = SimpleEvent.t

  type t = {
    id     : id;
    region : Abstractions.Network.region;
    links  : Abstractions.Network.links;
    mutable received_blocks : id list;
    mutable state  : Simulator.Block.t;
  }

  let init id links region =
    {
      id = id;
      region = region;
      links = links;
      received_blocks = [];
      state = SimpleBlock.null;
    }
  
  let send_to_neighbours node msg =
    List.iter (fun neighbour -> SimpleNetwork.send node.id neighbour msg) node.links

  let add_to_chain node block =
    node.state <- block

  let process_block node block =
    let is_valid_block b = 
      if node.state = SimpleBlock.null then
        true
      else
        if SimpleBlock.total_difficulty b > SimpleBlock.total_difficulty node.state then true else false
    in
    let already_seen = (List.exists (fun x -> x=(SimpleBlock.id block)) node.received_blocks) in
    if is_valid_block block && not already_seen then begin
      add_to_chain node block;
      node.received_blocks <- node.received_blocks @ [SimpleBlock.id block];
      send_to_neighbours node block;
      SimplePow.stop_minting node.id;
      SimplePow.start_minting node.id block;
    end;
    node

  let handle (node:t) (event:ev) : t =
    match event with
    | SimpleEvent.MintBlock(_,_) ->
      begin
      if node.state = SimpleBlock.null then
        process_block node (SimpleBlock.genesis_pow node.id (SimplePow.total_mining_power ())) 
      else
        process_block node (SimpleBlock.create node.id node.state)
      end
    | SimpleEvent.Message(_,_,_,block_msg) -> process_block node block_msg
    | _ -> node

  (* this function is the same in every blockchain node, 
    just changing the prefix of the protocol (AlgorandBlock, BitcoinBlock, SimpleBlock, _Block...) *)
  let chain_height node = 
    SimpleBlock.height node.state

  (* The following four functions are the same for every node object *)
  let compare n1 n2 =
    if n1.id < n2.id then -1 else if n1.id > n2.id then 1 else 0
  
  let state n =
    n.state

  let state_id n =
    Unique.id n.state

  let parameters () =
    Parameters.Protocol.get ()

end

module SimpleInitializer : (Abstract.Initializer with type node=SimpleNode.t and type ev=SimpleEvent.t) = struct
  type node = SimpleNode.t

  type ev = SimpleEvent.t

  let init nodes = 
    let index = (Random.int ((Hashtbl.length nodes)-1))+1 in
    [SimpleEvent.MintBlock(index, 0)]
  
end

module SimpleStatistics : (Protocol.Statistics with type ev = SimpleEvent.t and type value = Simulator.Block.t) = struct

  type ev = SimpleEvent.t
  type value = Simulator.Block.t

  let consensus_reached _ _ =
    ()

  let process _ =
    ()

  let get () =
    "{}"

end

module SimpleProtocol = Protocol.Make(SimpleEvent)(SimpleQueue)(SimpleBlock)(SimpleNode)(SimpleInitializer)(SimpleLogger)(SimpleStatistics);;










