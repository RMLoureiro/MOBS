open Implementation

module BlockContents = struct
  type t = unit
end

module SimpleMsg : (Simulator.Events.Message with type t = BlockContents.t Simulator.Block.t) = struct 
  type t = BlockContents.t Simulator.Block.t

  let to_json (blk:t) : string = 
    Printf.sprintf "{\"block_id\":\"%d\"}" (Simulator.Block.id blk)

  let get_size (_:t) =
    Simulator.Size.Kilobyte(534)

  let processing_time (_:t) =
    2

  let identifier (msg:t) =
    Simulator.Block.id msg

end

module SimpleEvent   = Simulator.Events.MakeEvent(SimpleMsg);;
module SimpleQueue   = Simulator.Events.MakeQueue(SimpleEvent);;
module SimpleNetwork = Abstractions.Network.Make(SimpleEvent)(SimpleQueue)(SimpleMsg);;
module SimpleLogger  = Simulator.Logging.Make(SimpleMsg)(SimpleEvent);;
module SimpleTimer   = Abstractions.Timer.Make(SimpleEvent)(SimpleQueue);;
module SimpleBlock   = Simulator.Block.Make(SimpleLogger)(BlockContents);;
module SimplePow     = Abstractions.Pow.Make(SimpleEvent)(SimpleQueue)(SimpleBlock);;
let _ = SimplePow.init_mining_power ();;

module SimpleNode : (Protocol.Node with type ev=SimpleEvent.t and type value=SimpleBlock.block) = struct
  type value = SimpleBlock.block
  
  module V = struct
    type v = value
  end

  include Abstract.MakeBaseNode(V)

  type ev = SimpleEvent.t

  type node_data = {
    mutable received_blocks : int list;
  }

  type t = (node_data, value) Abstract.template

  let init id links region : (t) =
    {
      id = id;
      region = region;
      links = links;
      state = SimpleBlock.null ();
      data = {
        received_blocks = [];
      }
    }
  
  let send_to_neighbours (node:t) msg =
    Array.iter (fun neighbour -> SimpleNetwork.send node.id neighbour msg) node.links

  let add_to_chain (node:t) block =
    node.state <- block

  let process_block (node:t) block =
    let is_valid_block b = 
      if node.state = SimpleBlock.null () then
        true
      else
        if SimpleBlock.total_difficulty b > SimpleBlock.total_difficulty node.state then true else false
    in
    let already_seen = (List.exists (fun x -> x=(SimpleBlock.id block)) node.data.received_blocks) in
    if is_valid_block block && not already_seen then begin
      add_to_chain node block;
      node.data.received_blocks <- node.data.received_blocks @ [SimpleBlock.id block];
      send_to_neighbours node block;
      SimplePow.stop_minting node.id;
      SimplePow.start_minting node.id block;
    end;
    node

  let handle (node:t) (event:ev) : t =
    match event with
    | SimpleEvent.MintBlock(_,_) ->
      begin
      if node.state = SimpleBlock.null () then
        process_block node (SimpleBlock.genesis_pow node.id (SimplePow.total_mining_power ()) ()) 
      else
        process_block node (SimpleBlock.create node.id node.state ())
      end
    | SimpleEvent.Message(_,_,_,block_msg) -> process_block node block_msg
    | _ -> node

  (* this function is the same in every blockchain-specific node, 
    just changing the prefix of the protocol (AlgorandBlock, BitcoinBlock, SimpleBlock, _Block...) *)
  let chain_height (node:t) = 
    SimpleBlock.height node.state

end

module SimpleInitializer : (Abstract.Initializer with type node=SimpleNode.t and type ev=SimpleEvent.t) = struct
  type node = SimpleNode.t

  type ev = SimpleEvent.t

  let init nodes = 
    let index = (Random.int ((Hashtbl.length nodes)-1))+1 in
    [SimpleEvent.MintBlock(index, 0)]
  
end

module SimpleStatistics : (Protocol.Statistics with type ev = SimpleEvent.t and type value = SimpleBlock.block) = struct

  type ev = SimpleEvent.t
  type value = SimpleBlock.block

  let consensus_reached _ _ =
    ()

  let process _ =
    ()

  let get () =
    "{}"

end

module SimpleProtocol = Protocol.Make(SimpleEvent)(SimpleQueue)(SimpleBlock)(SimpleTimer)(SimpleNode)(SimpleInitializer)(SimpleLogger)(SimpleStatistics);;










