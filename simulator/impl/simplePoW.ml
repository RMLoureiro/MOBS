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

module SimpleNode : (Protocol.Node with type ev=SimpleEvent.t and type id=int and type block=Simulator.Block.t) = struct
  type block = Simulator.Block.t
  
  type id = int

  type ev = SimpleEvent.t

  type t = {
    id     : id;
    region : Abstractions.Network.region;
    links  : Abstractions.Network.links;
    mutable received_blocks : id list;
    mutable chain  : Simulator.Block.t option;
  }

  let init id links region =
    {
      id = id;
      region = region;
      links = links;
      received_blocks = [];
      chain = None;
    }
  
  let send_to_neighbours node msg =
    List.iter (fun neighbour -> SimpleNetwork.send node.id neighbour msg) node.links

  let add_to_chain node block =
    node.chain <- Some block

  let process_block node block =
    let is_valid_block b = 
      match node.chain with
      | None -> true
      | Some(blk) -> 
        if SimpleBlock.total_difficulty b > SimpleBlock.total_difficulty blk then true else false
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
      match node.chain with
      | None -> process_block node (SimpleBlock.genesis_pow node.id (SimplePow.total_mining_power ())) 
      | Some(parent) -> process_block node (SimpleBlock.create node.id parent)
      end
    | SimpleEvent.Message(_,_,_,block_msg) -> process_block node block_msg
    | _ -> node

  let compare n1 n2 =
    if n1.id < n2.id then -1 else if n1.id > n2.id then 1 else 0

  let chain_head n =
    n.chain

  let chain_height node = 
    match node.chain with
    | None -> 0
    | Some(blk) -> SimpleBlock.height blk

  let parameters () =
    "{}"

end

module SimpleInitializer : (Protocol.Initializer with type node=SimpleNode.t and type ev=SimpleEvent.t) = struct
  type node = SimpleNode.t

  type ev = SimpleEvent.t

  let init nodes = 
    let index = (Random.int ((Hashtbl.length nodes)-1))+1 in
    [SimpleEvent.MintBlock(index, 0)]
  
end

module SimpleStatistics : (Protocol.Statistics with type ev = SimpleEvent.t) = struct

  type ev = SimpleEvent.t

  let consensus_reached _ _ =
    ()

  let process _ =
    ()

  let get () =
    "{}"

end

module SimpleProtocol = Protocol.Make(SimpleEvent)(SimpleQueue)(SimpleBlock)(SimpleNode)(SimpleInitializer)(SimpleLogger)(SimpleStatistics);;










