module BitcoinMsg : (Simulator.Events.Message with type t = Simulator.Block.t) = struct 
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

module BitcoinEvent   = Simulator.Events.MakeEvent(BitcoinMsg);;
module BitcoinQueue   = Simulator.Events.MakeQueue(BitcoinEvent);;
module BitcoinNetwork = Abstractions.Network.Make(BitcoinEvent)(BitcoinQueue)(BitcoinMsg);;
module BitcoinLogger  = Simulator.Logging.Make(BitcoinMsg)(BitcoinEvent);;
module BitcoinBlock   = Simulator.Block.Make(BitcoinLogger);;
module BitcoinPow     = Abstractions.Pow.Make(BitcoinEvent)(BitcoinQueue)(BitcoinBlock);;
let _ = BitcoinPow.init_mining_power ();;

module BitcoinNode : (Protocol.Node with type ev=BitcoinEvent.t and type id=int and type block=Simulator.Block.t) = struct
  type block = Simulator.Block.t
  
  type id = int

  type ev = BitcoinEvent.t

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
    List.iter (fun neighbour -> BitcoinNetwork.send node.id neighbour msg) node.links

  let add_to_chain node block =
    node.chain <- Some block

  let process_block node block =
    let is_valid_block b = 
      match node.chain with
      | None -> true
      | Some(blk) -> 
        if BitcoinBlock.total_difficulty b > BitcoinBlock.total_difficulty blk then true else false
    in
    let already_seen = (List.exists (fun x -> x=(BitcoinBlock.id block)) node.received_blocks) in
    if is_valid_block block && not already_seen then begin
      add_to_chain node block;
      node.received_blocks <- node.received_blocks @ [BitcoinBlock.id block];
      send_to_neighbours node block;
      BitcoinPow.stop_minting node.id;
      BitcoinPow.start_minting node.id block;
    end;
    node

  let handle (node:t) (event:ev) : t =
    match event with
    | BitcoinEvent.MintBlock(_,_) ->
      begin
      match node.chain with
      | None -> process_block node (BitcoinBlock.genesis_pow node.id (BitcoinPow.total_mining_power ())) 
      | Some(parent) -> process_block node (BitcoinBlock.create node.id parent)
      end
    | BitcoinEvent.Message(_,_,_,block_msg) -> process_block node block_msg
    | _ -> node

  let compare n1 n2 =
    if n1.id < n2.id then -1 else if n1.id > n2.id then 1 else 0

  let chain_head n =
    n.chain

  let chain_height node = 
    match node.chain with
    | None -> 0
    | Some(blk) -> BitcoinBlock.height blk

end

module BitcoinInitializer : (Protocol.Initializer with type node=BitcoinNode.t and type ev=BitcoinEvent.t) = struct
  type node = BitcoinNode.t

  type ev = BitcoinEvent.t

  let init nodes = 
    let index = (Random.int ((Hashtbl.length nodes)-1))+1 in
    [BitcoinEvent.MintBlock(index, 0)]
  
end

module BitcoinProtocol = Protocol.Make(BitcoinEvent)(BitcoinQueue)(BitcoinBlock)(BitcoinNode)(BitcoinInitializer)(BitcoinLogger);;










