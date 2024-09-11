# SimplePoW

This is a simplified implementation of a proof of work protocol in the simulator.

---

Besides the default (defined in [simulator/lib/block.ml](/simulator/lib/block.ml)), blocks contain zero additional data:

```
module BlockContents = struct
  type t = unit
end
```

The messages exchanged are simply blocks:

```
module SimpleMsg : (Simulator.Events.Message with type t = BlockContents.t Simulator.Block.t) = struct 
  type t = BlockContents.t Simulator.Block.t

  ...

end
```

Every required moduled except for **Node** is obtained automatically by calling functors:

```
module SimpleEvent   = Simulator.Events.MakeEvent(SimpleMsg);;
module SimpleQueue   = Simulator.Events.MakeQueue(SimpleEvent);;
module SimpleNetwork = Abstractions.Network.Make(SimpleEvent)(SimpleQueue)(SimpleMsg);;
module SimpleLogger  = Simulator.Logging.Make(SimpleMsg)(SimpleEvent);;
module SimpleTimer   = Abstractions.Timer.Make(SimpleEvent)(SimpleQueue);;
module SimpleBlock   = Simulator.Block.Make(SimpleLogger)(BlockContents);;
module SimplePow     = Abstractions.Pow.Make(SimpleEvent)(SimpleQueue)(SimpleBlock);;
let _ = SimplePow.init_mining_power ();;
```

---

The overall behavior of the **Node** is defined in the ***handle*** and ***process_block*** functions:

```
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
```

To kickstart the simulation (done via the Initializer), a random node is selected to mint a block:

```
module SimpleInitializer : (Abstract.Initializer with type node=SimpleNode.t and type ev=SimpleEvent.t) = struct
  type node = SimpleNode.t

  type ev = SimpleEvent.t

  let init nodes = 
    let index = (Random.int ((Hashtbl.length nodes)-1))+1 in
    [SimpleEvent.MintBlock(index, 0)]
  
end
```

---

From a high level view, the behavior of the protocol is as follows:
- if a **Node** receives a *MintBlock* event, he proceeds to create a new block, and propagate it to its neighbours (through a message)
    - if the node is currently storing the null block at the head of its chain, it means this is the first block in the chain, so it creates the genesis block
    - the node will also start the minting process to extend the new head of chain
- when a **Node** receives a *Message*, it will process the contained block. If the block is valid and the node as not seen that block before
    - it will update its chain
    - propagate the block to its neighbours
    - and if it was currently minting, it will restart the minting process in order to extend the new head of the chain

Note that the simulator offers abstractions for starting/stopping the minting process [simulator/abstractions/pow.ml](simulator/abstractions/pow.ml).

---

Calling the *Protocol.Make* functor with all the modules will produce the final module for the protocol

```
module SimpleProtocol = Protocol.Make(SimpleEvent)(SimpleQueue)(SimpleBlock)(SimpleTimer)(SimpleNode)(SimpleInitializer)(SimpleLogger)(SimpleStatistics);;

```

which the simulator can then run by calling it in [/simulator/bin/main.ml](/simulator/bin/main.ml).