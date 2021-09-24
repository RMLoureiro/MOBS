# Bitcoin (PoW)

The Bitcoin implementation is a re-implementation of the one provided by [SimBlock](https://github.com/dsg-titech/simblock). In reality, as shown by the authors, by changing different parameters in [default-parameters.json](/simulator/default-parameters.json) this implementation can simulate different PoW protocols, such as Bitcoin, DogeCoin and LiteCoin.

The purpose of the reimplementation is to demonstrate that it is easier to implement protocols in this simulator, leading to less code written, better code structure, and more analysis capabilities.

---

There are 3 types of messages:

```
type msg = 
  Block of BlockContents.t Simulator.Block.t
  | Inv of int*int
  | Rec of int*int
```

By implementing the statistics module, the following metrics are being extracted from the simulation:
- *avegrage block propagation time*
- *median block propagation time*

To start the simulation, one **Node** is selected to mint the genesis block.

```
module BitcoinInitializer : (Abstract.Initializer with type node=BitcoinNode.t and type ev=BitcoinEvent.t) = struct
  type node = BitcoinNode.t

  type ev = BitcoinEvent.t

  let init nodes = 
    let index = (Random.int ((Hashtbl.length nodes)-1))+1 in
    [BitcoinEvent.MintBlock(index, 0)]
  
end
```

---

The overall behavior of the protocol is defined in the **handle** function:

```
let handle (node:t) (event:ev) : t =
    match event with
    | BitcoinEvent.MintBlock(_,_) ->
      begin
        if node.state = BitcoinBlock.null () then
          process_block node (BitcoinBlock.genesis_pow node.id (BitcoinPow.total_mining_power ()) ()) 
        else
          process_block node (BitcoinBlock.create node.id node.state ())
      end
    | BitcoinEvent.Message(_,_,_,msg) -> 
      begin
      match msg with
      | Block(b) -> process_block node b
      | Inv(id,sender)  -> process_inv node id sender
      | Rec(id,sender)  -> process_rec node id sender
      end
    | BitcoinEvent.Timeout(_,_,"message_sent") -> send_next_block node
    | _ -> node
```

The behavior is as follows:
- If a node finishes minting a new block, it will propagate that block among its neighbours
- If a node receives a block that it as not seen yet, it will send an **Inv** message to its neighbours, announcing that it possesses that block
- If a node receives an **Inv** message, and it hasn't seen the block whose hash is referred in the message, it will respond with a **Rec** message, asking to receive that block
- If a node receives a **Rec** message, it will respond by sending the corresponding block
- Any time a node receives a valid block that extends the longest chain, it will update its chain and restart its minting process, to extend the new head of the chain


--- 

Note that the simulator offers abstractions for starting/stopping the minting process [simulator/abstractions/pow.ml](simulator/abstractions/pow.ml).


Also note that this implementation leverages one functionality of the simulator, which produces a **Timeout** event labelled *message_sent* every time a node sends a large message. This allows us to easily model the fact that a node shouldn't be able to use its full bandwidth to send multiple blocks simultaneously.
