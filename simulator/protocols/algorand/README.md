# Algorand

The bulk of the implementation is based on the paper [Algorand Agreement: Super Fast and Partition Resilient Byzantine Agreement](https://eprint.iacr.org/2018/377.pdf), as our focus is on the consensus portion of the protocol.

The blockchain-specific details are based on the paper [Algorand: Scaling Byzantine Agreements for Cryptocurrencies](https://people.csail.mit.edu/nickolai/papers/gilad-algorand-eprint.pdf).

Note that cryptography operations, namely node signatures and validation of said signatures, are simplified as the focus of the simulation is on the consensus portion of the protocol.

---

There are 5 kinds of messages:

```
type alg_msg = 
    Priority of round * period * step * priority * signature 
  | Proposal of round * period * step * (certificate Simulator.Block.t) * signature
  | SoftVote of round * period * step * block_id * signature
  | CertVote of round * period * step * block_id * signature  
  | NextVote of round * period * step * block_id * signature  
and certificate = {
  mutable certificate: signature list;
  prev_block_certificate: signature list;
}
and signature = Signature of int
```

Blocks have additional data, namely an associated certificate:

```
module BlockContents = struct
  type t = certificate
end
```

Most required modules are obtained through functors, including abstractions for stake-based sortition:

```
module AlgorandEvent   = Simulator.Events.MakeEvent(AlgorandMsg);;
module AlgorandQueue   = Simulator.Events.MakeQueue(AlgorandEvent);;
module AlgorandTimer   = Abstractions.Timer.Make(AlgorandEvent)(AlgorandQueue);;
module AlgorandNetwork = Abstractions.Network.Make(AlgorandEvent)(AlgorandQueue)(AlgorandMsg);;
module AlgorandLogger  = Simulator.Logging.Make(AlgorandMsg)(AlgorandEvent);;
module AlgorandBlock   = Simulator.Block.Make(AlgorandLogger)(BlockContents);;
module AlgorandPoS     = Abstractions.Pos.Make(AlgorandLogger)(AlgorandBlock);;
```

To start the simulation, the **Initializer** produces a timeout event for each node, labeled *step*.

```
module AlgorandInitializer : (Abstract.Initializer with type node=AlgorandNode.t and type ev=AlgorandEvent.t) = struct
  type node = AlgorandNode.t

  type ev = AlgorandEvent.t

  let init nodes = 
    let evs = ref [] in
    Hashtbl.iter (fun nid _ -> evs := !evs @ [AlgorandEvent.Timeout(nid, 0, "step")]) nodes;
    !evs
  
end
```

---

The overall behavior of the protocol is as follows:
- When a node receives a message, it will
    - discard it, if the message is for an older round or period
    - temporarily ignore it (add it to a "pending" queue) if it is for a future round or period
    - otherwise, it will store it and check if any of the following conditions has been met
        - if the **halting condition** is verified, a block is added to the chain and the node moves to the next round
        - if it received the **proposal** associated with the **highest priority** seen, the node executes **step 2** of the protocol
        - if it observed a **majority of softvotes**, the node executes **step 3** of the protocol
- When a node times out, it will increment the step of the protocol, and execute the corresponding step
    - before running each step, the node will check if it belongs to the committee for the current **round** and **period** of the protocol
    - if the node belongs to the committee, it will behave as described below. Otherwise, it will act as a spectator, simply validating, storing and counting votes, without actually participating in the voting process

**Protocol Steps** (**rounds** involve one or more **periods**, which are composed of **steps**):

0. **Value Proposal**
    - the node will check whether or not he was selected to be a **proposer** for that **round** and **period**. If so, it will create a new block and will propagate two messages across the network, a **Proposal** and a **Priority**.
    - regardless of whether or not a node is selected as proposer, it will now wait a predefined amount of time before running the next step of the protocol, to ensure most nodes receive the **Priority** message.

1. **Highest Priority**
    - the node will check if it has already received the **Proposal** associated with the highest **Priority** seen
        - if so, it will immediatly run the next step
        - otherwise, it will wait a predefined amount of time before timing out, or until the **Proposal** is received
2. **Filtering Step** 
    - the node will **SoftVote** for the **Proposal** with the highest associated **Priority**, if it exists. Otherwise, it will **SoftVote** for the empty block.
    - the node will run the next step after timing out, or after receiving a majority of **SoftVotes**
3. **Certifying Step** 
    - if the node has seen a majority of **SoftVotes** for a non-empty block, it will **CertVote** for that block
    - the next step will be executed if the node times out (if a majority of **CertVotes** is seen, it will move to the next round, without needing to execute the remaining steps)
4. **Finishing Step1** 
    - if the node has **CertVoted** for a block during the round, it will **NextVote** for that same block. Otherwise, it will **NextVote** for the empty block
    - the next step will be executed when a timeout occurs
5. **Finishing Step2** 
    - very similar to the previous step, except the node will keep periodically executing this step until either
        - the **halting condition** is reached
        - the node sees a majority of **NextVotes**, upon which the **period** is incremented (but the round stays the same)

**Halting Condition**:
- When a node observes a majority of **Certvotes** for a given block in a round, it means consensus was reached on that block and it can be added to the chain, along with the **majority of Certvotes** it has seen, which form the block's **Certificate**.
- If the halting condition is verified (at any point during the round), the block is added to the chain and the node moves to the next round

---

Finally, the implementation of the statistics module produces metrics for the time taken to complete each step of the protocol:
- **Block Proposal**
- **Majority Softvotes**
- **Majority Certvotes**

With these metrics, and leveraging the [GUI](/visualizer) we can easily obtain graphics with the same information shown in the Algorand paper, allowing us to reach the same conclusion on, for example, the effects of varying the block size - the size of a block only impacts the duration of the **Block Proposal** step of the protocol, and has no influence in the remaining steps.

![](/images/algorand_params.png)

![](/images/algorand_graph.png)