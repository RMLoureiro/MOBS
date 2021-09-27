# Tenderbake

The main references for this implementation are:
- [A look ahead to Tenderbake](https://blog.nomadic-labs.com/a-look-ahead-to-tenderbake.html)
- [tenderbake-simulator](https://gitlab.com/nomadic-labs/tenderbake-simulator/-/tree/master/src/tenderbake)
- [Tenderbake - A Solution to Dynamic Repeated Consensus for Blockchains](https://arxiv.org/pdf/2001.11965.pdf)

For an introduction to the protocol, read [A look ahead to Tenderbake](https://blog.nomadic-labs.com/a-look-ahead-to-tenderbake.html).

At an high level, this is an adaptation of the implementation in [tenderbake-simulator](https://gitlab.com/nomadic-labs/tenderbake-simulator/-/tree/master/src/tenderbake) for our simulator, as the overall logic is extremely similar.

The main **goal** was to demonstrate the added functionalities that this simulator offers, and not to fundamentaly change an already correct and validated implementation of the protocol.

---

The main differences between this implementation and the one referenced above are:
- the round is kept in the node's state, instead of being stored and updated in the blocks
    - the block only stores the round when it was created
- messages for future rounds are stored in a buffer to be processed later, as opposed to being ignored
- proposer and committee members are selected through stake-based sortition
- more network customization - for example, the network is **not** fully linked (although it can be)
- simplified hashes and signatures
- mutable records are leveraged to store and update the nodes' state
- can easily extend the statistics module to gather arbitrary statistics about the execution of the protocol
    - at the moment, the only one being extracted is the average time to decide on a block
- can leverage the **GUI** to run batch simulations, to analyze the gathered statistics and to monitor the overall behavior of the protocol


---

An user can, for example, use the GUI to see the messages being exchanged between nodes and the state of each node, to help with detecting and debugging unexpected behaviors or unexpected results in the extracted metrics.

![](/images/tenderbake_map.png)

