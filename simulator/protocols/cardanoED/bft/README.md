# Cardano (Ouroboros-BFT)

The implementation of the protocol is based on the paper [Ouroboros-BFT: A Simple Fault Tolerant Consensus Protocol](https://eprint.iacr.org/2018/1049.pdf).

---

At an high level, the behavior of the protocol is as follows:
- Initialization
  - If the node starts at a later point in the protocol
    - Request chain data from network
    - Set the state to reflect the received maxvalid chain
  - If the node starts at the begining of the protocol
    - Obtain the genesis block, and store it in the state
- Chain Extension
  - Upon receiving a longer valid chain, with the proper leader signature, update its state to match the new chain
  - At the beggining of each slot, each node checks if `(i-1 = j-1 mod n; where i is the node's id, j is the slot, and n is the number of nodes)`. If so, the respective node will create and diffuse a new block, along with its signature