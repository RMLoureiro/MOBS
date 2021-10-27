# Cardano (Ouroboros-Praos)

The implementation of the protocol is based on the paper [Ouroboros-Praos: An adaptively-secure, semi-synchronous proof-of-stake blockchain](https://eprint.iacr.org/2017/573.pdf).

At an high level, the behavior of the protocol is as follows:
- Initialization
  - If the node starts at a later point in the protocol
    - Request chain data from network
    - Set the state to reflect the received maxvalid chain
  - If the node starts at the begining of the protocol
    - Obtain the genesis block, and store it in the state
    - Obtain the `nonce` that seeds leader election, and store it in the state
- Chain Extension
  - If a new epoch has started, with epoch>=2, then
    - set the most recent block from the last epoch to be used for leader selection
  - Collect valid chains received for the current slot, and verify that
    - the stakeholder who created the block is indeed the slot's leader
    - the signature is valid (the proposal was not tampered with)
    - and update the current state to the maxvalid chain observed
  - Check if we are the slot leader for the current slot. If so
    - generate a new block, and sign it
    - add the block to the chain, updating its state
    - propagate the new up-to-date chain

