# Assumptions

During this task, the following assumptions have been made:
1. We use an account-based blockchain (not UTxO-based).
2. In the future, forking and node-to-node interaction will be implemented.

---

# Architecture Description

The central component is the `state` package.  
`BlocksTree` and `BlocksTreeState` allow retrieving the state of a specific chain element at any block by applying and unapplying blocks. This is achieved as follows: `BlocksTree` maintains parent-child relationships (currently, only a linear structure is supported), while `BlockTreeState` provides a mechanism for applying (and, in the future, unapplying) blocks to a given state.
This approach enables, for example, retrieving the ledger state at a specific block for transaction verification.

The `Ledger` contains ledger data, such as account balances and nonces. The ledger state can be accessed at any point in the chain.

The `Mempool` contains transactions that could be included in a block. The current implementation allows only **consistent** transactions in the memory pool, meaning it is not possible to add a transaction if it conflicts with an existing transaction in the pool. This will be improved in the future to allow "updated" transactions (e.g., transactions with higher fees) and to prevent DoS attacks on the memory pool, as consistency checks take time.

`BlockProduction`, located in the `forging` package, is responsible for block minting. It retrieves data from `BlockPacker` and passes it to the `Forger` for signing. It is also responsible for adopting newly forged blocks. Currently, `BlockPacker` is quite simple, but in a production environment, packing transactions into a limited block could take additional time. For this reason, `BlockPacker` could run in the background to perform its work without slowing down `BlockProduction`.

Cryptographic primitives such as keys and signatures are represented in Base64 string format for easier comparison, encoding, and decoding. However, in a production environment, a byte array format should be used to save space.

For user interaction, a REST server is used, providing a simple and effective way to interact with the node. Currently, only two REST API endpoints are available: `transaction`, for adding new transactions, and `mint`, for packing all transactions in the memory pool into a block and minting it. Additional requests, such as retrieving chain information, will be added in the future.

For testing, a `ChainGenerator` has been implemented to generate consistent blockchain data (blocks with transactions).

---

# TODO
This project has a long list of pending tasks and is not production-ready. Some key improvements include:
1. The private key is hardcoded in a `.scala` file but should be loaded from a configuration file.
2. Cryptographic primitives (i.e., the type of keys used for signing and verification) are semi-hardcoded. In many places, they are passed implicitly, while in others (such as in Genesis), they are hardcoded. Instead, they should be dynamically configurable (e.g., for testing purposes).
3. There is no persistence support, meaning the blockchain always starts from scratch. However, all stateful components support an initial state, so persistence could be easily added.
4. Multi-node support is currently absent—there is no consensus algorithm or peer-to-peer interaction.
5. ... and more.

---

# Launch
To start the REST server, use:
```sh
sbt "runMain blockchain.Main"
```

---

# Test
One of the most interesting tests is **"Transaction after 20 blocks is accepted"** in `IntegrationTest`. This test simulates adding consistent data to the blockchain for 20 blocks and forging them. `IntegrationTest` also includes checks for incorrect transactions.