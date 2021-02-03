# BIP8-Tester

This is an attempt at implementing the logic BIP8, in particular to check whether allowing some `MUST_SIGNAL` blocks to not signal ([PR #1021](https://github.com/bitcoin/bips/pull/1021) has the desired effect.

Running `./tests.sh` should *quickcheck* that the following properties hold for a large number of chains:

- if the chain is valid under `{ lot = true, timeout = T }`, it's valid under `{ lot = false, timeout >= T }`
- if the chain is valid under `{ lot = false, timeout = T }` and the bip8 state is LockedIn or Active, it's valid under `{ lot = true, timeout = T }`

Note that only property 2. wouldn't hold without [PR #1021](https://github.com/bitcoin/bips/pull/1021)).
