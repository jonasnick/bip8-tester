# BIP8-Tester

This is an attempt at implementing the logic BIP8, in particular to check whether allowing some `MUST_SIGNAL` blocks to not signal ([PR #1021](https://github.com/bitcoin/bips/pull/1021) has the desired effect.

Running `./tests.sh` should *quickcheck* that the following properties hold for a large number of chains:

- if the chain is valid under `{ lot = true, timeout = T }`, it's valid under `{ lot = false, timeout >= T }`
- if the chain is valid under `{ lot = false, timeout = T }` and the bip8 state is LockedIn or Active, it's valid under `{ lot = true, timeout = T }`

Note that only property 2. wouldn't hold without [PR #1021](https://github.com/bitcoin/bips/pull/1021)).


## threshold.py

Quick & dirty simulation of impact of threshold parameters

```
$ python ./threshold.py
Probability that unupdated node is on >= 2 block invalid fork with 95.0% threshold: 1.22%
Probability that unupdated node is on >= 3 block invalid fork with 95.0% threshold: 0.17%
Probability that unupdated node is on >= 2 block invalid fork with 90.0% threshold: 4.81%
Probability that unupdated node is on >= 3 block invalid fork with 90.0% threshold: 1.28%
Probability that unupdated node is on >= 2 block invalid fork with 85.0% threshold: 10.45%
Probability that unupdated node is on >= 3 block invalid fork with 85.0% threshold: 4.13%
```
