#!/bin/bash
set -eu
set -o pipefail

printf "
quickCheck $ forAll genHeights prop_getChainAtHeight\n
quickCheck prop_state\n
test_state
quickCheck (withMaxSuccess 1000 prop_IsValid)
quickCheck (withMaxSuccess 1000 prop_IsValid2)
" | ghci ./Chain.hs
echo "Tests succeeded"


