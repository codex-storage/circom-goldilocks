#!/bin/bash

ORIG=`pwd`
MAIN="testmain"
INPUT="input.json"

cd build

circom ../${MAIN}.circom --r1cs --wasm

#echo '{ "dummy": 666 }' >${INPUT}
#echo '{ "A": [66,111] }' >${INPUT}
echo '{ "A": [57,137] , "B": [66,111] }' >${INPUT}

cd ${MAIN}_js

node generate_witness.js ${MAIN}.wasm ../${INPUT} witness.wtns

cd ${ORIG}