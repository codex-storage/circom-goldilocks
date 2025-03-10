#!/bin/bash

ORIG=`pwd`
MAIN="testmain"
INPUT="dummy.json"

cd build

circom ../${MAIN}.circom --r1cs --wasm

echo '{ "dummy": 666 }' >${INPUT}

cd ${MAIN}_js

node generate_witness.js ${MAIN}.wasm ../${INPUT} witness.wtns

cd ${ORIG}