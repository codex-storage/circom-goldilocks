
pragma circom 2.2.0;

include "test_wrapper.circom";
include "poseidon.circom";

// component main { public [A,B] } = AddWrapper();
// component main { public [A]   } = InvWrapper();

component main { public [dummy] } = CheckPermutationKAT();
