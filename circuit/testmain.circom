
pragma circom 2.2.0;

include "test_wrapper.circom";
include "test_wrapper_ext.circom";

include "poseidon.circom";

// component main { public [A,B] } = AddWrapper();
// component main { public [A]   } = InvWrapper();

// component main { public [dummy] } = CheckPermutationKAT();

// component main { public [A,B] } = MulExtWrapper();
// component main { public [A]   } = SqrExtWrapper();
component main { public [A,B] } = DivExtWrapper();
