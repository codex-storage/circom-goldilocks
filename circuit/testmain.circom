
pragma circom 2.2.0;

include "test_wrapper.circom";

//component main { public [A,B] } = AddWrapper();

component main { public [A] } = InvWrapper();
