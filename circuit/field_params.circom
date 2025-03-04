
//
// for soundness testing we use the `r1cs-solver` testing framework, which 
// currently uses the ambient prime 65537 (instead of BN254); with realistic
// primes it would be too slow (needs fast square root for example) 
//
// thus, as the second best option, we want to test the soundness of the field 
// emulation by testing it on the "tiny-goldilocks" prime `P = 2^8 - 2^4 + 1`
//
// hence we try and make all this "parametric" over the Solinas primes `P(a,b) := 2^b - 2^a + 1`
//
// unfortunately, circom does not support global constants, so we need
// to do some hacking to hack around this limitation.
//


pragma circom 2.2.0;

//------------------------------------------------------------------------------

// function SolinasExpoBig()   { return 64; }
// function SolinasExpoSmall() { return 32; }

function SolinasExpoBig()   { return 8; }
function SolinasExpoSmall() { return 4; }

function FieldPrime()       { return (2**SolinasExpoBig() - 2**SolinasExpoSmall() + 1); }

//------------------------------------------------------------------------------
