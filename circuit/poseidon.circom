
// 
// Plonky2's Poseidon hash instance over the Goldilocks field
// 

pragma circom 2.2.0;

include "goldilocks.circom";
include "constants.circom";

//------------------------------------------------------------------------------

template SBOX() {
  input  Goldilocks() inp;
  output Goldilocks() out;

  Goldilocks() x3;

  x3  <== Mul3()( inp , inp , inp );
  out <== Mul3()( x3  , x3  , inp );
}

//------------------------------------------------------------------------------

// numbers in the range `0 <= x < 2*P`
bus AtMostTwoP() {
  signal val;
}

// If `x < P` then of course `x < 2*P` too
template Embed() {
  input  Goldilocks() inp;
  output AtMostTwoP() out;
  out.val <== inp.val;
}

//------------------------------------------------------------------------------

template ExternalNonlinearLayer(round) {
  input  Goldilocks() inp[12];
  output Goldilocks() out[12];

  var rc[12] = poseidon_round_constants(round);
  Goldilocks RC [12];
  Goldilocks tmp[12];

  for(var i=0; i<12; i++) {
    RC[i].val <== rc[i];                       // we know that the constants are in the corrrect range
    tmp[i]    <== Add()( inp[i] , RC[i] );     // this range check could be probably optimized away with a more general Mul3
    out[i]    <== SBOX()( tmp[i] );
  }

//  log("\nAFTER external nonlinear layer | round=",round);
//  for(var i=0; i<12; i++) { 
//    log("out[",i,"] = ",out[i].val);
//  }

}

//------------------------------------------------------------------------------

//
// NOTE: we don't reduce the output, as it will be multiplied by the MDS matrix next,
// and it's enough to reduce after that
//
template InternalNonlinearLayer(round) {
  input  Goldilocks() inp[12];
  output AtMostTwoP() out[12];

  var rc[12] = poseidon_round_constants(round);

  Goldilocks RC0;  
  RC0.val <== rc[0];       // we know that the constants are in the corrrect range

  Goldilocks tmp  <== Add()( inp[0] , RC0 );
  Goldilocks sbox <== SBOX()( tmp );
  out[0]          <== Embed()( sbox );

  for(var i=1; i<12; i++) {
    out[i].val <== inp[i].val + rc[i];
  }
}

//------------------------------------------------------------------------------

//
// NOTE: the sum of the coefficients of a row of the circulant matrix is
// 
// > sum [17, 15, 41, 16,  2, 28, 13, 13, 39, 18, 34, 20] = 256
//
// So if each input is less than `B`, then the (unreduced) linear combination is less than `256*B`
//
// The only exception is the first row, becuase the diagonal 
// matrix `[8,0,0,...,0]` is added to the circulant matrix
//
template LinearDiffusionLayer(extra) {
  input  AtMostTwoP   inp[12];
  output Goldilocks() out[12];

  var coeffs[12] = circular_matrix_coeffs();

  var ys[12];
  for(var k=0; k<12; k++) {
    var y = 0;
    for(var j=0; j<12; j++) {
      var diag = ((k==0)&&(j==0)) ? 8 : 0;
      y += inp[j].val * ( diag + coeffs[ (j-k+12) % 12 ] );
    }
    ys[k] = y;
  }  

  out[0] <== ReduceModP( 9+extra )( ys[0] );
  for(var k=1; k<12; k++) { out[k] <== ReduceModP( 8+extra )( ys[k] ); }
}

//------------------------------------------------------------------------------

template ExternalRound(round) {
  input  Goldilocks() inp[12];
  output Goldilocks() out[12];
  AtMostTwoP() tmp[12];
  Goldilocks() gls[12];

  gls <== ExternalNonlinearLayer(round)( inp );
  for(var k=0; k<12; k++) { tmp[k] <== Embed()( gls[k] ); }
  out <== LinearDiffusionLayer(0)( tmp );

}

template InternalRound(round) {
  input  Goldilocks() inp[12];
  output Goldilocks() out[12];
  AtMostTwoP() tmp[12];

  tmp <== InternalNonlinearLayer(round)( inp );
  out <== LinearDiffusionLayer(1)( tmp );
}

//------------------------------------------------------------------------------

template Permutation() {
  input  Goldilocks() inp[12];
  output Goldilocks() out[12];
  Goldilocks() aux[31][12];

  aux[0] <== inp;

  for(var i=0 ; i<4 ; i++) { aux[i+1] <== ExternalRound(i)( aux[i] ); }
  for(var i=4 ; i<26; i++) { aux[i+1] <== InternalRound(i)( aux[i] ); }
  for(var i=26; i<30; i++) { aux[i+1] <== ExternalRound(i)( aux[i] ); }

  aux[30] ==> out;
}

//------------------------------------------------------------------------------

template CheckPermutationKAT() {
  signal input dummy;                // hack around circom
  Goldilocks() inp[12];
  Goldilocks() out[12];
  
  for(var i=0; i<12; i++) { inp[i].val <== i; }
  out <== Permutation()( inp );

  var kat[12] = poseidon_kat();

  log("\nfinal permutation output:");
  for(var i=0; i<12; i++) { 
    log("out[",i,"] = ",out[i].val, " vs. expected = ",kat[i]);
  }

  for(var i=0; i<12; i++) { 
    out[i].val === kat[i];
  }  
}

