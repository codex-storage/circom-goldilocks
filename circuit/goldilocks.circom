
pragma circom 2.2.0;

include "field_params.circom";
include "goldilocks_func.circom";
include "misc.circom";

//------------------------------------------------------------------------------

// a type for bit-decomposed numbers
// the invariant to respect is that the value is in the range [0..2^n-1]
//
bus Binary(n) {
  signal val;
  signal bits[n];
}

// the type of Goldilocks field elements.
// the invariant to respect is that the value is in the range [0..P-1]
//
bus Goldilocks() {
  signal val;
}

//------------------------------------------------------------------------------

// n-bit binary decomposition
template ToBinary(n) {
  signal input  inp;
  output Binary(n) out;

  component tobits = ToBits(n);
  tobits.inp <== inp;
  tobits.out ==> out.bits;
  inp        ==> out.val;
}

//------------------------------------------------------------------------------

//
// do a range check for [0,P-1] where `P = 2^64 - 2^32 + 1`
//

template ToGoldilocks() {
  signal input  inp;
  output Goldilocks() out;

  // first we do a range check and bit decomposition to 64 bits

  var nbits = SolinasExpoBig();
  Binary(nbits) bin <== ToBinary(nbits)( inp );

  // compute the low and high 32 bit words

  var sum_lo = 0;
  for(var i=0; i < SolinasExpoSmall(); i++) {
    sum_lo += (2**i) * bin.bits[i];
  }

  var expo_jump = SolinasExpoBig() - SolinasExpoSmall();

  var sum_hi = 0;
  for(var i=0; i < expo_jump; i++) {
    sum_hi += (2**i) * bin.bits[ SolinasExpoSmall() + i ];
  }

  // now, since we have p-1 = 2^64 - 2^32 = 0xff...ff00...00
  //
  // we need to check that IF hi == 2^32-1 THEN lo == 0

  signal iseq <== IsEqual()( sum_hi , 2**expo_jump - 1 );

  iseq * sum_lo === 0;           // either (hi < 2^32-1) OR (lo == 0)

  out.val <== bin.val;
}

//------------------------------------------------------------------------------

// reduce numbers in the range `0 <= x < 2^k * P` modulo P
template ReduceModP(k) {
  signal input inp;
  output Goldilocks() out;

  var P = FieldPrime();

  signal quot <-- inp \ P;
  signal rem  <-- inp % P;

  // check that `quot` is in a `k` bit range!
  component bits = ToBits( k );
  bits.inp <== quot;                             

  inp === rem + P * quot;          // check the multiplication equation
  out <== ToGoldilocks()(rem);     // range check on the output
}

//------------------------------------------------------------------------------

//
// negation in the Goldilocks field
//

template Neg() {
  input  Goldilocks() A;
  output Goldilocks() C;

  signal isz <== IsZero()( A.val );

  C.val <== (1 - isz) * (FieldPrime() - A.val);
}

//--------------------------------------

//
// addition in the Goldilocks field
//

template Add() {
  input  Goldilocks() A,B;
  output Goldilocks() C;

  var P = FieldPrime();

  // A + B = C + P * bit

  var overflow = (A.val + B.val >= P);

  signal quot <-- (overflow ? 1 : 0);
  signal rem  <-- A.val + B.val - quot * P;

  quot*(1-quot) === 0;                   // `quot` must be either zero or one
  A.val + B.val === rem + P * quot;      // check the addition
  C <== ToGoldilocks()(rem);             // range check on C
}

//
// subtraction in the Goldilocks field
//

template Sub() {
  input  Goldilocks() A,B;
  output Goldilocks() C;

  var P = FieldPrime();

  // A - B = C - P * bit

  var overflow = (A.val - B.val < 0);

  signal aquot <-- (overflow ? 1 : 0);         // absolute value of the quotient
  signal rem   <-- A.val - B.val + aquot * P;

  aquot*(1-aquot) === 0;                       // `aquot` must be either zero or one
  A.val - B.val === rem - P * aquot;           // check the subtraction
  C <== ToGoldilocks()(rem);                   // range check on C
}

//------------------------------------------------------------------------------

//
// summation in the Goldilocks field
//

template Sum(k) {
  input  Goldilocks() A[k];
  output Goldilocks() C;

  // sum A[i] = C + P * Q
  // since all A[i] < 2^64, Q will have at most as many bits as `k` have
  // so we can do a simple binary range-check on Q

  var sum = 0;
  for(var i=0; i<k; i++) { sum += A[k].val; }

  C <== ReduceModP( CeilLog2(k) )( sum ); 
}

//------------------------------------------------------------------------------

//
// multiplication in the Goldilocks field
//

template Mul() {
  input  Goldilocks() A,B;
  output Goldilocks() C;

  // A * B = C + P * Q

  C <== ReduceModP( SolinasExpoBig() )( A.val * B.val ); 
}

//
// multiplication of 3 Goldilocks field elements
// as this still fits into 192 < 254 bits, we can do it a bit more efficiently
// than two multiplications.
//

template Mul3() {
  input  Goldilocks() A,B,C;
  output Goldilocks() D;

  // A * B * C = D + P * Q

  signal AB <== A.val * B.val;

  D <== ReduceModP( 2 * SolinasExpoBig() )( AB * C.val ); 
}

//--------------------------------------

// Squaring (this is more interesting in the extension field case)
//
template Sqr() {
  input  Goldilocks() A;
  output Goldilocks() C;
  C <== Mul()( A , A );
}

//------------------------------------------------------------------------------

//
// inversion in the Goldilocks field
// (maybe this could be optimized a bit?)
// 

template Inv() {
  input  Goldilocks() A;
  output Goldilocks() C;

  // guess the result, and range-check it
  signal candidate <-- goldilocks_inv(A.val);
  C <== ToGoldilocks()(candidate);

  // is A = 0?
  signal isz <== IsZero()(A.val);

  // multiply back, and check the equation (but only when A != 0)
  Goldilocks() AC <== Mul()( A , C );
  (1 - isz) * (AC.val - 1) === 0;

  // if A=0, we have to enforce C=0 too
  isz * C.val === 0;
}

//
// division in the Goldilocks field
// (maybe this could be optimized a bit?)
//

template Div() {
  input  Goldilocks() A,B;
  output Goldilocks() C;

  // guess the result, and range-check it
  signal candidate <-- goldilocks_div(A.val, B.val);
  C <== ToGoldilocks()(candidate);

  // is B = 0?
  signal isz <== IsZero()(B.val);
 
  // multiply back, and check the equation (but only when B != 0)
  Goldilocks() BC <== Mul()( B , C );
  (1 - isz) * (BC.val - A.val) === 0;

  // if B=0, we have to enforce C=0 too
  isz * C.val === 0;
}

//------------------------------------------------------------------------------
