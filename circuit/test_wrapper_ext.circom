
// wrappers around the Goldilocks templates, so that input and output are classic
// signals, not Bus-es. The testing framework does not handle Bus inputs/outputs yet

pragma circom 2.2.0;

include "goldilocks.circom";
include "goldilocks_ext.circom";

//------------------------------------------------------------------------------

template NegExtWrapper() {
  signal input  A[2];
  signal output C[2];

  GoldilocksExt() A1; A1.real.val <== A[0]; A1.imag.val <== A[1];

  GoldilocksExt() C1 <== NegExt()( A1 );

  C1.real.val ==> C[0];
  C1.imag.val ==> C[1];
}

template AddExtWrapper() {
  signal input  A[2],B[2];
  signal output C[2];

  GoldilocksExt() A1; A1.real.val <== A[0]; A1.imag.val <== A[1];
  GoldilocksExt() B1; B1.real.val <== B[0]; B1.imag.val <== B[1];

  GoldilocksExt() C1 <== AddExt()( A1 , B1 );

  C1.real.val ==> C[0];
  C1.imag.val ==> C[1];
}

template SubExtWrapper() {
  signal input  A[2],B[2];
  signal output C[2];

  GoldilocksExt() A1; A1.real.val <== A[0]; A1.imag.val <== A[1];
  GoldilocksExt() B1; B1.real.val <== B[0]; B1.imag.val <== B[1];

  GoldilocksExt() C1 <== SubExt()( A1 , B1 );

  C1.real.val ==> C[0];
  C1.imag.val ==> C[1];
}

//------------------------------------------------------------------------------

template SqrExtWrapper() {
  signal input  A[2];
  signal output C[2];

  GoldilocksExt() A1; A1.real.val <== A[0]; A1.imag.val <== A[1];

  GoldilocksExt() C1 <== SqrExt()( A1 );

  C1.real.val ==> C[0];
  C1.imag.val ==> C[1];
}

template MulExtWrapper() {
  signal input  A[2],B[2];
  signal output C[2];

  GoldilocksExt() A1; A1.real.val <== A[0]; A1.imag.val <== A[1];
  GoldilocksExt() B1; B1.real.val <== B[0]; B1.imag.val <== B[1];
  
  GoldilocksExt() C1 <== MulExt()( A1 , B1 );

  C1.real.val ==> C[0];
  C1.imag.val ==> C[1];
}

//------------------------------------------------------------------------------

template InvExtWrapper() {
  signal input  A[2];
  signal output C[2];

  GoldilocksExt() A1; A1.real.val <== A[0]; A1.imag.val <== A[1];

  GoldilocksExt() C1 <== InvExt()( A1 );

  C1.real.val ==> C[0];
  C1.imag.val ==> C[1];
}

template DivExtWrapper() {
  signal input  A[2],B[2];
  signal output C[2];

  GoldilocksExt() A1; A1.real.val <== A[0]; A1.imag.val <== A[1];
  GoldilocksExt() B1; B1.real.val <== B[0]; B1.imag.val <== B[1];

  GoldilocksExt() C1 <== DivExt()( A1 , B1 );

  C1.real.val ==> C[0];
  C1.imag.val ==> C[1];
}

//------------------------------------------------------------------------------
