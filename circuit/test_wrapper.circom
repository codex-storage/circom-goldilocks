
// wrappers around the Goldilocks templates, so that input and output are classic
// signals, not Bus-es. The testing framework does not handle Bus inputs/outputs yet

pragma circom 2.2.0;

include "goldilocks.circom";

//------------------------------------------------------------------------------

template ToGoldilocksWrapper() {
  signal input  inp;
  signal output out;

  Goldilocks() goldi <== ToGoldilocks()(inp);
  goldi.val ==> out;
}

//------------------------------------------------------------------------------

template NegWrapper() {
  signal input  A;
  signal output C;

  Goldilocks() A1; A1.val <== A;
  Goldilocks() C1; 

  C1 <== Neg()( A1 );
  C1.val ==> C;
}

template AddWrapper() {
  signal input  A,B;
  signal output C;

  Goldilocks() A1; A1.val <== A;
  Goldilocks() B1; B1.val <== B;
  Goldilocks() C1; 

  C1 <== Add()( A1 , B1 );
  C1.val ==> C;

//  log("A = ",A, " | B = ",B, " | C = ",C, " | expected = ", goldilocks_add(A,B));
}

template SubWrapper() {
  signal input  A,B;
  signal output C;

  Goldilocks() A1; A1.val <== A;
  Goldilocks() B1; B1.val <== B;
  Goldilocks() C1; 

  C1 <== Sub()( A1 , B1 );
  C1.val ==> C;
}

//------------------------------------------------------------------------------

template MulWrapper() {
  signal input  A,B;
  signal output C;

  Goldilocks() A1; A1.val <== A;
  Goldilocks() B1; B1.val <== B;
  Goldilocks() C1; 

  C1 <== Mul()( A1 , B1 );
  C1.val ==> C;
}

template InvWrapper() {
  signal input  A;
  signal output C;

  Goldilocks() A1; A1.val <== A;
  Goldilocks() C1; 

  C1 <== Inv()( A1 );
  C1.val ==> C;

  log("A = ",A, " | C = ",C, " | expected = ", goldilocks_inv(A));
}

template DivWrapper() {
  signal input  A,B;
  signal output C;

  Goldilocks() A1; A1.val <== A;
  Goldilocks() B1; B1.val <== B;
  Goldilocks() C1; 

  C1 <== Div()( A1 , B1 );
  C1.val ==> C;
}

//------------------------------------------------------------------------------
