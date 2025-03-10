
//
// the quadratic field extension `F[X] / (X^2 - 7)` over Goldilocks
//
// note: `X^2 - 7` is also irreducible over the field of size `2^8 - 2^4 + 1`
// so can use it for testing too.
//

pragma circom 2.2.0;

include "goldilocks.circom";
// include "misc.circom";

//------------------------------------------------------------------------------
//
// the type of quadratic field extension elements.
//

bus GoldilocksExt() {
  Goldilocks() real;
  Goldilocks() imag;
}

template GoldilocksToExt() {
  input  Goldilocks()    inp;
  output GoldilocksExt() out;
  out.real     <== inp;
  out.imag.val <== 0;
}

//------------------------------------------------------------------------------

template NegExt() {
  input  GoldilocksExt() A;
  output GoldilocksExt() C;

  C.real <== Neg()( A.real );
  C.imag <== Neg()( A.imag );
}

template AddExt() {
  input  GoldilocksExt() A,B;
  output GoldilocksExt() C;

  C.real <== Add()( A.real , B.real );
  C.imag <== Add()( A.imag , B.imag );
}

template SubExt() {
  input  GoldilocksExt() A,B;
  output GoldilocksExt() C;

  C.real <== Sub()( A.real , B.real );
  C.imag <== Sub()( A.imag , B.imag );
}

//------------------------------------------------------------------------------

template SevenTimesProduct() {
  input  Goldilocks() inp1,inp2;
  output Goldilocks() out;
  out <== ReduceModP( 3 + SolinasExpoBig() )( 7 * inp1.val * inp2.val ); 
}

template SevenTimesSquare() {
  input  Goldilocks() inp;
  output Goldilocks() out;
  out <== ReduceModP( 3 + SolinasExpoBig() )( 7 * inp.val * inp.val ); 
}

//--------------------------------------

template MulExt() {
  input  GoldilocksExt() A,B;
  output GoldilocksExt() C;

  Goldilocks() RR, IR, RI, IIx7;
  RR   <== Mul()( A.real , B.real );
  IR   <== Mul()( A.imag , B.real );
  RI   <== Mul()( A.real , B.imag );
  IIx7 <== SevenTimesProduct()( A.imag , B.imag );

  C.real <== Add()( RR , IIx7 );
  C.imag <== Add()( IR , RI   );
}

template SqrExt() {
  input  GoldilocksExt() A;
  output GoldilocksExt() C;

  Goldilocks() RR, IR, IIx7;
  RR   <== Sqr()( A.real );
  IR   <== Mul()( A.imag , A.real );
  IIx7 <== SevenTimesSquare()( A.imag );

  C.real <== Add()( RR , IIx7 );
  C.imag <== Add()( IR , IR   );

  // log("\nsqrExt");
  // log( "A = (", A.real.val, " , ", A.imag.val, ")" );
  // log( "C = (", C.real.val, " , ", C.imag.val, ")" );
  // log( "RR   = ", RR.val   );
  // log( "IR   = ", IR.val   );
  // log( "IRx7 = ", IIx7.val );
}

//------------------------------------------------------------------------------

template InvExt() {
  input  GoldilocksExt() A;
  output GoldilocksExt() C;

  Goldilocks() seven, RR, IIx7;
  seven.val <== 7;
  RR   <== Sqr()( A.real );
  IIx7 <== SevenTimesSquare()( A.imag );

  Goldilocks() denom     <== Sub()( RR , IIx7 );
  Goldilocks() mult      <== Inv()( denom     );
  Goldilocks() minusImag <== Neg()( A.imag );

  C.real <== Mul()( A.real    , mult );
  C.imag <== Mul()( minusImag , mult );
}

template DivExt() {
  input  GoldilocksExt() A,B;
  output GoldilocksExt() C;

  GoldilocksExt() invB <== InvExt()( B );
  C <== MulExt()( A , invB );

  log("\ndivExt");
  log( "A = (", A.real.val, " , ", A.imag.val, ")" );
  log( "B = (", B.real.val, " , ", B.imag.val, ")" );
  log( "C = (", C.real.val, " , ", C.imag.val, ")" );

}

//------------------------------------------------------------------------------
