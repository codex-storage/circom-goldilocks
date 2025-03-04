
//
// compile time functions calculating in the Goldilocks field
//
// all functions assume that the inputs are in the range [0..P-1]
//

pragma circom 2.2.0;

include "field_params.circom";

//------------------------------------------------------------------------------

function goldilocks_neg(x) {
  return ( (x > 0) ? (FieldPrime() - x) : 0 );
}

function goldilocks_add(x,y) {
  return (x+y) % FieldPrime();
}

function goldilocks_sub(x,y) {
  return (x-y+FieldPrime()) % FieldPrime();      // note: % behaves the wrong way in most mainstream languages
}

function goldilocks_mul(x,y) {
  return (x*y) % FieldPrime();
}

function goldilocks_pow(x0,expo) {
  var acc = 1;
  var s = x0;
  var e = expo;
  while( e>0 ) {
    acc = ((e & 1) == 1) ? goldilocks_mul(acc, s) : acc;
    e = e >> 1;
    s = goldilocks_mul(s,s);
  }
  return acc;
}

function goldilocks_inv(x) {
  return goldilocks_pow( x , FieldPrime() - 2 );
}

function goldilocks_div(x,y ) {
  return goldilocks_mul( x , goldilocks_inv(y) );
}

//------------------------------------------------------------------------------
