
Goldilocks field emulation in `circom`
--------------------------------------

Experimenting with Goldilocks field emulation (the native field being the BN254 scalar field).

The "Goldilocks" prime is `P = 2^64 - 2^32 + 1`, and the corresponding
prime field is used for example by the [Plonky2](https://github.com/0xPolygonZero/plonky2/)
proof system, which is our primary motivation here.

We want to do computations in the Goldilocks field in a `circom` circuit 
instantiated over say the BN254 curve's scalar field (a 254 bit prime field).


### Basic approach

We are in a relatively simple situation, as the field we want to emulate is
much smaller than the native field we are working inside. In particular, 
even the product of 3 Goldilocks elements fit inside a BN254 element without
any danger of overflowing.

The critical part is thus that we need range checks for the range `[0..P-1]`.

To start with, we can do a simple bit-decomposition based range check for the range `[0..2^64-1]`.

Then for example we could do another such range check for `x + 2^32 - 1` instead of `x`,
and that would be sufficient; however, this doubles the number of range checks.

Instead, we can exploit the special form of the prime `P`: Observe, that in binary
decomposition, `P - 1` looks like this:

        P-1  =  2^64 - 2^32  =  0x FFFF FFFF 0000 0000 

This means, that if we already know that `x` fits into 64 bits, then it's enough
to check that IF the top 32 bits are all 1, THEN the bottom 32 bits are all 0
(if the top bits are anything else, then obviously we are already in the desired range).

This is relatively easy to do in `circom`.


### Type safety in `circom`

In recent versions of `circom`, we can use so called "buses" and "tags" to
enforce invariants. 

For example, we can define a Goldilocks "bus" (read: type);
a safe casting function from arbitray signals into Goldilocks signal, which checks
that the input is in the range `[0..P-1]`; and then write all operations such
that they expect their inputs have Goldilocks type (that is, already checked),
and they themselves enforce that their output is also reduced modulo P (and
has the right type to show it).

This approach (which is very standard in normal statically typed programming languages)
makes using these sub-circuits _much_ safer.


### Soundness testing

We have the [`r1cs-solver`](https://github.com/faulhornlabs/r1cs-solver) soundness 
testing tool at our disposal, which can, for any given concrete inputs, try to solve 
the equations specialized to those inputs.

In general this has a good chance of succeeding for small circuits, so we
can actually _prove_ that the circuit behaves as expected, at least for _particular inputs_.

Unfortunately, this tool currently only supports very small prime fields (for
example, `P' = 65537`). The reason for this is mainly performance: Both performance
of "normal" operations like addition and multiplication, and probably more importantly,
we also need efficient square roots (as the tool solves systems of quadratic equations).
With such a small field, we can simply store a table of square roots in memory. 
With a ~256 bit field, this would be impossible, and a square root algorithm
would be most probably way too slow.

What we can still do though, is to make the operations we want to test generic over 
[Solinas primes](https://en.wikipedia.org/wiki/Solinas_prime)
of the form `P = 2^a - 2^b + 1` with `a > b > 0`.

For example `2^8 - 2^4 + 1 = 241` is a prime. We can probably do 
_exhaustive_ testing over this small field, checking the _soundness_ over
every single possible inputs for the basic operations, which would give us
a very high confidence that there are no bugs in the implementation.

Furthermore, only inversion and division needs an actual prime modulus; 
the rest of the operations can be tested with any modulus.

