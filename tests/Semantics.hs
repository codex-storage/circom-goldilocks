
module Semantics where

--------------------------------------------------------------------------------

import Prelude hiding (div)

--------------------------------------------------------------------------------

type F = Int

fieldPrime :: Int
fieldPrime = 2^8 - 2^4 + 1

modp :: Int -> F
modp x = mod x fieldPrime

--------------------------------------------------------------------------------

neg :: F -> F
neg 0 = 0
neg x = fieldPrime - x

add :: F -> F -> F
add x y = modp (x + y)

sub :: F -> F -> F
sub x y = modp (x - y)

mul :: F -> F -> F
mul x y = modp (x * y)

sqr :: F -> F 
sqr x = mul x x

mul3 :: F -> F -> F -> F
mul3 x y z = mul (mul x y) z

pow :: F -> Int -> F
pow x0 expo
  | expo < 0  = error "pow: negative exponent"
  | expo == 0 = 1
  | otherwise = go 1 x0 expo
  where
    go acc s 0 = acc
    go acc s e = case divMod e 2 of
      (e', 0) -> go      acc    (sqr s) e'
      (e' ,1) -> go (mul acc s) (sqr s) e'

inv :: F -> F
inv x = pow x (fieldPrime - 2)      

div :: F -> F -> F
div x y = mul x (inv y)

--------------------------------------------------------------------------------
