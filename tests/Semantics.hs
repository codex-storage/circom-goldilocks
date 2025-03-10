
{-# LANGUAGE BlockArguments #-}
module Semantics where

--------------------------------------------------------------------------------

import Prelude hiding (div)
import Control.Monad
import System.Random

--------------------------------------------------------------------------------

type F    = Int
type FExt = (F,F)

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

negExt :: FExt -> FExt
negExt (r,i) = (neg r, neg i)

addExt :: FExt -> FExt -> FExt
addExt (r1,i1) (r2,i2) = (add r1 r2, add i1 i2)

subExt :: FExt -> FExt -> FExt
subExt (r1,i1) (r2,i2) = (sub r1 r2, sub i1 i2)

mulExt :: FExt -> FExt -> FExt
mulExt (r1,i1) (r2,i2) = (r3,i3) where
  r3 = (mul r1 r2) `add` (mul3 7 i1 i2) 
  i3 = (mul r1 i2) `add` (mul i1 r2)

sqrExt :: FExt -> FExt 
sqrExt x = mulExt x x

powExt :: FExt -> Int -> FExt
powExt x0 expo
  | expo < 0  = error "pow: negative exponent"
  | expo == 0 = (1,0)
  | otherwise = go (1,0) x0 expo
  where
    go acc s 0 = acc
    go acc s e = case divMod e 2 of
      (e', 0) -> go         acc    (sqrExt s) e'
      (e' ,1) -> go (mulExt acc s) (sqrExt s) e'

invExt :: FExt -> FExt
invExt (r,i) = (r `mul` z , neg i `mul` z) where 
  denom = (sqr r) `sub` (mul3 7 i i)
  z = inv denom

divExt :: FExt -> FExt -> FExt
divExt x y = mulExt x (invExt y)

--------------------------------------------------------------------------------
-- some quick & dirty testing

data Prop a 
  = Prop1 String (a -> Bool)
  | Prop2 String (a -> a -> Bool)
  | Prop3 String (a -> a -> a -> Bool)

propName :: Prop a -> String
propName prop = case prop of
  Prop1 name _ -> name
  Prop2 name _ -> name
  Prop3 name _ -> name

some_properties :: [Prop F]
some_properties = 
  [ Prop2 "add-sub" \x y   -> add (sub x y) y == x 
  , Prop2 "add-neg" \x y   -> sub x y == add x (neg y)
  , Prop1 "inv-mul" \x     -> x == 0 || inv x `mul` x == 1
  , Prop1 "inv-pow" \x     -> inv x == pow x (fieldPrime - 2)
  , Prop2 "mul-div" \x y   -> y == 0 || mul (div x y) y == x 
  , Prop3 "mul-add" \x y z -> mul (add x y) z == mul x z `add` mul y z 
  , Prop1 "mul-pow" \x     -> mul (mul x x) x == pow x 3
  ]

some_ext_properties :: [Prop FExt]
some_ext_properties = 
  [ Prop2 "add-sub" \x y   -> addExt (subExt x y) y == x 
  , Prop2 "add-neg" \x y   -> subExt x y == addExt x (negExt y)
  , Prop1 "inv-mul" \x     -> x == (0,0) || invExt x `mulExt` x == (1,0)
  , Prop1 "inv-pow" \x     -> invExt x == powExt x (fieldPrime^2 - 2)
  , Prop2 "mul-div" \x y   -> y == (0,0) || mulExt (divExt x y) y == x 
  , Prop3 "mul-add" \x y z -> mulExt (addExt x y) z == mulExt x z `addExt` mulExt y z 
  , Prop1 "mul-pow" \x     -> mulExt (mulExt x x) x == powExt x 3
  ]

--------------------------------------------------------------------------------

runTests :: IO ()
runTests = do
  let n = 1000
  runTestsBase n
  runTestsExt  n

runTestsBase :: Int -> IO ()
runTestsBase n = do
  putStrLn $ "\nbase field properties"
  forM_ some_properties $ \prop -> do
    oks <- replicateM n (runPropF prop) 
    let good = length (filter id oks)
    let bad  = if good < n then " - FAILED!" else " - OK."
    putStrLn $ "  - " ++ propName prop ++ ": " ++ show good ++ " / " ++ show n ++ " passed. " ++ bad 

runTestsExt :: Int -> IO ()
runTestsExt n = do
  putStrLn $ "\nextension field properties"
  forM_ some_ext_properties $ \prop -> do
    oks <- replicateM n (runPropFExt prop) 
    let good = length (filter id oks)
    let bad  = if good < n then " - FAILED!" else " - OK."
    putStrLn $ "  - " ++ propName prop ++ ": " ++ show good ++ " / " ++ show n ++ " passed. " ++ bad

----------------------------------------

rndF :: IO F
rndF = randomRIO (0,fieldPrime-1)

rndFExt :: IO FExt
rndFExt = (,) <$> rndF <*> rndF

runPropF :: Prop F -> IO Bool
runPropF prop = case prop of
  Prop1 _ f1 -> f1 <$> rndF
  Prop2 _ f2 -> f2 <$> rndF <*> rndF
  Prop3 _ f3 -> f3 <$> rndF <*> rndF <*> rndF

runPropFExt :: Prop FExt -> IO Bool
runPropFExt prop = case prop of
  Prop1 _ f1 -> f1 <$> rndFExt
  Prop2 _ f2 -> f2 <$> rndFExt <*> rndFExt
  Prop3 _ f3 -> f3 <$> rndFExt <*> rndFExt <*> rndFExt

--------------------------------------------------------------------------------
