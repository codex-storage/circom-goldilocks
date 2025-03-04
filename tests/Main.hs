
-- | Testing the soundness of the Goldilock field emulation templates
--

module Main where

--------------------------------------------------------------------------------

import R1CS

import TestGoldilocks

--------------------------------------------------------------------------------

testGoldilocks :: IO ()
testGoldilocks = testGoldilocks' Field24 Silent

testGoldilocks' :: FieldChoice -> Verbosity -> IO ()
testGoldilocks' fld verbosity = runWithField fld $ \pxy -> do

  let runSpec     what = testSemantics     pxy what verbosity
  let runSpecMany what = testSemanticsMany pxy what verbosity

  runSpec   specIsZero
  runSpec   specToGoldi
  runSpec $ specUnary  Neg semantics_neg
  runSpec $ specBinary Add semantics_add
  runSpec $ specBinary Sub semantics_sub
  runSpec $ specUnary  Inv semantics_inv

  -- these are very slow so we don't do exhaustive testing
  runSpec $ specBinarySmall Mul semantics_mul
  runSpec $ specBinarySmall Div semantics_div

--------------------------------------------------------------------------------

main = do
  testGoldilocks