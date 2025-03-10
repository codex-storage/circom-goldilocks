
-- | Testing the soundness of the Goldilock field emulation templates
--

module Main where

--------------------------------------------------------------------------------

import R1CS

import qualified TestGoldilocks    as Gld
import qualified TestGoldilocksExt as Ext

--------------------------------------------------------------------------------

testGoldilocks :: IO ()
testGoldilocks = testGoldilocks' Field24 Silent

testGoldilocks' :: FieldChoice -> Verbosity -> IO ()
testGoldilocks' fld verbosity = runWithField fld $ \pxy -> do

  let runSpec     what = testSemantics     pxy what verbosity
  let runSpecMany what = testSemanticsMany pxy what verbosity

  runSpec   Gld.specIsZero
  runSpec   Gld.specToGoldi
  runSpec $ Gld.specUnary  Gld.Neg Gld.semantics_neg
  runSpec $ Gld.specBinary Gld.Add Gld.semantics_add
  runSpec $ Gld.specBinary Gld.Sub Gld.semantics_sub
  runSpec $ Gld.specUnary  Gld.Sqr Gld.semantics_sqr
  runSpec $ Gld.specUnary  Gld.Inv Gld.semantics_inv

  -- these are very slow so we don't do exhaustive testing
  runSpec $ Gld.specBinarySmall Gld.Mul Gld.semantics_mul
  runSpec $ Gld.specBinarySmall Gld.Div Gld.semantics_div

--------------------------------------------------------------------------------

testGoldilocksExt :: IO ()
testGoldilocksExt = testGoldilocksExt' Field24 Silent

testGoldilocksExt' :: FieldChoice -> Verbosity -> IO ()
testGoldilocksExt' fld verbosity = runWithField fld $ \pxy -> do

  let runSpec     what = testSemantics     pxy what verbosity
  let runSpecMany what = testSemanticsMany pxy what verbosity

  runSpec $ Ext.specUnary  Ext.Neg Ext.semantics_neg
  runSpec $ Ext.specBinary Ext.Add Ext.semantics_add
  runSpec $ Ext.specBinary Ext.Sub Ext.semantics_sub
--  runSpec $ Ext.specUnary  Ext.Sqr Ext.semantics_sqr
--  runSpec $ Ext.specBinary Ext.Mul Ext.semantics_mul
--  runSpec $ Ext.specUnary  Ext.Inv Ext.semantics_inv
--  runSpec $ Ext.specBinary Ext.Div Ext.semantics_div

--------------------------------------------------------------------------------

main = do
  testGoldilocks
  testGoldilocksExt
