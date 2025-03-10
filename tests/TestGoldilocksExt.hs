

module TestGoldilocksExt where

--------------------------------------------------------------------------------

import Control.Monad
import System.IO.Unsafe

import Semantics
import Common

--------------------------------------------------------------------------------
-- global parameters

circomFile :: FilePath
circomFile = circuitSourceDir </> "test_wrapper_ext.circom"

data Op
  = Neg
  | Add
  | Sub
  | Sqr
  | Mul
  | Inv
  | Div
  deriving (Eq,Show,Bounded,Enum)

enumerateOps :: [Op]
enumerateOps = enumFromTo minBound maxBound

----------------------------------------

mainComponent :: Op -> MainComponent
mainComponent op = 
  case op of
    Neg -> unary  "NegExt"
    Add -> binary "AddExt"
    Sub -> binary "SubExt"
    Sqr -> unary  "SqrExt"
    Mul -> binary "MulExt"
    Inv -> unary  "InvExt"
    Div -> binary "DivExt"
  where

    unary name = MainComponent 
      { _templateName   = name ++ "Wrapper"
      , _templateParams = []
      , _publicInputs   = ["A"]
      }

    binary name = MainComponent 
      { _templateName   = name ++ "Wrapper"
      , _templateParams = []
      , _publicInputs   = ["A","B"]
      }

--------------------------------------------------------------------------------
-- test cases and expected semantics

type TestCase1 =  (Int,Int)
type TestCase2 = ((Int,Int),(Int,Int))

type Output = (Int,Int)

nNestCases = 10

randomTestCasesUnary :: [TestCase1]
randomTestCasesUnary = unsafePerformIO $ replicateM nNestCases rndFExt

randomTestCasesBinary :: [TestCase2]
randomTestCasesBinary = unsafePerformIO $ replicateM nNestCases $ do { x <- rndFExt ; y <- rndFExt ; return (x,y) }

----------------------------------------

semantics_neg :: FExt -> Expected FExt
semantics_neg x = Expecting $ Semantics.negExt x

semantics_add :: (FExt,FExt) -> Expected FExt
semantics_add (x,y) = Expecting $ Semantics.addExt x y

semantics_sub :: (FExt,FExt) -> Expected FExt
semantics_sub (x,y) = Expecting $ Semantics.subExt x y

semantics_sqr :: FExt -> Expected FExt
semantics_sqr x = Expecting $ Semantics.sqrExt x

semantics_mul :: (FExt,FExt) -> Expected FExt
semantics_mul (x,y) = Expecting $ Semantics.mulExt x y

semantics_inv :: FExt -> Expected FExt
semantics_inv x = Expecting $ Semantics.invExt x

semantics_div :: (FExt,FExt) -> Expected FExt
semantics_div (x,y) = Expecting $ Semantics.divExt x y

--------------------------------------------------------------------------------
-- inputs and outputs

inputsA :: TestCase1 -> Inputs Name Integer
inputsA a = Inputs $  toMapping "A" a
                    
inputsAB :: TestCase2 -> Inputs Name Integer
inputsAB (a,b) = Inputs $  toMapping "A" a
                        <> toMapping "B" b

outputsC :: Output -> Outputs Name Integer
outputsC y = Outputs $ toMapping "C" y

--------------------------------------------------------------------------------

specUnary :: Op -> (FExt -> Expected FExt) -> TestSpec TestCase1 Output
specUnary op semantics = TestSpec circomFile (mainComponent op) inputsA outputsC semantics randomTestCasesUnary

specBinary :: Op -> ((FExt,FExt) -> Expected FExt) -> TestSpec TestCase2 Output
specBinary op semantics = TestSpec circomFile (mainComponent op) inputsAB outputsC semantics randomTestCasesBinary

--------------------------------------------------------------------------------
