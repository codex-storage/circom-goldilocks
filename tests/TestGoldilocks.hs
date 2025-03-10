

module TestGoldilocks where

--------------------------------------------------------------------------------

import Semantics

import Common

--------------------------------------------------------------------------------
-- global parameters

circomFile :: FilePath
circomFile = circuitSourceDir </> "test_wrapper.circom"

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
    Neg -> unary  "Neg"
    Add -> binary "Add"
    Sub -> binary "Sub"
    Sqr -> unary  "Sqr"
    Mul -> binary "Mul"
    Inv -> unary  "Inv"
    Div -> binary "Div"
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

type TestCase1 = Int
type TestCase2 = (Int,Int)

type Output = Int

testCasesUnary :: [TestCase1]
testCasesUnary = [0..fieldPrime-1]

testCasesBinary :: [TestCase2]
testCasesBinary = [ (a,b) | a<-testCasesUnary , b<-testCasesUnary ]

-- | Multiplication and division is too slow to test exhaustively
testCasesBinarySmall :: [TestCase2]
testCasesBinarySmall = [ (a,b) | a<-testset, b<-testset ] where
  testset = [0..17] ++ [63,64,65] ++ [127,128,129] ++ [191,192,193] ++ [fieldPrime-18..fieldPrime-1]

----------------------------------------

semantics_neg :: F -> Expected F
semantics_neg x = Expecting $ Semantics.neg x

semantics_add :: (F,F) -> Expected F
semantics_add (x,y) = Expecting $ Semantics.add x y

semantics_sub :: (F,F) -> Expected F
semantics_sub (x,y) = Expecting $ Semantics.sub x y

semantics_sqr :: F -> Expected F
semantics_sqr x = Expecting $ Semantics.sqr x 

semantics_mul :: (F,F) -> Expected F
semantics_mul (x,y) = Expecting $ Semantics.mul x y

semantics_inv :: F -> Expected F
semantics_inv x = Expecting $ Semantics.inv x

semantics_div :: (F,F) -> Expected F
semantics_div (x,y) = Expecting $ Semantics.div x y

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

specUnary :: Op -> (F -> Expected F) -> TestSpec TestCase1 Output
specUnary op semantics = TestSpec circomFile (mainComponent op) inputsA outputsC semantics testCasesUnary

specBinary :: Op -> ((F,F) -> Expected F) -> TestSpec TestCase2 Output
specBinary op semantics = TestSpec circomFile (mainComponent op) inputsAB outputsC semantics testCasesBinary

specBinarySmall :: Op -> ((F,F) -> Expected F) -> TestSpec TestCase2 Output
specBinarySmall op semantics = TestSpec circomFile (mainComponent op) inputsAB outputsC semantics testCasesBinarySmall

--------------------------------------------------------------------------------

input :: TestCase1 -> Inputs Name Integer
input x = Inputs $  toMapping "inp" x

output :: Output -> Outputs Name Integer
output y = Outputs $ toMapping "out" y


semantics_toGoldi :: Int -> Expected Int
semantics_toGoldi k
  | k < 0            = ShouldFail
  | k >= fieldPrime  = ShouldFail
  | otherwise        = Expecting k

main_toGoldi :: MainComponent
main_toGoldi = MainComponent 
  { _templateName   = "ToGoldilocksWrapper"
  , _templateParams = []
  , _publicInputs   = ["inp"]
  }

specToGoldi :: TestSpec TestCase1 Output
specToGoldi = TestSpec circomFile main_toGoldi input output semantics_toGoldi [-10..300] 

--------------------------------------------------------------------------------

semantics_isZero :: Int -> Expected Int
semantics_isZero k = Expecting (if k == 0 then 1 else 0)

main_isZero :: MainComponent
main_isZero = MainComponent 
  { _templateName   = "IsZero"
  , _templateParams = []
  , _publicInputs   = ["inp"]
  }

specIsZero :: TestSpec Int Int
specIsZero = TestSpec circomFile main_isZero input output semantics_isZero [-50..300] 

--------------------------------------------------------------------------------

