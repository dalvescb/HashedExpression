{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module HashedErrorUtils where

import Prelude hiding
    ( (*)
    , (+)
    , (-)
    , (/)
    , (^)
    , acos
    , acosh
    , asin
    , asinh
    , atan
    , atanh
    , const
    , cos
    , cosh
    , exp
    , negate
    , sin
    , sinh
    , sqrt
    , tan
    , tanh
    )

import qualified Prelude as P
import qualified Data.Map as Map
import Debug.Trace
import HashedExpression
import Data.Array
import Data.Complex
import HashedUtils
import HashedNode (retrieveShape, retrieveNode)

{--
    =============================================
    ==  Data Types  ==
    =============================================
-}

-- | Zero dimension Error data type
-- Double : Error amount based on the standard deviation
-- Double : Exact amount for the function
-- [Double] : Error bound calculated based on the different sub operations
type ErrorType = (Double, Double , [Double])


-- | Zero dimension Error data type
-- Double : Error amount based on the standard deviation
-- Double : Exact amount for the function
-- [Double] : Error bound calculated based on the different sub operations
type ErrorTypeC = (Complex Double, Complex Double , [Complex Double])

-- | One dimension Error data type
-- Double : Error amount based on the standard deviation
-- Double : Exact amount for the function
-- Array Int [Double] : Error bound calculated for one dimension array
type OneDArrayErrorType = Array Int ErrorType

-- | Two dimension Error data type
-- Double : Error amount based on the standard deviation
-- Double : Exact amount for the function
-- Array (Int,Int) [Double] : Error bound calculated for two dimension array
type TwoDArrayErrorType = Array (Int,Int) ErrorType


-- | Three dimension Error data type
-- Double : Error amount based on the standard deviation
-- Double : Exact amount for the function
-- Array (Int,Int,Int) [Double] : Error bound calculated for three dimension array
type ThreeDArrayErrorType = Array (Int,Int) ErrorType


-- | This operation emulates the mathematical operation
-- | Turn expression to the right type
--
expZeroR :: ExpressionMap -> Int -> Expression Zero R
expZeroR = flip Expression

expOneR :: ExpressionMap -> Int -> Expression One R
expOneR = flip Expression

expTwoR :: ExpressionMap -> Int -> Expression Two R
expTwoR = flip Expression

expThreeR :: ExpressionMap -> Int -> Expression Three R
expThreeR = flip Expression

expZeroC :: ExpressionMap -> Int -> Expression Zero C
expZeroC = flip Expression

expOneC :: ExpressionMap -> Int -> Expression One C
expOneC = flip Expression

expTwoC :: ExpressionMap -> Int -> Expression Two C
expTwoC = flip Expression

expThreeC :: ExpressionMap -> Int -> Expression Three C
expThreeC = flip Expression


-- | Choose branch base on condition value
--
chooseBranch :: [Double] -> Double -> [a] -> a
chooseBranch marks val branches
    | val < head marks = head branches
    | otherwise =
        snd . last . filter ((val >=) . fst) $ zip marks (tail branches)

{--
    =============================================
    ==  Interval Generation related functions  ==
    =============================================
-}

-- | Interval generation based on the value mapped to the function
intervalGen ::
    Double -> -- ^ Radius for generating the interval
    Double -> -- ^ The value mapped to the function
    [Double]  -- ^ Output Interval
intervalGen radius val = [val P.- radius, val P.+ radius]

-- | Class for calculating the intervals
class InterValGenCLass val interval where
  getInterval :: Double -> val -> interval

-- | Instance of function "getInterval" for generating a interval of double numbers based on an double input
instance InterValGenCLass Double [Double] where
  getInterval :: Double -> Double -> [Double]
  getInterval = intervalGen

-- | Instance of function "getInterval" for generating a interval of a 1d array numbers
instance InterValGenCLass (Array Int Double) (Array Int [Double]) where
  getInterval:: Double -> Array Int Double -> Array Int [Double]
  getInterval radius val = listArray (bounds val) [intervalGen radius (val ! i)  | i <- indices val]

-- | Instance of function "getInterval" for generating a interval of a 2d array numbers
instance InterValGenCLass (Array (Int,Int) Double) (Array (Int,Int) [Double]) where
  getInterval :: Double -> Array (Int,Int) Double -> Array (Int,Int) [Double]
  getInterval radius val  = listArray (bounds val) [intervalGen radius (val ! (i, j))  | (i, j) <- indices val]

-- | Instance of function "getInterval" for generating a interval of a 3d array numbers
instance InterValGenCLass (Array (Int,Int,Int) Double) (Array (Int,Int,Int) [Double]) where
  getInterval :: Double -> Array (Int,Int,Int) Double -> Array (Int,Int,Int) [Double]
  getInterval radius val  = listArray (bounds val) [intervalGen radius (val ! (i, j, k))  | (i, j, k) <- indices val]

{--
    ==============================================================
    ==  Functions for doing statistical calculation over lists  ==
    ==============================================================
-}

-- | For calculation of length for the list
length' = fromIntegral . length

-- | Calculating the mean of the interval
mean :: [Double] -> Double
mean = (P./) <$> sum <*> length' -- Calculating the amount of Mean

-- | Calculating the variance of an interval
variance :: [Double] -> Double
variance list =
  let avg = mean list
      summedElements = sum (map (\x -> (x P.- avg) P.^ 2) list) -- Nominator for the Std calculation
      lengthX = length' list -- Denominator for Std calculation
   in summedElements P./ lengthX

-- | Calculate standard deviation for an interval
stdDev ::
  [Double] -> -- ^ Target Interval
   Double -- ^ out put std value
stdDev list = P.sqrt $ variance list


{--
    ==============================================================
    ==  Functions for tracing the amount of errors and values  ==
    ==============================================================
-}

-- | Class for tracing amount of values in different depth
class ValueTrace a  where
  valueTracer :: a -> Double -> Int -> a

-- | Trace the amount of value for Zero dimension values
instance ValueTrace (Complex Double) where
  valueTracer :: (Complex Double) -> Double -> Int -> (Complex Double)
  valueTracer val radius depth
    | radius < 0 =
      let traceStrnig= "The left bound value in depth (" ++ show depth ++ ") is " ++ show val
      in trace (traceStrnig) val
    | radius > 0 =
      let traceStrnig= "The Right bound value in depth (" ++ show depth ++ ") is " ++ show val
      in trace (traceStrnig) val
    | otherwise =
      let traceStrnig= "The value amount in depth (" ++ show depth ++ ") is " ++ show val
      in trace (traceStrnig) val


-- | Trace the amount of value for Zero dimension values
instance ValueTrace Double where
  valueTracer :: Double -> Double -> Int -> Double
  valueTracer val radius depth
    | radius < 0 =
      let traceStrnig= "The left bound value in depth (" ++ show depth ++ ") is " ++ show val
      in trace (traceStrnig) val
    | radius > 0 =
      let traceStrnig= "The Right bound value in depth (" ++ show depth ++ ") is " ++ show val
      in trace (traceStrnig) val
    | otherwise =
      let traceStrnig= "The value amount in depth (" ++ show depth ++ ") is " ++ show val
      in trace (traceStrnig) val



-- | Class for tracing error based on the selected radius and different level of depths.
class ErrorTrace a b where
  errorTracer ::
    a -> -- ^ Error Amount
    b -> -- ^ Error Bound
    Int -> -- ^ Calculation depth
    c -> -- ^ Input value
    c -- ^ Output equals to input

instance ErrorTrace Double [Double] where
-- | Class for Tracing the Error amount
  errorTracer::
    Double ->  -- ^ Error amount
    [Double] -> -- ^ Error bound
    Int -> -- ^ Calculation depth
    a -> -- ^ input that just going to path throw function
    a  -- ^ output same as input
  errorTracer errorAmount errorBound calcDepth input =
    let errorString = "The Expression depth is " ++ show calcDepth ++ ", and The error amount for the bound "
                    ++ show errorBound ++ " is " ++ show errorAmount ++ "."
    in trace errorString input


instance ErrorTrace (Array Int Double) (Array Int [Double]) where
-- | Class for Tracing the Error amount
  errorTracer::
    Array Int Double ->  -- ^ Error amount
    Array Int [Double] -> -- ^ Error bound
    Int -> -- ^ Calculation depth
    a -> -- ^ input that just going to path throw function
    a  -- ^ output same as input
  errorTracer errorAmount errorBound calcDepth input =
    let errorString = "The Expression depth is " ++ show calcDepth ++ ", and The error amount for the bound "
                    ++ show errorBound ++ " is " ++ show errorAmount ++ "."
    in trace errorString input


instance ErrorTrace (Array (Int, Int) Double) (Array (Int, Int) [Double]) where
-- | Class for Tracing the Error amount
  errorTracer::
    Array (Int, Int) Double ->  -- ^ Error amount
    Array (Int, Int) [Double] -> -- ^ Error bound
    Int -> -- ^ Calculation depth
    a -> -- ^ input that just going to path throw function
    a  -- ^ output same as input
  errorTracer errorAmount errorBound calcDepth input =
    let errorString = "The Expression depth is " ++ show calcDepth ++ ", and The error amount for the bound "
                    ++ show errorBound ++ " is " ++ show errorAmount ++ "."
    in trace errorString input


{--
    ==============================================================
    ==  Functions for calculating constant Values Error errors  ==
    ==============================================================
-}

-- | Class for calculating the error amount for variables
class ConstantErorrCalc input output  where
  constantErorCalc ::
    Int ->    -- ^ Depth of the expression
    Double ->   -- ^ Radius that will be used for interval calculation
    input ->      -- ^ Input Variable as Double, 1d, 2d or 3d array
    output         -- ^ Output same as input variable

-- | Calculate and generate the interval for variables
instance ConstantErorrCalc Double ErrorType where
  constantErorCalc ::
    Int -- ^ Depth of the expression
    -> Double -- ^ Radius that will be used for interval calculation
    -> Double -- ^ Input Variable as Double, 1d, 2d or 3d array
    -> ErrorType -- ^ Output same as input variable
  constantErorCalc depth radius input  =
    let errorBound = getInterval radius input -- Calculate the interval based on the radius amount
        stdAmount = stdDev errorBound -- Calculate the generated interval's standard deviation amount
    in errorTracer stdAmount errorBound depth (stdAmount,input,errorBound)



  {--
      ======================
      ==  Error Tracer    ==
      ======================
  -}

showError :: ErrorType -> ErrorType
showError input = trace ("The amount in this step is " ++ show input) input


  {--
      =====================================
      ==  Sum Error bound calculation    ==
      =====================================
  -}

-- | Class for interval Sum
class SumCalculation input output where
  intervalSum ::
    input -> -- ^ Input : input of the interval Sum function - could be either list or array of ErrorType
    Int -> -- ^ Depth of the calculations
    output -- ^ Output : Output of the interval Sum function - could be either list or array of ErrorType


instance SumCalculation [ErrorType] ErrorType where
  intervalSum ::  [ErrorType] -> Int -> ErrorType
  intervalSum [xs] depth = xs
  intervalSum (xs:xss) depth =
    let
        (_,firstExactAmount,firstErrorBound) = xs
        (_,restExactAmount,restErrorBound) = (intervalSum xss depth :: ErrorType)
        overAllIntervalSumValues = [i+j | i <- firstErrorBound, j<- restErrorBound]
        newOverAllSum = firstExactAmount + restExactAmount
        lowerBound = minimum overAllIntervalSumValues
        upperBound = maximum overAllIntervalSumValues
        newErrorBound = [lowerBound,upperBound]
        newErrorAmount = stdDev newErrorBound
    in  errorTracer newErrorAmount newErrorBound depth (newErrorAmount,newOverAllSum,newErrorBound)


    {--
        =====================================
        ==  Product Error bound calculation    ==
        =====================================
    -}

  -- | Class for interval Product
class ProductCalculation input output where
    intervalProduct ::
      input -> -- ^ Input : input of the interval Sum function - could be either list or array of ErrorType
      Int -> -- ^ Depth of the calculations
      output -- ^ Output : Output of the interval Sum function - could be either list or array of ErrorType


instance ProductCalculation [ErrorType] ErrorType where
    intervalProduct ::  [ErrorType] -> Int -> ErrorType
    intervalProduct [xs] depth = xs
    intervalProduct (xs:xss) depth =
      let
          (_,firstExactAmount,firstErrorBound) = xs
          (_,restExactAmount,restErrorBound) = (intervalProduct xss depth :: ErrorType)
          overAllIntervalProductValues = [i * j | i <- firstErrorBound, j<- restErrorBound]
          newOverAllProduct = firstExactAmount * restExactAmount
          lowerBound = minimum overAllIntervalProductValues
          upperBound = maximum overAllIntervalProductValues
          newErrorBound = [lowerBound,upperBound]
          newErrorAmount = stdDev newErrorBound
      in  errorTracer newErrorAmount newErrorBound depth (newErrorAmount,newOverAllProduct,newErrorBound)

 {--
        =====================================
        ==  Neg Error bound calculation    ==
        =====================================
    -}

-- | Class for Neg interval calculation
class NegCalculation input output where
    intervalNeg ::
      Int -> -- ^ Depth of the calculations
      input -> -- ^ Input : input of the interval Sum function - could be either list or array of ErrorType
      output -- ^ Output : Output of the interval Sum function - could be either list or array of ErrorType

instance NegCalculation ErrorType ErrorType where
  intervalNeg ::  Int -> ErrorType -> ErrorType
  intervalNeg depth inputInterval  =
    let (_,exactAmount,errorBound) = inputInterval
        primaryNegInterval = map negate errorBound
        lowerBound = minimum primaryNegInterval
        upperBound = maximum primaryNegInterval
        newErrorBound = [lowerBound, upperBound]
        newErrorAmount = stdDev newErrorBound
     in errorTracer newErrorAmount newErrorBound depth (newErrorAmount, negate exactAmount, newErrorBound)


{--
        =====================================
        ==  Power Error bound calculation  ==
        =====================================
-}


-- | Class for Power interval calculation
class PowerCalculation input output where
  intervalPower ::
      Int -> -- ^ Depth of the calculations
      Int -> -- ^ Input : Power value as integer
      input -> -- ^ Input : input of the interval Power function - could be either list or array of ErrorType
      output -- ^ Output : Output of the interval Sum function - could be either list or array of ErrorType

instance PowerCalculation ErrorType ErrorType where
  intervalPower ::   Int -> Int -> ErrorType ->  ErrorType
  intervalPower depth power inputInterval   =
    let (_,exactAmount,errorBound) = inputInterval
        primaryNegInterval = map (^ power) errorBound
        lowerBound = minimum primaryNegInterval
        upperBound = maximum primaryNegInterval
        newErrorBound = [lowerBound, upperBound]
        newErrorAmount = stdDev newErrorBound
     in errorTracer newErrorAmount newErrorBound depth (newErrorAmount, exactAmount ^ power, newErrorBound)



{--
        =====================================
        ==  Power Error bound calculation  ==
        =====================================
-}


-- | Class for Div interval calculation
class DivCalculation nom den output where
  intervalDiv ::
      Int -> -- ^ Depth of the calculations
      nom -> -- ^ Input : Nominator of the fraction - could be either list or array of ErrorType
      den -> -- ^ Input : Denominator of the fraction - could be either list or array of ErrorType
      output -- ^ Output : Output of the interval Sum function - could be either list or array of ErrorType

-- | Div interval operation for Real numbers
instance DivCalculation ErrorType ErrorType ErrorType where
  intervalDiv ::  Int -> ErrorType -> ErrorType -> ErrorType
  intervalDiv depth nom den  =
    let newDen = intervalPower depth  (-1) den
        devResult = intervalProduct [nom,newDen] depth
        (newErrorAmount,exactAmount,newErrorBound) = devResult
     in errorTracer newErrorAmount newErrorBound depth devResult


{--
        =====================================
        ==  Sqrt Error bound calculation   ==
        =====================================
-}

-- | Class for Sqrt interval calculation
class SqrtCalculation input output where
    intervalSqrt ::
      Int -> -- ^ Depth of the calculations
      input -> -- ^ Input : input of the interval Sum function - could be either list or array of ErrorType
      output -- ^ Output : Output of the interval Sum function - could be either list or array of ErrorType


-- | Sqrt interval calculation for real numbers
instance SqrtCalculation ErrorType ErrorType where
  intervalSqrt ::  Int -> ErrorType -> ErrorType
  intervalSqrt depth inputInterval  =
    let (_,exactAmount,errorBound) = inputInterval
        primarySqrtInterval = map P.sqrt errorBound
        lowerBound = minimum primarySqrtInterval
        upperBound = maximum primarySqrtInterval
        newErrorBound = [lowerBound, upperBound]
        newErrorAmount = stdDev newErrorBound
     in errorTracer newErrorAmount newErrorBound depth (newErrorAmount, P.sqrt exactAmount, newErrorBound)


{--
        =====================================
        ==  Log Error bound calculation   ==
        =====================================
-}

-- | Class for Log interval calculation
class LogCalculation input output where
    intervalLog ::
      Int -> -- ^ Depth of the calculations
      input -> -- ^ Input : input of the interval Sum function - could be either list or array of ErrorType
      output -- ^ Output : Output of the interval Sum function - could be either list or array of ErrorType


-- | Sqrt interval calculation for real numbers
instance LogCalculation ErrorType ErrorType where
  intervalLog ::  Int -> ErrorType -> ErrorType
  intervalLog depth inputInterval =
    let (_,exactAmount,errorBound) = inputInterval
        primarySqrtInterval = map P.log errorBound
        lowerBound = minimum primarySqrtInterval
        upperBound = maximum primarySqrtInterval
        newErrorBound = [lowerBound, upperBound]
        newErrorAmount = stdDev newErrorBound
     in errorTracer newErrorAmount newErrorBound depth (newErrorAmount, P.log exactAmount, newErrorBound)