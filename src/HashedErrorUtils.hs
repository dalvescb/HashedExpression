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
                    ++ show errorBound ++ " is " ++ show errorAmount ++ ", and the mean value for the interval is "
                    ++ show (mean errorBound) ++ "."
    in trace (errorString) input


{--
    ==============================================================
    ==  Functions for calculating constant Values Error errors  ==
    ==============================================================
-}

-- | Class for calculating the error amount for variables
class ConstantErorrCalc val  where
  constantErorCalc ::
    Double ->   -- ^ Radius that will be used for interval calculation
    Int ->    -- ^ Depth of the expression
    val ->      -- ^ Input Variable as Double, 1d, 2d or 3d array
    ErrorType         -- ^ Output same as input variable

-- | Calculate and generate the interval for variables
instance ConstantErorrCalc Double where
  constantErorCalc ::
    Double -- ^ Radius that will be used for interval calculation
    -> Int -- ^ Depth of the expression
    -> Double -- ^ Input Variable as Double, 1d, 2d or 3d array
    -> ErrorType -- ^ Output same as input variable
  constantErorCalc radius depth val  =
    let errorBound = getInterval radius val -- Calculate the interval based on the radius amount
        stdAmount = stdDev errorBound -- Calculate the generated interval's standard deviation amount
    in errorTracer stdAmount errorBound depth (stdAmount,val,errorBound)

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
  intervalSum ::input-> output


instance SumCalculation [ErrorType] ErrorType where
  intervalSum ::  [ErrorType] -> ErrorType
  intervalSum [xs] = xs
  intervalSum (xs:xss) =
    let
        (_,firstExactAmount,firstErrorBound) = xs
        (_,restExactAmount,restErrorBound) = (intervalSum xss :: ErrorType)
        overAllIntervalSumValues = [i+j | i <- firstErrorBound, j<- restErrorBound]
        newOverAllSum = firstExactAmount + restExactAmount
        lowerBound = minimum overAllIntervalSumValues
        upperBound = maximum overAllIntervalSumValues
        newErrorBound = [lowerBound,upperBound]
        newErrorAmount = stdDev newErrorBound
    in  showError (newErrorAmount,newOverAllSum,newErrorBound)


    {--
        =====================================
        ==  Product Error bound calculation    ==
        =====================================
    -}

  -- | Class for interval Sum
class ProductCalculation input output where
    intervalProduct ::input-> output


instance ProductCalculation [ErrorType] ErrorType where
    intervalProduct ::  [ErrorType] -> ErrorType
    intervalProduct [xs] = xs
    intervalProduct (xs:xss) =
      let
          (_,firstExactAmount,firstErrorBound) = xs
          (_,restExactAmount,restErrorBound) = (intervalProduct xss :: ErrorType)
          overAllIntervalProductValues = [i * j | i <- firstErrorBound, j<- restErrorBound]
          newOverAllProduct = firstExactAmount * restExactAmount
          lowerBound = minimum overAllIntervalProductValues
          upperBound = maximum overAllIntervalProductValues
          newErrorBound = [lowerBound,upperBound]
          newErrorAmount = stdDev newErrorBound
      in  showError (newErrorAmount,newOverAllProduct,newErrorBound)

  {--
      ==========================================================
      ==  Do the calculation considering the radius amount    ==
      ==========================================================
  -}

class CalcErrorEvaluable d rc output | d rc -> output where
    calcErrorEval :: ValMaps -> Double -> Int -> Expression d rc -> output

-- | Repeat the process of the sum operation using an error bound parameter
--
instance CalcErrorEvaluable Zero R Double where
    calcErrorEval :: ValMaps -> Double -> Int -> Expression Zero R -> Double
    calcErrorEval valMap radius depth e@(Expression n mp)
        | [] <- retrieveShape n mp =
            case retrieveNode n mp of
                Var name ->
                    case Map.lookup name $ vm0 valMap of
                        Just val -> valueTracer(val P.+ radius) radius depth -- calculating the shift amount based on radius
                        _ -> error "no value associated with the variable"
                Const val -> valueTracer(val P.+ radius) radius depth -- Calculating the shift amount based on the radius
                Sum R args -> valueTracer(sum . map (calcErrorEval valMap radius (depth + 1) . expZeroR mp) $ args)
                                radius depth -- sum operation considering the shift
--                Mul R args -> product . map (errorEval valMap . expZeroR mp) $ args
--                Neg R arg -> -(eval valMap $ expZeroR mp arg)
--                Scale R arg1 arg2 ->
--                    eval valMap (expZeroR mp arg1) *
--                    eval valMap (expZeroR mp arg2)
--                Power x arg -> eval valMap (expZeroR mp arg) ^ x
--                Div arg1 arg2 ->
--                    eval valMap (expZeroR mp arg1) /
--                    eval valMap (expZeroR mp arg2)
--                Sqrt arg -> sqrt (eval valMap (expZeroR mp arg))
--                Sin arg -> sin (eval valMap (expZeroR mp arg))
--                Cos arg -> cos (eval valMap (expZeroR mp arg))
--                Tan arg -> tan (eval valMap (expZeroR mp arg))
--                Exp arg -> exp (eval valMap (expZeroR mp arg))
--                Log arg -> log (eval valMap (expZeroR mp arg))
--                Sinh arg -> sinh (eval valMap (expZeroR mp arg))
--                Cosh arg -> cosh (eval valMap (expZeroR mp arg))
--                Tanh arg -> tanh (eval valMap (expZeroR mp arg))
--                Asin arg -> asin (eval valMap (expZeroR mp arg))
--                Acos arg -> acos (eval valMap (expZeroR mp arg))
--                Atan arg -> atan (eval valMap (expZeroR mp arg))
--                Asinh arg -> asinh (eval valMap (expZeroR mp arg))
--                Acosh arg -> acosh (eval valMap (expZeroR mp arg))
--                Atanh arg -> atanh (eval valMap (expZeroR mp arg))
--                RealPart arg -> realPart (eval valMap (expZeroC mp arg))
--                ImagPart arg -> imagPart (eval valMap (expZeroC mp arg))
--                InnerProd R arg1 arg2 ->
--                    case retrieveShape arg1 mp of
--                        [] ->
--                            eval valMap (expZeroR mp arg1) *
--                            eval valMap (expZeroR mp arg2)
--                        [size] ->
--                            let res1 = eval valMap $ expOneR mp arg1
--                                res2 = eval valMap $ expOneR mp arg2
--                             in sum [ x * y
--                                    | i <- [0 .. size - 1]
--                                    , let x = res1 ! i
--                                    , let y = res2 ! i
--                                    ]
--                        [size1, size2] ->
--                            let res1 = eval valMap $ expTwoR mp arg1
--                                res2 = eval valMap $ expTwoR mp arg2
--                             in sum [ x * y
--                                    | i <- [0 .. size1 - 1]
--                                    , j <- [0 .. size2 - 1]
--                                    , let x = res1 ! (i, j)
--                                    , let y = res2 ! (i, j)
--                                    ]
--                        [size1, size2, size3] ->
--                            let res1 = eval valMap $ expThreeR mp arg1
--                                res2 = eval valMap $ expThreeR mp arg2
--                             in sum [ x * y
--                                    | i <- [0 .. size1 - 1]
--                                    , j <- [0 .. size2 - 1]
--                                    , k <- [0 .. size3 - 1]
--                                    , let x = res1 ! (i, j, k)
--                                    , let y = res2 ! (i, j, k)
--                                    ]
--                        _ -> error "4D shape?"
--                Piecewise marks conditionArg branchArgs ->
--                    let cdt = eval valMap $ expZeroR mp conditionArg
--                        branches = map (eval valMap . expZeroR mp) branchArgs
--                     in chooseBranch marks cdt branches
--                _ ->
--                    error
--                        ("expression structure Scalar R is wrong " ++ prettify e)
        | otherwise = error "one r but shape is not [] ??"
