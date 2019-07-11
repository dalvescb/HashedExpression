{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-} --

module HashedOperation where

import Data.IntMap.Strict (fromList, union, unions)
import HashedExpression
import HashedHash
import HashedInner
import HashedNode
import HashedUtils

-- Hide the operations that we are going to define here and load the new qualified version of Prelude
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
import qualified Prelude

-- | Create primitive expressions
--
var :: String -> Expression Zero R
var name = Expression h (fromList [(h, node)])
  where
    node = ([], Var name)
    h = hash node

var1d :: Int -> String -> Expression One R
var1d size name = Expression h (fromList [(h, node)])
  where
    node = ([size], Var name)
    h = hash node

var2d :: (Int, Int) -> String -> Expression Two R
var2d (size1, size2) name = Expression h (fromList [(h, node)])
  where
    node = ([size1, size2], Var name)
    h = hash node

var3d :: (Int, Int, Int) -> String -> Expression Three R
var3d (size1, size2, size3) name = Expression h (fromList [(h, node)])
  where
    node = ([size1, size2, size3], Var name)
    h = hash node

-- |
--
const :: Double -> Expression Zero R
const val = Expression h (fromList [(h, node)])
  where
    node = ([], Const val)
    h = hash node

const1d :: Int -> Double -> Expression One R
const1d size val = Expression h (fromList [(h, node)])
  where
    node = ([size], Const val)
    h = hash node

const2d :: (Int, Int) -> Double -> Expression Two R
const2d (size1, size2) val = Expression h (fromList [(h, node)])
  where
    node = ([size1, size2], Const val)
    h = hash node

const3d :: (Int, Int, Int) -> Double -> Expression Three R
const3d (size1, size2, size3) val = Expression h (fromList [(h, node)])
  where
    node = ([size1, size2, size3], Const val)
    h = hash node

-- | Element-wise sum
--  This is a instance of AddableOp which is a class where d as "Dimension" can have "DimensionType", and et as "ElementType" can be
--  Addable. The rest is the implementation of each function pre-defined in the class definition of AddableOp (See the
--  Comment of AddableOp in HashedExpression.hs)
instance (DimensionType d, Addable et) => AddableOp (Expression d et) where
    (+) :: Expression d et -> Expression d et -> Expression d et
    (+) e1 e2 =
        -- Generate an OperationOption (op) using Sum Node and e1 (first Expression)
        let op = naryET Sum ElementDefault `hasShape` expressionShape e1
        -- Ensure that two input expressions have the same shape and then generate a new expression based on the
        -- new operation (op)
         in ensureSameShape e1 e2 $ applyBinary op e1 e2
    negate :: Expression d et -> Expression d et -- I think the definition is not correct
    negate e1 =
        -- Generate an OperationOption (op) using Neg Node and e1 (first Expression)
        let op = unaryET Neg ElementDefault `hasShape` expressionShape e1
        -- Generate the new expression based on the operation option produced above created
        in applyUnary op e1

    --        This is the old version
    --        =======================================================================================
    --        negate :: Expression d et -> Expression d et -- I think the definition is not correct
    --        negate =
    --         -- Generate an OperationOption (op) using Sum and e1 (first Expression)
    --        let op = unaryET Neg ElementDefault
    --
    --        in applyUnary $ unaryET Neg ElementDefault
    --        ========================================================================================


-- | The sum function is getting a list of expressions and add them up together using the Sum Node defined in
-- HashedExpression.hs and generates a new one based on the "Sum" node.
sum :: (DimensionType d, Addable et)
-- Input : List of Expressions
    => [Expression d et]
-- Output : May be an Expression
    -> Maybe (Expression d et)
-- Return Nothing if the list if empty
sum [] = Nothing
-- If the list is not empty, then add them up together
sum es = Just . applyNary (naryET Sum ElementDefault) $ es

-- | Element-wise multiplication
-- This is a instance of MultiplyOp which is a class defined in "Hashed Expression" (Check its comments), which gets an
-- Expression with d as DimensionType and et as NumType which is an ElementType and produce and expression with same
-- dimension and element type
instance (DimensionType d, NumType et) =>
         MultiplyOp (Expression d et) (Expression d et) (Expression d et) where
-- Inputs : two same type Expressions
-- Output : An expression just like two input expressions.
    (*) :: Expression d et -> Expression d et -> Expression d et
    (*) e1 e2 =
-- Generate an OperationOption (op) using Mul Node and e1 (first Expression)
        let op = naryET Mul ElementDefault `hasShape` expressionShape e1
-- Ensure that two input expressions have the same shape and then generate a new expression based on the
-- new operation (op)
         in ensureSameShape e1 e2 $ applyBinary op e1 e2

-- | The product function is getting a list of expressions, and use the Mul operation (Which is a Node) defined
--  in HashehExpessson.hs and gets a list of expressions and generate a new one based on the "Mul" node.
product ::
       (DimensionType d, NumType et)
-- Input : List of Expressions
    => [Expression d et]
-- Output : May be an Expression
    -> Maybe (Expression d et)
-- Return Nothing if the list if empty
product [] = Nothing
-- If the list is not empty, then use the "Mul" node the generate an Expression using the list of Expression given as
-- input
product es = Just . applyNary (naryET Mul ElementDefault) $ es

-- | Element-wise Power
-- Power Operation is very similar to Product Operation. Both of them has output of d as DimensionType and et asNumType
-- which is an ElementType. The input for this function is an Expression like output and an Integer number (Which is the
-- power).
instance (DimensionType d, NumType et) => PowerOp (Expression d et) Int where
    (^) ::
      -- Input : Expression with d (DimensionType) and et (NumType)
      Expression d et ->
      -- Input : Integer number
      Int ->
      -- Output : Expression with d (DimensionType) and et (NumType)
      Expression d et
    -- First create new OperationOption vased on `hasShape` and "Power" node. And then create a new Expression, using
    -- 'applyUnary' function.
    (^) e1 x = applyUnary (unary (Power x) `hasShape` expressionShape e1) e1

-- | Scale in vector space
--
instance (VectorSpace d et s) =>
         VectorSpaceOp (Expression Zero s) (Expression d et) where
    scale :: Expression Zero s -> Expression d et -> Expression d et
    scale e1 e2 =
        let op =
                binaryET Scale (ElementSpecific $ expressionElementType e2) `hasShape`
                expressionShape e2
         in applyBinary op e1 e2

---- | From R to C two part
----
instance (DimensionType d) =>
         ComplexRealOp (Expression d R) (Expression d C) where
    (+:) :: Expression d R -> Expression d R -> Expression d C
    (+:) e1 e2 =
        let op = binary RealImag
         in ensureSameShape e1 e2 $ applyBinary op e1 e2
    xRe :: Expression d C -> Expression d R
    xRe =
        let op = unary RealPart
         in applyUnary op
    xIm :: Expression d C -> Expression d R
    xIm =
        let op = unary ImagPart
         in applyUnary op

-- | NumOp for R
--
instance (DimensionType d) => NumOp (Expression d R) where
    sqrt = applyUnary (unary Sqrt)
    exp = applyUnary (unary Exp)
    log = applyUnary (unary Log)
    sin = applyUnary (unary Sin)
    cos = applyUnary (unary Cos)
    tan = applyUnary (unary Tan)
    asin = applyUnary (unary Asin)
    acos = applyUnary (unary Acos)
    atan = applyUnary (unary Atan)
    sinh = applyUnary (unary Sinh)
    cosh = applyUnary (unary Cosh)
    tanh = applyUnary (unary Tanh)
    asinh = applyUnary (unary Asinh)
    acosh = applyUnary (unary Acosh)
    atanh = applyUnary (unary Atanh)
    (/) e1 e2 = ensureSameShape e1 e2 $ e1 * e2 ^ (-1)

-- | inner product
--
instance (InnerProductSpace d s) =>
         InnerProductSpaceOp (Expression d s) (Expression d s) (Expression Zero s) where
    (<.>) :: Expression d s -> Expression d s -> Expression Zero s
    (<.>) e1 e2 =
        let scalarShape = []
            op =
                binaryET InnerProd (ElementSpecific $ expressionElementType e2) `hasShape`
                scalarShape
         in ensureSameShape e1 e2 $ applyBinary op e1 e2

-- | Huber loss: https://en.wikipedia.org/wiki/Huber_loss
--
huber :: (DimensionType d) => Double -> Expression d R -> Expression d R
huber delta e = piecewise [-delta, delta] e [lessThan, inRange, largerThan]
  where
    deltaVector =
        constWithShape (expressionShape e) (delta * delta) :: Expression d R
    inRange = const 0.5 *. (e * e)
    lessThan = negate (e * deltaVector) - const 0.5 *. deltaVector
    largerThan = e * deltaVector - const 0.5 *. deltaVector

huber2 :: (DimensionType d) => Double -> Expression d R -> Expression d R
huber2 delta e = piecewise [delta] (e * e) [lessThan, largerThan]
  where
    deltaVector =
        constWithShape (expressionShape e) (delta * delta) :: Expression d R
    lessThan = sqrt (e * e)
    largerThan = const 0.5 *. (e * e + deltaVector)

-- | Piecewise, with a condition expression and branch expressions
-- This is element corresponding, so condition and all branches should have the same dimension and shape
--
piecewise ::
       (DimensionType d)
    => [Double]
    -> Expression d R
    -> [Expression d et]
    -> Expression d et
piecewise marks conditionExp branchExps =
    guard $
    applyConditionAry (conditionAry (Piecewise marks)) conditionExp branchExps
  where
    guard =
        ensureSameShapeList branchExps .
        ensureSameShape conditionExp (head branchExps)

instance (ElementType et) => RotateOp Int (Expression One et) where
    rotate :: Int -> Expression One et -> Expression One et
    rotate x = applyUnary . unary $ Rotate [x]

instance (ElementType et) => RotateOp (Int, Int) (Expression Two et) where
    rotate :: (Int, Int) -> Expression Two et -> Expression Two et
    rotate (x, y) = applyUnary . unary $ Rotate [x, y]

instance (ElementType et) =>
         RotateOp (Int, Int, Int) (Expression Three et) where
    rotate :: (Int, Int, Int) -> Expression Three et -> Expression Three et
    rotate (x, y, z) = applyUnary . unary $ Rotate [x, y, z]

-- | Prelude version of * and +
--
times :: (Num a) => a -> a -> a
times a b = Prelude.product [a, b]

plus :: (Num a) => a -> a -> a
plus a b = Prelude.sum [a, b]

-- | Our instance of operation of built-in types likes Int, Double
--
instance {-# OVERLAPPABLE #-} Num a => AddableOp a where
    (+) = plus
    negate = Prelude.negate

instance {-# OVERLAPPABLE #-} Num a => MultiplyOp a a a where
    (*) = times

--instance {-# OVERLAPPABLE #-} Num a => (^) a a a where
--    (^) x y = x Prelude.^ y
instance {-# OVERLAPPABLE #-} (Num a, Floating a) => NumOp a where
    sqrt = Prelude.sqrt
    exp = Prelude.exp
    log = Prelude.log
    sin = Prelude.sin
    cos = Prelude.cos
    tan = Prelude.tan
    asin = Prelude.asin
    acos = Prelude.acos
    atan = Prelude.atan
    sinh = Prelude.sinh
    cosh = Prelude.cosh
    tanh = Prelude.tanh
    asinh = Prelude.asinh
    acosh = Prelude.acosh
    atanh = Prelude.atanh
    (/) x y = x Prelude./ y
