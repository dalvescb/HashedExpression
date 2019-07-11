{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE FunctionalDependencies #-}

module HashedExpression where

import Data.Array
import qualified Data.Complex as DC
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Proxy (Proxy)
import Data.Typeable (Typeable, typeRep)
import GHC.TypeLits (Nat)
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
    , cos
    , cosh
    , exp
    , negate
    , sin
    , sinh
    , tan
    , tanh
    )

-- | Type representation of elements in the 1D, 2D, 3D, ... grid
--
data R
    deriving (NumType, ElementType, Addable, Typeable)

data C
    deriving (NumType, ElementType, Addable, Typeable)

data Covector
    deriving (ElementType, Addable, Typeable)

-- | Type representation of vector dimension
--
data Zero
    deriving (DimensionType, Typeable)

data One
    deriving (DimensionType, Typeable)

data Two
    deriving (DimensionType, Typeable)

data Three
    deriving (DimensionType, Typeable)

-- | Classes as constraints
--
class ElementType et

-- | NumType Element Type definition
class ElementType et =>
      NumType et

-- | Addable Element Type definition
class ElementType et =>
      Addable et

-- | Dimension Type Definition
class DimensionType d



class (DimensionType d, Addable et, NumType s) =>
      VectorSpace d et s


class VectorSpace d s s =>
      InnerProductSpace d s


-- | Instances
-- Set language pragma {-# OVERLAPPABLE #-} because GHC looks at the head first (e.g VectorSpace d et R) and check
-- the constraints later, therefore it will show overlap instances error if we declare more instances of VectorSpace if
-- if the arguments don't satisfy the constraints
--
--instance {-# OVERLAPPABLE #-} ElementType et => Addable et
instance {-# OVERLAPPABLE #-} (ElementType et, Addable et, DimensionType d) =>
                              VectorSpace d et R

instance (DimensionType d) => VectorSpace d C C

instance (DimensionType d, NumType nt) => VectorSpace d Covector nt -- TODO? True with C??

instance VectorSpace d s s => InnerProductSpace d s

-- | Classes for operations so that both Expression and Pattern (in HashedPattern) can implement
--

-- | Addable Operations are (+), Negative and (-)
-- Class for defining addable operations
class AddableOp a where
    -- (+) gets two value of the same type and return a third value from the same type
    (+) :: a -> a -> a
    -- "negate" gets a value of the same type and return a same type value
    negate :: a -> a
    -- (-) gets two values of the same type and return a same type value (combination of two operation above)
    (-) :: a -> a -> a
    x - y = x + negate y

-- | Class for defining the Multiply Operation
class MultiplyOp a b c | a b -> c where
-- The only operation considers as MultiplyOp is (*) which gets two not necessarily same type values (a and b) and
-- returns not necessarily same type output (c)
    (*) :: a -> b -> c

class PowerOp a b | a -> b where
    (^) :: a -> b -> a

class AddableOp b =>
      VectorSpaceOp a b
    where
    scale :: a -> b -> b
    (*.) :: a -> b -> b
    (*.) = scale

class NumOp a where
    sqrt :: a -> a
    exp :: a -> a
    log :: a -> a
    sin :: a -> a
    cos :: a -> a
    tan :: a -> a
    asin :: a -> a
    acos :: a -> a
    atan :: a -> a
    sinh :: a -> a
    cosh :: a -> a
    tanh :: a -> a
    asinh :: a -> a
    acosh :: a -> a
    atanh :: a -> a
    (/) :: a -> a -> a

class ComplexRealOp r c | r -> c, c -> r where
    (+:) :: r -> r -> c
    xRe :: c -> r
    xIm :: c -> r

class InnerProductSpaceOp a b c | a b -> c where
    (<.>) :: a -> b -> c
-- RotateOp class is used for defining the operations related to rotate.
class RotateOp k a | a -> k where
    --  Rotate operation which belong to RotateOp class and doing rotation.
    rotate :: k -> a -> a

infixl 6 +, -

infixl 7 *

infixl 8 *., `scale`, <.>

infix 8 ^

-- Different kinds of Shape types used in Hashed Expression
-- | Shape type:
-- []        --> scalar
-- [n]       --> 1D with size n
-- [n, m]    --> 2D with size n × m
-- [n, m, p] --> 3D with size n × m × p
type Shape = [Int]

-- | Args - list of indices of arguments in the ExpressionMap
--
type Args = [Int]

type Arg = Int

type ConditionArg = Int

type BranchArg = Int


-- Rotate Amount is a List of Integer  Number
-- | Amount of the Rotate which will be presented with a list of integer numbers.
type RotateAmount = [Int]

-- | Data representation of element type
-- It can be Either R which is Real Numbers, C which is Complex Numbers or Covector
data ET -- Element
    = R
    | C
    | Covector
    deriving (Show, Eq, Ord)

-- | Internal Tuple has the structure as described Bellow:
-- Shape: Shape of the expression (See Shape  Comments on this file)
-- Node : Node of the Expression (See Node Comments of this file)
-- We can reconstruct the type of the Expression
type Internal = (Shape, Node)

-- | Hash map of all subexpressions
-- Expression Map is a dictionary of Internal data type (See the Internal Data Type Description)
-- (Based on the definition of IntMap) which mapping the integer keys to values.
type ExpressionMap = IntMap Internal

-- | Expression with 2 phantom types (dimension and Element Type)
-- But an Expression also keeps other information as structure (Index of the Expression, as well as the expression Map
-- of all subexpressions it has (Have a llok at the Expression map Comment).
data Expression d et =
    Expression
        { exIndex :: Int -- the index this expression
        , exMap :: ExpressionMap -- all subexpressions
        }
    deriving (Show, Eq, Ord, Typeable)

type role Expression nominal nominal -- So the users cannot use Data.Coerce.coerce to convert between expression types

-- | Node type
--
data Node
    = Var String
    | DVar String -- only contained in **Expression d Covector (1-form)**
    | Const Double -- only all elements the same
    -- MARK: Basics
    | Sum ET Args -- element-wise sum
    | Mul ET Args -- multiply --> have different meanings (scale in vector space, multiplication, ...)
    | Power Int Arg -- TODO: Power for Complex or not ?
    | Neg ET Arg
    | Scale ET Arg Arg
    -- MARK: only apply to R
    | Div Arg Arg
    | Sqrt Arg
    | Sin Arg
    | Cos Arg
    | Tan Arg
    | Exp Arg
    | Log Arg
    | Sinh Arg
    | Cosh Arg
    | Tanh Arg
    | Asin Arg
    | Acos Arg
    | Atan Arg
    | Asinh Arg
    | Acosh Arg
    | Atanh Arg
    -- MARK: Complex related
    | RealImag Arg Arg -- from real and imagine
    | RealPart Arg -- extract real part
    | ImagPart Arg -- extract imaginary part
    -- MARK: Inner product Space
    | InnerProd ET Arg Arg
    -- MARK: Piecewise
    | Piecewise [Double] ConditionArg [BranchArg]
    -- Mark : Rotate Node type which gets two input, the first input is Rotate Amount (A list of Integers) the second is
    -- the Arg  which is the list of indices of arguments in the ExpressionMap
    | Rotate RotateAmount Arg
    deriving (Show, Eq, Ord)
