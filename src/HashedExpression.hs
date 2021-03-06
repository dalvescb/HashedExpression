module HashedExpression
  ( exteriorDerivative,
    derivativeAllVars,
    R,
    C,
    Covector,
    Expression,
    Scalar,
    PowerOp (..),
    PiecewiseOp (..),
    VectorSpaceOp (..),
    FTOp (..),
    NodeID,
    ComplexRealOp (..),
    RotateOp (..),
    InnerProductSpaceOp (..),
    constant,
    constant1D,
    constant2D,
    constant3D,
    variable,
    variable1D,
    variable2D,
    variable3D,
    sum,
    product,
    normalize,
    Evaluable (..),
  )
where

import HashedExpression.Derivative
import HashedExpression.Internal.CollectDifferential
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Normalize
import HashedExpression.Interp
import HashedExpression.Operation
import Prelude hiding ((^))
