import Data.Array.Unboxed as U
import Data.Map (fromList, union)
import qualified Data.Set as Set
import HashedDerivative
import HashedExpression
import HashedInterp
import HashedOperation hiding (product, sum)
import qualified HashedOperation
import HashedPrettify
import HashedSimplify
import qualified HashedSimplifySpec
import qualified HashedToCSpec
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
    , log
    , negate
    , product
    , sin
    , sinh
    , sqrt
    , sum
    , tan
    , tanh
    )
import qualified SimplifyEval.OneCSpec as OneCSpec
import qualified SimplifyEval.OneRSpec as OneRSpec
import qualified SimplifyEval.ZeroCSpec as ZeroCSpec
import qualified SimplifyEval.ZeroRSpec as ZeroRSpec
import qualified Test1
import qualified Test2

import Commons
import Data.Maybe (fromJust)
import HashedUtils ((|>))
import HashedVar
import Test.Hspec

main :: IO ()
main = do
    hspec spec

--    measureTime $ do
--        let exp1 = (((((n+:l))^3))^3)
--        let exp2 = ((k+:u)+(p+:j))
--        showExp $ simplify $ exp1 * exp2
spec :: Spec
spec = do
    describe "SimplifyEval.ZeroRSpec" ZeroRSpec.spec
    describe "SimplifyEval.ZeroCSpec" ZeroCSpec.spec
    describe "SimplifyEval.OneRSpec" OneRSpec.spec
    describe "SimplifyEval.OneCSpec" OneCSpec.spec
    describe "SimplifySpec" HashedSimplifySpec.spec
    describe "Test1" Test1.spec
    describe "Test2" Test2.spec
    describe "HashedToCSpec" HashedToCSpec.spec
