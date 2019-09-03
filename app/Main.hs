{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.Array
import qualified Data.IntMap.Strict as IM
import Data.Map (fromList, union)
import qualified Data.Set as Set
import HashedDerivative
import HashedExpression
import HashedInterp

-- import HashedError
import HashedOperation hiding (product, sum)
import qualified HashedOperation
import HashedPrettify
import HashedSimplify
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

import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.STRef.Strict
import HashedCollect
import HashedError
import HashedErrorUtils
import HashedToC (generateProgram)
import HashedUtils
import HashedVar
import Test.Hspec
import Test.QuickCheck hiding (scale)

sum1 :: (DimensionType d, Addable et) => [Expression d et] -> Expression d et
sum1 = fromJust . HashedOperation.sum

prod1 :: (DimensionType d, Addable et) => [Expression d et] -> Expression d et
prod1 = fromJust . HashedOperation.sum

--
--main = do
--    measureTime $ do
--        let exp1 = (((((n +: l)) ^ 3)) ^ 3)
--        let exp2 = ((k +: u) + (p +: j))
--        showExp $ simplify $ exp1 * exp2
main
--    let exp = x * (x1 * y1 * (s *. z1)) <.> sin x1 * z
 =
    let exp1 = a + e
        valMaps =
            ValMaps
                { vm0 =
                      fromList
                          [ ("a", -34.014382143320645)
                          , ("e", 16.28066712773697)
                          , ("f", -20.563120542303395)
                          , ("l", -42.60173642164761)
                          , ("m", 24.88803997980011)
                          , ("n", 14.668148863837619)
                          , ("q", 33.35634608052121)
                          , ("r", -25.57446677272314)
                          , ("t", 22.5565441852349)
                          , ("u", -0.567665380786866)
                          , ("v", 40.71643373865157)
                          , ("w", -41.553065619799)
                          , ("y", 8.166721044435123)
                          ]
                , vm1 = fromList []
                , vm2 = fromList []
                , vm3 = fromList []
                }
     in do print $ (errorEval valMaps (0.05) 0 exp1 :: ErrorType)--   let exp1 = (a) ^ 2
--   in
--   do print $ eval valMaps exp1
-- do
--    let exp =
--            Expression
--                { exIndex = 4615994729
--                , exMap =
--                      IM.fromList
--                          [ (4614, ([], Var "g"))
--                          , (5029, ([], Var "l"))
--                          , (6025, ([], Var "x"))
--                          , (2999986292, ([], Const 0.0))
--                          , ( 4615994729
--                            , ([], Mul R [5418990404, 4815047263, 27399273777]))
--                          , (4815047263, ([], Mul R [5029, 6025]))
--                          , (5418990404, ([], Power 2 2999986292))
--                          , (27399273777, ([], InnerProd R 4614 5029))
--                          ]
--                } :: Expression Zero R
----    let exp = x *. y
--    showExp exp
--    showExpDebug . collectDifferentials . exteriorDerivative allVars $ exp
--let valMaps = ValMaps
--                      { vm0 = fromList [("a1",-34.014382143320645)
--                                , ("e1",16.28066712773697)
--                                , ("f1",-20.563120542303395)
--                                , ("l1",-42.60173642164761)
--                                , ("m1",24.88803997980011)
--                                , ("n1",14.668148863837619)
--                                , ("q1",33.35634608052121)
--                                , ("r1",-25.57446677272314)
--                                , ("t1",22.5565441852349)
--                                , ("u1",-0.567665380786866)
--                                , ("v1",40.71643373865157)
--                                , ("w1",-41.553065619799)
--                                , ("y1",8.166721044435123)
--                                ]
--                      , vm1 =
--                            fromList
--                                [ ( "a1"
--                                  , array
--                                        (0, 9)
--                                        [ (0, -34.014382143320645)
--                                        , (1, -14.383754829952368)
--                                        , (2, 13.839044225244418)
--                                        , (3, 39.23444270399285)
--                                        , (4, -8.346196105971842)
--                                        , (5, 32.021897482465405)
--                                        , (6, -2.099207335357949)
--                                        , (7, 18.89977709356699)
--                                        , (8, 19.849842558276876)
--                                        , (9, -11.034400538586366)
--                                        ])
--                                , ( "e1"
--                                  , array
--                                        (0, 9)
--                                        [ (0, 16.28066712773697)
--                                        , (1, 15.080728338924786)
--                                        , (2, 4.357769628329768)
--                                        , (3, -33.8990668273663)
--                                        , (4, -13.97708871384697)
--                                        , (5, -4.893475857813698)
--                                        , (6, 33.846173946374705)
--                                        , (7, -1.8162302579196141)
--                                        , (8, 24.15807469533238)
--                                        , (9, -36.53049237413164)
--                                        ])
--                                , ( "f1"
--                                  , array
--                                        (0, 9)
--                                        [ (0, -20.563120542303395)
--                                        , (1, -13.734858073632479)
--                                        , (2, 8.815103664575819)
--                                        , (3, -39.69109719906434)
--                                        , (4, 36.324764051000685)
--                                        , (5, -42.40223429581905)
--                                        , (6, 4.0300185825299994)
--                                        , (7, -21.728820436712176)
--                                        , (8, -32.858631852783965)
--                                        , (9, 20.81902930688235)
--                                        ])
--                                , ( "l1"
--                                  , array
--                                        (0, 9)
--                                        [ (0, -42.60173642164761)
--                                        , (1, 24.48325365685874)
--                                        , (2, 37.875894604014604)
--                                        , (3, 6.700749779894621)
--                                        , (4, 41.38139270245421)
--                                        , (5, -0.5556750519567633)
--                                        , (6, 35.03112590437778)
--                                        , (7, 9.25747980950886)
--                                        , (8, 6.203033327408072)
--                                        , (9, 31.12115687644298)
--                                        ])
--                                , ( "m1"
--                                  , array
--                                        (0, 9)
--                                        [ (0, 24.88803997980011)
--                                        , (1, 37.72777555520414)
--                                        , (2, -41.48463050647311)
--                                        , (3, -27.949016277129385)
--                                        , (4, -22.949974304537996)
--                                        , (5, -2.1399950069594116)
--                                        , (6, -18.128629662471266)
--                                        , (7, 9.142503284371648)
--                                        , (8, 5.490040959456475)
--                                        , (9, -36.48641487203991)
--                                        ])
--                                , ( "n1"
--                                  , array
--                                        (0, 9)
--                                        [ (0, 14.668148863837619)
--                                        , (1, -29.99872589535029)
--                                        , (2, 29.86231545917022)
--                                        , (3, 34.06161387379809)
--                                        , (4, -19.259917637428277)
--                                        , (5, -8.18361392650242)
--                                        , (6, 22.205424109160543)
--                                        , (7, -19.766633785196248)
--                                        , (8, 28.351349475813503)
--                                        , (9, 29.106770247969642)
--                                        ])
--                                , ( "q1"
--                                  , array
--                                        (0, 9)
--                                        [ (0, 33.35634608052121)
--                                        , (1, 11.552040694814782)
--                                        , (2, 14.776704999531347)
--                                        , (3, -24.326291692472864)
--                                        , (4, -28.632957541510464)
--                                        , (5, -27.850388726681572)
--                                        , (6, 20.289900976555284)
--                                        , (7, 12.297408073245023)
--                                        , (8, -2.9608641428049025)
--                                        , (9, -28.269603881462892)
--                                        ])
--                                , ( "r1"
--                                  , array
--                                        (0, 9)
--                                        [ (0, -25.57446677272314)
--                                        , (1, -19.165656620392234)
--                                        , (2, 31.503151006418868)
--                                        , (3, -30.313464192092642)
--                                        , (4, -4.269664823153967)
--                                        , (5, 3.4016239451813037)
--                                        , (6, -35.59302790178572)
--                                        , (7, 14.286794628713691)
--                                        , (8, -6.693659119999791)
--                                        , (9, 24.466866868724868)
--                                        ])
--                                , ( "t1"
--                                  , array
--                                        (0, 9)
--                                        [ (0, 22.5565441852349)
--                                        , (1, -14.50086046228511)
--                                        , (2, -5.80982117647436)
--                                        , (3, -35.698124048185946)
--                                        , (4, 41.92948816763535)
--                                        , (5, -42.86033880835764)
--                                        , (6, 13.935157837449818)
--                                        , (7, 22.80620294961593)
--                                        , (8, 31.47834260976649)
--                                        , (9, 6.928031718141705)
--                                        ])
--                                , ( "u1"
--                                  , array
--                                        (0, 9)
--                                        [ (0, -0.567665380786866)
--                                        , (1, -24.143251856454725)
--                                        , (2, 2.199403832695001)
--                                        , (3, 18.34171754727925)
--                                        , (4, -32.75483789538159)
--                                        , (5, 10.419337693747584)
--                                        , (6, 21.418047943707403)
--                                        , (7, -21.95329212662424)
--                                        , (8, 29.967152316454207)
--                                        , (9, 13.656287879314968)
--                                        ])
--                                , ( "v1"
--                                  , array
--                                        (0, 9)
--                                        [ (0, 40.71643373865157)
--                                        , (1, 19.93355571572348)
--                                        , (2, -16.965498605273833)
--                                        , (3, 38.869946691828886)
--                                        , (4, 24.13356614671275)
--                                        , (5, -20.151454859086606)
--                                        , (6, -28.097978187285797)
--                                        , (7, 35.72016281599924)
--                                        , (8, -28.524300040626265)
--                                        , (9, -39.963559454229355)
--                                        ])
--                                , ( "w1"
--                                  , array
--                                        (0, 9)
--                                        [ (0, -41.553065619799)
--                                        , (1, -18.3583898008342)
--                                        , (2, -11.699131894367701)
--                                        , (3, -21.626643958372842)
--                                        , (4, -42.873207426399986)
--                                        , (5, -26.589079077640715)
--                                        , (6, -22.988217412471247)
--                                        , (7, -16.65335788465353)
--                                        , (8, 30.656496633519772)
--                                        , (9, -23.459884474141333)
--                                        ])
--                                , ( "y1"
--                                  , array
--                                        (0, 9)
--                                        [ (0, 8.166721044435123)
--                                        , (1, -20.928848038423993)
--                                        , (2, 18.02983931201259)
--                                        , (3, 16.878778438786902)
--                                        , (4, 41.11458456220103)
--                                        , (5, 30.82717521269954)
--                                        , (6, 25.810468297863164)
--                                        , (7, 24.085520101811746)
--                                        , (8, 8.10746605651697)
--                                        , (9, 36.25503611351855)
--                                        ])
--                                ]
--                      , vm2 = fromList []
--                      , vm3 = fromList []
--                      }
