module HashedExpression.Internal.ToLLVM 
  (     LLVMMemMap(..)
      , LLVMMemMapEntry
      , Code
      , makeLLVMMemMap
  ) where 

import Data.Array
import Data.Graph (buildG, topSort)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', intercalate, intersperse, tails)
import Data.List.HT (splitLast)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set, empty, insert, member)
import qualified Data.Set as Set
import Debug.Trace (traceShowId)
import FFTW
import GHC.Stack (HasCallStack)
import HashedExpression.Internal.Expression
    ( DimensionType
    , ET(..)
    , Expression(..)
    , ExpressionMap
    , Node(..)
    , NumType
    , Shape
    )
import HashedExpression.Internal.Hash
import HashedExpression.Internal.Inner
import HashedExpression.Internal.Node
import HashedExpression.Internal.Utils
import HashedExpression.Prettify (prettifyDebug)
import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Global
import LLVM.Context
import LLVM.Module

import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS

--offset/key into the allocated memory
offsetType :: Type
offsetType = IntegerType 64

--all elements used here are of double type
elementType :: Type
--elementType = Double
elementType = FloatingPointType DoubleFP

type LLVMMemMapEntry = (Int, EntryType, Shape)

data EntryType
    = EntryR
    | EntryC
    deriving (Show, Eq, Ord)

type Code = AST.Module

data LLVMMemMap =
    LLVMMemMap
        { entryMap :: IntMap LLVMMemMapEntry -- node id -> (offset, R or C, shape)
        , totalDoubles :: Int
        }
    deriving (Show, Eq, Ord)
    
makeLLVMMemMap :: ExpressionMap -> LLVMMemMap
makeLLVMMemMap exprMap = uncurry LLVMMemMap $ foldl' (mmUpdate exprMap) (IM.empty, 0) nkeys
  where
    nkeys = IM.keys exprMap

-- | An update function for foldl
--
mmUpdate ::
       ExpressionMap
    -> (IntMap LLVMMemMapEntry, Int)
    -> Int
    -> (IntMap LLVMMemMapEntry, Int)
mmUpdate exprMap (memMapSoFar, sizeSoFar) nId =
    let (shape, node) = retrieveInternal nId exprMap
        (nodeSz, mmShape)
            | nodeElementType node exprMap == R = (product shape, EntryR)
            | nodeElementType node exprMap == C = (2 * product shape, EntryC)
        newMemMap = IM.insert nId (sizeSoFar, mmShape, shape) memMapSoFar
     in (newMemMap, sizeSoFar + nodeSz)



-- | Generate evaluation code (usually an expression and its partial derivatives) given an ExpressionMap and indices of nodes to be computed
--
generateEvaluatingCodes :: MemMap -> (ExpressionMap, [Int]) -> Code
generateEvaluatingCodes memMap (mp, rootIds) =
    GlobalDefinition functionDefaults
       { name = Name "func"
       , parameters =
           ( [ Parameter int (Name "a") []
             , Parameter int (Name "b") [] ]
           , False )
       , returnType = int
       , basicBlocks = [body]
       }
       where
         body = BasicBlock
             (Name "entry")
             map genCode $ topologicalSortManyRoots (mp, rootIds)
             (Do $ Ret (Just (LocalReference int (Name "result"))) [])
         getShape :: Int -> Shape
         getShape nId = retrieveShape nId mp
    -- |
    --
         addressOf :: Int -> String
         addressOf nId = "(ptr + " ++ show (memOffset memMap nId LookupR) ++ ")"
    -- for with only body
         for :: String -> Int -> Code -> Code
         for iter nId scopeCodes = forWith iter (getShape nId) ([], scopeCodes, [])
    -- Real node
         at :: Int -> String -> String
         at = accessPtr memMap LookupR mp
    -- Real part of complex node
         reAt :: Int -> String -> String
         reAt = accessPtr memMap LookupReC mp
    -- Real part of complex node
         imAt :: Int -> String -> String
         imAt = accessPtr memMap LookupImC mp
         infix 9 `at`, `imAt`, `reAt`
         genCode :: Int -> Code -- From node id to codes
         genCode n =
          let (shape, node) = retrieveInternal n mp
              elementType nId = retrieveElementType nId mp
          in case node of
                Var _ -> [ ]
                DVar _ -> error "DVar should not be here"
                Const val -> for i n [n `at` i <<- show val]
                Sum _ args -> let arName = map (`at` i) args 
                              [ Name nId := Add False  -- no signed wrap
                                                False  -- no unsigned wrap
                                                (LocalReference int (Name argName!!0))
                                                (LocalReference int (Name argName!!1))
                                                []]
                   -- | elementType n == R ->
                     --   let sumAt i = intercalate " + " $ map (`at` i) args
                       --  in LLVM.AST.FAdd for i n [n `at` i <<- sumAt i
                       --  ]
                         -- need to generate an address calculation and load for each input
                         -- each input also needs a temporary variable to load into (can use nodeIds because they are unique)
                         --   for inputs you need both the "node" and the "args" one by one
                         --   but don't load the same input twice!  so convert to a Set and back to a List
                         --   for output you can just use node
                         -- (count args - 1) Fadds
                         -- one address calculation and one store for the output
                Mul _ args -> error "Mul not implemented"
                    -- | elementType n == R ->
                      --  let prodAt i = intercalate " * " $ map (`at` i) args
                        -- in for i n [n `at` i <<- prodAt i]
                Power x arg  -> error "Power not implemented"
                    -- | elementType n == R ->
                       -- let powerAt i =
                         --       "pow(" ++ arg `at` i ++ "," ++ show x ++ ")"
                        -- in for i n [n `at` i <<- powerAt i]
                Neg _ arg -> [ Name nId := Add False  -- no signed wrap
                                           False  -- no unsigned wrap
                                           (LocalReference int (ConstantOperand (C.Int 32 0)))
                                           (LocalReference int (Name arg))
                                           []]
                    -- | elementType n == R ->
                    --    let negAt i = "-" ++ arg `at` i
                      --   in for i n [n `at` i <<- negAt i]
                         -- need to generate address, load, negate, address and store (and two temporary)
                   -- | elementType n == C ->
                     --   let negReAt i = "-" ++ (arg `reAt` i)
                       --     negImAt i = "-" ++ (arg `imAt` i)
                         --in for i n [n `reAt` i <<- negReAt i] ++
                           -- for i n [n `imAt` i <<- negImAt in ]
                Scale _ scalar arg -> error "Scale should not be here"
                -- MARK: only apply to R
                Div arg1 arg2  -> error "Div not implemented"
                  --  let divAt i = arg1 `at` i ++ " / " ++ arg2 `at` i
                    -- in for i n [n `at` i <<- divAt i]
                Sqrt arg -> error "for i n [n `at` i <<- "sqrt" ++ arg `at` i]"
                Sin arg -> error "for i n [n `at` i <<- "sin" ++ arg `at` i]"
                Cos arg -> error "for i n [n `at` i <<- "cos" ++ arg `at` i]"
                Tan arg -> error "for i n [n `at` i <<- "tan" ++ arg `at` i]"
                Exp arg -> error "for i n [n `at` i <<- "exp" ++ arg `at` i]"
                Log arg -> error "for i n [n `at` i <<- "log" ++ arg `at` i]"
                Sinh arg -> error "for i n [n `at` i <<- "sinh" ++ arg `at` i]"
                Cosh arg -> error "for i n [n `at` i <<- "cosh" ++ arg `at` i]"
                Tanh arg -> error "for i n [n `at` i <<- "tanh" ++ arg `at` i]"
                Asin arg -> error "for i n [n `at` i <<- "asin" ++ arg `at` i]"
                Acos arg -> error "for i n [n `at` i <<- "acos" ++ arg `at` i]"
                Atan arg -> error "for i n [n `at` i <<- "atan" ++ arg `at` i]"
                Asinh arg -> error "for i n [n `at` i <<- "asinh" ++ arg `at` i]"
                Acosh arg -> error "for i n [n `at` i <<- "acosh" ++ arg `at` i]"
                Atanh arg -> error "for i n [n `at` i <<- "atanh" ++ arg `at` i]"
                -- MARK: Complex related
                RealImag arg1 arg2 -> error "RealImag should not be here"
                RealPart arg -> error "RealPart should not be here"
                ImagPart arg -> error "ImagPart should not be here"
                InnerProd _ arg1 arg2 -> error "InnerProd should not be here"
                Piecewise marks condition branches -> error "Piecewise should not be here"
                Rotate [amount] arg -> error "Rotate 1D should not be here"
                Rotate [amount1, amount2] arg -> error "Rotate 2D should not be here"
                Rotate [amount1, amount2, amount3] arg -> error "Rotate 3D should not be here"