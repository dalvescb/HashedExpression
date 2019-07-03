{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}

module HashedToC where

import HashedDerivative
import HashedExpression
import HashedInstances
import HashedInterp
import HashedPacking
import HashedSimplify

import qualified Data.Text as T
import Data.IntMap (IntMap)
import qualified Data.IntMap as I
import Data.List as L

--import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C

{- Support different memory mappings
 - An optimized memory mapping would overwrite used nodes to allow a minimized memTotal
 -}
class MemMap a
    where
    mkMap :: Expression -> a
    memOffset ::
           a -- memory map created above
        -> Node -- node in the expression
        -> [Int] -- indices into array
        -> Maybe Int -- global offset into heap
    memIdentifier :: a -> Node -> T.Text
    memIdentType :: a -> Node -> T.Text
    memTotal :: a -> Int

{- DebugMemMap retains all intermediate nodes. Very innefficient, but useful for debugging
 -}
data DebugMemMap = DebugMemMap Int -- total size
                               (IntMap (Int, Dims))  -- map from node to offset/node dimensions
    deriving (Show, Eq)

instance MemMap DebugMemMap where
  mkMap (Expression node exprs) =
    let
      nodes :: IntMap ExpressionEdge
      nodes = I.fromListWithKey (\k a b -> if a == b then a else error $ "HashedToC.DebugMemMap.mkMap " ++ show (k,a,b))
              $ depthFirst exprs node

      (totalSize :: Int,nodeOffsets :: IntMap (Int,Dims)) = I.mapAccum
        (\accum edge -> let
                           dims   = dimSize edge
                           accum' = dimProd dims + accum
                        in (accum',(accum,dims))
        )
        0
        nodes

      dimSize :: ExpressionEdge -> Dims 
      dimSize (DVar _dims _name) = error $ "HashedToC.DebugMemMap.mkMem found DVar"
      dimSize (RelElem _ _ _) = error $ "HashedToC.DebugMemMap.mkMem found RelElem"
      dimSize (Var dims _name) = dims
      dimSize (Const dims _d) = dims
      dimSize (Op dims (FT _) _args) = dims -- real/imaginary (can be thought of as a dimension)
      dimSize (Op dims _op _args) = dims

    in DebugMemMap totalSize nodeOffsets

  memOffset (DebugMemMap _size offMap) node idxs =
    let
      idxOffset :: [Int] -> Dims -> Int
      idxOffset idxs dims = case dims of
                              Dim0 -> 0
                              Dim1 d            -> sum $ zipWith (*) idxs $ [product $ take i (1:d:[])     | i <- [1..]]
                              Dim2 (d1,d2)      -> sum $ zipWith (*) idxs $ [product $ take i [1,d1,d2]    | i <- [1..]]
                              Dim3 (d1,d2,d3)   -> sum $ zipWith (*) idxs $ [product $ take i [1,d1,d2,d3] | i <- [1..]]
                              _ -> error $ "HashedToC.DebugMemMap.memOffset dimension " ++ show dims ++ " not implemented"
    in case I.lookup node offMap of
         Nothing -> Nothing
         Just (offset,dims) -> Just $ offset + idxOffset idxs dims

  memIdentifier _ _ = T.pack "ptr" -- use a single pointer for all of memory

  memIdentType _ _ = T.pack "Number"

  memTotal (DebugMemMap size _offMap) = size


{- generate c code for an entire expression assuming memory layout provided by MemMap
 -}
genExpressionComp :: forall mem . MemMap mem => mem -> Expression -> [T.Text]
genExpressionComp mem (Expression node exprs) =
  let
    sortedNodes = topSort exprs node
  in concatMap (genUnitComp mem exprs) sortedNodes
-- TODO remove list overhead and just concat text together in genUnitComp for possible performance improvement

{- generate c code for unit computations (one node at a time) assuming memory layout provided by MemMap
 -}
genUnitComp :: forall mem . MemMap mem => mem -> Internal -> Node -> [T.Text]
genUnitComp mem exprs node =
  let
    ptrArg :: Node -> [Int] -> T.Text
    ptrArg arg idxs = case I.lookup arg exprs of
                        Just (Const Dim0 d) -> T.pack $ "(" ++ show d ++ ")"
                        Just (Const _ _d) -> error $ "HashedToC.genUnitComp.ptrArg higher-dim const not handled"
                        -- TODO adjust to handle multiple dims (consts should be allocated in memory)?
                        _ -> (T.pack "( *((")
                             `T.append` memIdentType mem arg
                             `T.append` (T.pack "*)(")
                             `T.append` (memIdentifier mem arg)
                             `T.append` (T.pack " + ")
                             `T.append` (T.pack $ show $ memOffset mem arg idxs)
                             `T.append` (T.pack "*sizeof(")
                             `T.append` memIdentType mem arg
                             `T.append` (T.pack ") ) ))")
                             -- TODO check for out-of-bounds here?

    ptrAssign :: Node -> [Int] -> T.Text -> [T.Text]
    ptrAssign arg idxs rvalue  = [(T.pack "*((")
                                  `T.append` (memIdentType mem arg)
                                  `T.append` (T.pack "*)(")
                                  `T.append` (memIdentifier mem arg)
                                  `T.append` (T.pack " + ")
                                  `T.append` (T.pack $ show  $ memOffset mem arg idxs)
                                  `T.append` (T.pack "*sizeof(")
                                  `T.append` (memIdentType mem arg)
                                  `T.append` (T.pack ") )) = ")
                                  `T.append` rvalue
                                  `T.append` (T.pack ";")]
                                 -- TODO adjust to handle mutliple dimensions?

    genSum :: Dims -> [Node] -> [T.Text]
    genSum dims nodes = case dims of
                               Dim0 -> ptrAssign node [] $
                                       T.concat $
                                       intersperse (T.pack " + ") $
                                       map ((flip ptrArg) []) nodes
                               _ -> error $ "HashedToC.genUnitComp.genSum unimplemented dimension"
                               -- TODO create for loop for computing higher dimensions

    genProduct :: Dims -> [Node] -> [T.Text]
    genProduct dims nodes = case dims of
                              Dim0 -> ptrAssign node [] $
                                      T.concat $
                                      intersperse (T.pack " * ") $
                                      map ((flip ptrArg) []) nodes
                              _ -> error $ "HashedToC.genUnitComp.genProduct unimplemented dimension"
                              -- TODO create for loop for computing higher dimensions
    genDim0UnaryOps :: OpId -> Node -> [T.Text]
    genDim0UnaryOps op arg =
      let
        opText = case op of
                   Abs -> T.pack "abs"
                   Signum -> T.pack "signum"
                   Sqrt -> T.pack "sqrt"
                   Sin -> T.pack "sin"
                   Cos -> T.pack "cos"
                   Tan -> T.pack "tan"
                   Exp -> T.pack "exp"
                   Log -> T.pack "log"
                   Sinh -> T.pack "sinh"
                   Cosh -> T.pack "cosh"
                   Tanh -> T.pack "tanh"
                   Asin -> T.pack "asin"
                   Acos -> T.pack "acos"
                   Atan -> T.pack "atan"
                   Asinh -> T.pack "asinh"
                   Acosh -> T.pack "acosh"
                   Atanh -> T.pack "atanh"
                   _ -> error $ "HashedToC.genUnitComp.genDim0Ops unknown op found " ++ show op
      in ptrAssign node [] $ T.append opText $ ptrArg arg []

    genDim0Div :: Node -> Node -> [T.Text]
    genDim0Div arg1 arg2 = ptrAssign node [] $ (ptrArg arg1 []) `T.append` (T.pack " / ") `T.append` (ptrArg arg2 [])

  in case I.lookup node exprs of
       Nothing -> []
       Just (Var _dims _nmae) -> []
       Just (DVar _dims _name) -> error $ "HashedToC.genUnitComp found DVar"
       Just (RelElem _ _ _)  -> error $ "HashedToC.genUnitComp found RelElem"
       Just (Const _dim _d) ->  [] -- ptrAssign node [] $ ptrArg node []
                                   -- TODO assign this to a pointer or just add in-place (as ptrArg currently does)
       Just (Op dims Sum args) -> genSum dims args
       Just (Op dims Prod args) -> genProduct dims args
       Just (Op Dim0 op [arg]) -> genDim0UnaryOps op arg
       Just (Op Dim0 Div [arg1,arg2]) -> genDim0Div arg1 arg2
       -- TODO finish unimplemented ops
       Just op -> error $ "HashedToC.genUnitComp unimplemented operation found " ++ show (node,op)
