(c) 2018 Curtis D'Alves
This module generates ipopt c code from HashedExpression
\begin{code}
{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}
module HashedToIpopt where

import HashedExpression
import HashedInstances
import HashedPacking
import HashedInterp
import HashedDerivative
import HashedSimplify
import HashedToC

import qualified Data.Array.Unboxed as U
import Data.List as L
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as I
import qualified Data.Map as Map
import Debug.Trace
import Data.Maybe as Maybe

import qualified Data.ByteString.Char8 as C
\end{code}

\begin{code}
hashedToIpopt :: Expression -> ValMaps -> [String]
hashedToIpopt (Expression n1 e1) vals =
  let
    comp = []
  in comp
\end{code}

\begin{code}
[t0,t1,t2] = map var ["t0","t1","t2"]

y0 :: Scalar
y0 = t0 + t1
\end{code}

\section{Memory Mapping}

The following code provides a mapping of Var's in an Expression to memory allocated on
the heap

\begin{code}
data IpoptMemMap = IpoptMemMap { immVarMap :: IntMap (Int,Dims,C.ByteString)
                               , immMemSize :: Int }
  deriving (Show,Eq)
\end{code}

Generate a memory map for JUST THE VARIABLES (i.e Var) in an expression.
Offsets in immVarMap are just an index (not multiplied by sizeof(..))
\begin{code}
instance MemMap IpoptMemMap where
  -- generate a map of just vars indexed by lexigraphically sorted var names
  mkMap (Expression node exprs) =
    let
      nodes = I.fromListWithKey  (\ k a b -> if a == b then a
                                             else error $ "HashedToC.DMM.mkMap " ++ show (k,a,b))
              $ depthFirst exprs node
      sortedVars = L.sortOn (snd . snd) $ I.toList $ I.mapMaybe pullVars nodes
      pullVars exprE = case exprE of
                      (Var dim name) -> Just (dim,name)
                      _               -> Nothing
      varMap = case sortedVars of
                 ((k,(d,n)):sortedVars') -> I.fromList $ scanl aggregateOffset (k,(0,d,n)) sortedVars'
                 [] -> error "No vars found in mkMap"
      aggregateOffset (_,(offset,dim,_)) (k',(dim',name')) = (k',(offset + dimProd dim'
                                                                 ,dim'
                                                                 ,name'))

      memSize = case I.foldl findMaxOffset (0,Dim0) varMap of
                  (offset,dim) -> offset + dimProd dim
      findMaxOffset (offset,dim) (offset',dim',_) = if offset > offset'
                                                    then (offset,dim)
                                                    else (offset',dim')
    in IpoptMemMap varMap memSize

  -- returns just an offset to the BEGINNING INDEX OF A VAR (not multiplied by sizeof(NUM))
  -- i.e idxs is unused
  memOffset memMap node idxs = case (I.lookup node (immVarMap memMap),idxs) of
    (Just (offset,Dim0,_),_)       -> Just offset
    (Just (offset,Dim1 d1,_),[i0]) -> if i0 > d1
                                      then error $ "memOffset index: " ++ show i0 ++ " out of bounds"
                                      else Just $ offset + i0
    (Nothing,_) -> Nothing

  memTotal memMap = immMemSize memMap
\end{code}

Generate malloc and assign statements for initial values at offsets given by IpoptMemMap
\begin{code}
preloadVars :: IpoptMemMap -> ValMaps -> [String]
preloadVars mem (ValMaps m0 m1 m2 m3 m4) =
  let
    lookupVals :: (Int,Dims,C.ByteString) -> [(Int,Double)]
    lookupVals (offset,Dim0,name) = case Map.lookup name m0 of
                                     Just val -> [(offset,val)]
                                     Nothing -> error $ "preloadVars failed to lookup Var: "
                                                ++ C.unpack name
    lookupVals (offset,Dim1 _,name) = case Map.lookup name m1 of
                                     Just uarray -> map (\(i,val) -> (offset+i,val))
                                                    $ zip [0..]
                                                    $ b2a uarray
                                     Nothing -> error $ "preloadVars failed to lookup Var: "
                                                ++ C.unpack name
    lookupVals (offset,Dim2 _,name) = case Map.lookup name m2 of
                                     Just uarray -> map (\(i,val) -> (offset+i,val))
                                                    $ zip [0..]
                                                    $ concat
                                                    $ b2a uarray
                                     Nothing -> error $ "preloadVars failed to lookup Var: "
                                                ++ C.unpack name
    lookupVals (offset,Dim3 _,name) = case Map.lookup name m3 of
                                     Just uarray -> map (\(i,val) -> (offset+i,val))
                                                    $ zip [0..]
                                                    $ concat
                                                    $ concat
                                                    $ b2a uarray
                                     Nothing -> error $ "preloadVars failed to lookup Var: "
                                                ++ C.unpack name
    lookupVals (offset,Dim4 _,name) = case Map.lookup name m4 of
                                     Just uarray -> map (\(i,val) -> (offset+i,val))
                                                    $ zip [0..]
                                                    $ concat
                                                    $ concat
                                                    $ concat
                                                    $ b2a uarray
                                     Nothing -> error $ "preloadVars failed to lookup Var: "
                                                ++ C.unpack name

    -- #TODO replace double with typedef?
    printAssign :: (Int,Double) -> [Char]
    printAssign (off,val) = "(*(double*)(ptr + " ++ show off ++ "*sizeof(double))) = " ++ show val ++ ";"

    varMap :: [(Int,Dims,C.ByteString)]
    varMap = map snd $ I.toList $ immVarMap mem

    -- TODO replace double with typedef?
    mallocStatement = ("double *ptr = malloc(sizeof(double) * " ++ show (immMemSize mem) ++ ");")
    assignStatements = (map printAssign $ concat $ map lookupVals varMap)
  in mallocStatement : assignStatements
\end{code}

\section{Expression code generation}

\begin{code}
-- staticComp :: forall mem . MemMap mem => mem -> Scalar -> [String]
-- staticComp mem (Scalar expr) =
--   let
--     (Expression node exprs) = expr

--     nodes :: [Node]
--     nodes = topSort exprs node

--     varArg :: Node -> [Int] -> String
--     varArg nd idxs = case I.lookup nd exprs of
--       Just (Var _dims _name)  -> "( *((double*)(ptr + "
--                                  ++ show (Maybe.fromJust $ memOffset mem nd idxs)
--                                  ++  ")) )"
--       Just (DVar _dims _name) -> error "DVar found in staticComp"
--       Just (RelElem _ _ _)    -> error "RelElem found in staticComp"
--       Just (Const dims val)   -> "(" ++ show val ++ ")"           -- FIXME is this suitable for > Dim0?
--       Just (Op _ _ _)         -> "(" ++ tmpVar nd ++ ")"
--       Nothing -> error "Bad node: " ++ show nd ++ " given in varArg"

--     tmpVar :: Node -> String
--     tmpVar nd = "tmp_" ++ show nd

--     varAssign :: Node -> String -> String
--     varAssign nd strVal = "const double " ++ tmpVar nd ++ " = " ++ strVal ++ ";"

--     comp' :: Node -> [String]
--     comp' n = case I.lookup n exprs of
--                 Just x -> comp x n
--                 Nothing -> []

--     comp :: ExpressionEdge -> Node -> [String]
--     -- no work to be done for variables
--     comp (Var _dims _name) _n  = []
--     comp (Const _dims _val) _n = []
--     comp (DVar _dims _name) _n = error $ "HashedToIpopt.staticComp found DVar"
--     comp (RelElem _ _ _) _n = error $ "HashedToIpopt.staticComp found RelElem"
--     -- compute oeprations
--     comp (Op dims Sum args) n =
--       case dims of
--         Dim0 -> [varAssign n ]
--   in concatMap comp' nodes
\end{code}

\section{Ipopt Overhead}
