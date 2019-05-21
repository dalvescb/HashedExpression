,Generate C code to evaluate expressions.
\begin{code}
{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}
module HashedToC where

import HashedExpression
import HashedInstances
import HashedPacking
import HashedInterp
import HashedDerivative
import HashedSimplify

import Data.Array.Unboxed
import Data.List as L
import Data.IntMap (IntMap)
import qualified Data.IntMap as I
import qualified Data.Map as Map
import Debug.Trace

--import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
\end{code}

\begin{code}
class HashedToC a where
  toC :: Packing -> a -> String
\end{code}

\begin{code}
instance HashedToC Scalar where
  toC _packing (Scalar _e) = error "toC Scalar"
\end{code}
\begin{code}
instance HashedToC OneD where
  toC _packing (OneD _e) = error $ "toC OneD"
\end{code}
\begin{code}
instance HashedToC TwoD where
  toC _packing (TwoD _e) = error $ "toC TwoD"
\end{code}

\subsection{Memory Allocation}

We need different memory allocation strategies, depending on whether we are debugging
or not, and whether there are loops, NUMA memory, etc.

Class for creating a memory map.
\begin{code}
class (Show a) => MemMap a where
  mkMap :: Expression -> a
  memOffset  :: a          -- memory map created above
             -> Node       -- node in expression
             -> [Int]      -- indices into array
             -> Maybe Int  -- global offset into heap using local memory mapping
                           -- if there is a local mapping, |Nothing| if no memory is allocated
  memTotal :: a -> Int     -- amount of memory to allocate
\end{code}

Allocate memory for all intermediate variables.  This is probably only useful for debugging.
\begin{code}
data DebugMemMap = DMM (IntMap (Int{-offset-},Dims{-dims-})) Int
  deriving (Show,Eq)

prettyMem exprs (DMM nodeMap total) = ("DMM total size "++ show total) :
  [(take 10 (show offset ++ "               ")) ++ (take 45 (pretty' (exprs,node)  ++ "                                "))
                ++ "    " ++ show dims ++ "   " ++ show node
  | (node,(offset,dims)) <- L.sortBy (\ (_,(o1,_)) (_,(o2,_)) -> compare o1 o2) $ I.toList nodeMap]

instance MemMap DebugMemMap where
  mkMap (Expression node exprs) = let
      nodes = I.fromListWithKey  (\ k a b -> if a == b then a
                                             else error $ "HashedToC.DMM.mkMap " ++ show (k,a,b))
                                 $ depthFirst exprs node
      nodesSize = I.mapMaybe mapSize nodes

      (totalSize,nodeOffsetDims) = I.mapAccum
        (\ accum dims -> (memBuffer + accum + sizeOfDouble * (dimProd dims),(accum,dims)))
        0
        nodesSize

      mapSize (Var dims _name) = Just dims
      mapSize (DVar _dims _name) = error $ "HashedToC.DebugMemMap.mkMem found DVar"
      mapSize (RelElem _ _ _) = error $ "HashedToC.DebugMemMap.mkMem found RelElem"
      mapSize (Const dims _d) = Just dims
      mapSize (Op dims (FT _) _args) = Just $ dims -- real/imaginary (can be thought of as a dimension)
        -- CKA removed 2:dims to get this to compile, because these dims are used to calculate indices inside the array, and I don't think we access complex arrays element by element
      mapSize (Op dims _op _args) = Just dims

    in DMM nodeOffsetDims totalSize
    --in error $ show $ (length $ L.nub $ L.sort $ depthFirst exprs node,I.size nodeOffsetDims,depthFirst exprs node,nodeOffsetDims,totalSize)

  memOffset (DMM iMap _size) node idxs = case I.lookup node iMap of
    Nothing -> Nothing
    Just (offset,dims) -> Just $ memBuffer + offset + sizeOfDouble * (
      case dims of
        Dim0              -> 0 -- if there are no dimensions, there is still a number
        Dim1 d            -> sum $ zipWith (*) idxs $ [product $ take i (1:d:[])     | i <- [1..]]
        Dim2 (d1,d2)      -> sum $ zipWith (*) idxs $ [product $ take i [1,d1,d2]    | i <- [1..]]
        Dim3 (d1,d2,d3)   -> sum $ zipWith (*) idxs $ [product $ take i [1,d1,d2,d3] | i <- [1..]]
        Dim3SL1 (_d1,d2,d3) nK  -> sum $ zipWith (*) idxs $ [product $ take i [1,nK,d2,d3] | i <- [1..]]
        d -> error $ "memOffset not implemented "++show d
      )

  memTotal (DMM _ size) = size
\end{code}

\subsection{Computation}
As with memory maps, we need different versions of computation,
we will implement the most straight-forward versions first.

We should try to use MC to compare simpler versions against more complicated optimizing
versions of code generation.

Simple version:  Just generate all the computation we can, don't support threads.

CKA:  It isn't clear how to enforce interactions between code generation and memory allocation,
e.g. |simpleComp| will not support overwriting.
\begin{code}
simpleComp :: forall mem . MemMap mem => mem -> Expression -> [String]
simpleComp mem (Expression node exprs) = let

      ptrArg offset arg
        | Just (Const Dim0 d) <- I.lookup arg exprs
        = "(" ++ show d ++ ")"
        | Just (Const _ d) <- I.lookup arg exprs
        = "(" ++ show d ++ "/*suspect -- generate a pointer here, and in an earlier step fill in that memory*/)"
        | True
        = "( *((double*)(ptr + "++ memOffset' arg ++ offset ++") ))"
      ptrOut offset n = "*((double*)(ptr + " ++ memOffset' n ++ offset ++ ") ) = "

      memOffset' n = case memOffset mem n [] of
        Just x -> show x
        Nothing -> error $ "HashedToC.simpleComp missing node in memmap " ++ show (n,mem,pretty $ Expression node exprs,exprs)

      comp' :: Node -> [String]
      comp' n = case I.lookup n exprs of
         Just x -> comp x n
         Nothing -> []

      comp (Var _dims _name) _n = []
      comp (DVar _dims _name) _n = error $ "HashedToC.DebugMemMap.simpleComp found DVar"
      comp (RelElem _ _ _) _n = error $ "HashedToC.DebugMemMap.simpleComp found RelElem"
      comp (Const _ d) n = [ptrOut "" n ++ show d ++ ";"]
      comp (Op _ (Compound _) _) _n = ["/* Compound */"]
      comp (Op dims Sum args) n =
        case dims of
          Dim0 -> [ptrOut "" n ++ (concat $ intersperse " + " (map (ptrArg "") args)) ++ ";"]
          _ -> ["/* Sum in " ++ pretty (Expression n exprs) ++ " */"
               ,"{int i; for (i=0; i<" ++ show (dimProd dims) ++"; i++) {"
               ++ ptrOut " + 8*i " n ++ (concat $ intersperse " + " (map (ptrArg " + 8*i ") args)) ++ ";}}"]
      comp (Op Dim0 Prod args) n = [ptrOut "" n ++ (concat $ intersperse " * " (map (ptrArg "") args)) ++ ";"]
      comp (Op Dim0 op [arg]) n =
        [ptrOut "" n
          ++ (case op of
                Abs -> "abs"
                Signum -> "signum"
                Sqrt -> "sqrt"
                Sin -> "sin"
                Cos -> "cos"
                Tan -> "tan"
                Exp -> "exp"
                Log -> "log"
                Sinh -> "sinh"
                Cosh -> "cosh"
                Tanh -> "tanh"
                Asin -> "asin"
                Acos -> "acos"
                Atan -> "atan"
                Asinh -> "asinh"
                Acosh -> "acosh"
                Atanh -> "atanh"
                _ -> error $ "HashedToC.DebugMemMap.simpleComp []->[] Op found " ++ show op
             ) ++ "( * ((double*)(ptr + " ++ (memOffset' arg) ++ " )));" ]
      comp (Op Dim0 Div [arg1,arg2]) n =
        [ptrOut "" n ++ "( * (ptr + " ++ (memOffset' arg1) ++ " ))"
                ++ "/ ( * (ptr + " ++ (memOffset' arg2) ++ " ));" ]
      comp (Op Dim0 Dot [arg1,arg2]) n =
             ["/* Dot in "
              ++ show (Scalar $ Expression n exprs) ++ " */"
             ,"{double accum = 0; int i;"
             ,"for (i=0; i < " ++ show (dimProdE (exprs,arg1))
             ++ "; i++) { accum += " ++  ptrArg " + 8*i " arg1 ++ " * "
             ++ ptrArg " + 8*i " arg2 ++ "; } " ++ ptrOut "" n ++ " accum; }"]
      comp (Op dims ScaleV [arg1,arg2]) n =
        ["/* ScaleV in " ++ pretty (Expression n exprs) ++ " */"
        ,"{double scale = (" ++ ptrArg "" arg1 ++ ");"
        ,"int i;"
        ,"for (i=0; i<" ++ show (dimProd dims) ++"; i++) {"
        ,ptrOut " + 8*i " n ++ "scale * " ++ ptrArg " + 8*i " arg2 ++ ";}}"]
      comp (Op dims (Project ss) [arg]) n =
         case ss of
           (SSCrop [(low,high)] _) -> -- Crop  1D
             ["/* SSCrop 1D in " ++ pretty (Expression n exprs) ++ " */"
             ,"{int i;"
             ,"for (i=" ++ show low ++ "; i<=" ++ show high ++ "; i++) {"
             ,ptrOut (" + 8*(i - "++show low++")") n ++ " " ++ ptrArg " + 8*i " arg ++ ";}}"]
           (SSCrop [(low1,high1),(low2,high2)] _) -> -- Crop 2D
             let
               Dim2 (oldDimX,_) = getDimE exprs arg
               Dim2 (dimX,_) = dims
             in
               ["/* SSCrop 2D in " ++ pretty (Expression n exprs) ++ " */"
               ,"{int y,x;"
               ,"for (y=" ++ show low2 ++ "; y<=" ++ show high2 ++ "; y++) {"
               ,"for (x=" ++ show low1 ++ "; x<=" ++ show high1 ++ "; x++) {"
               ,ptrOut (" + ((y-"++show low2++")*" ++ show dimX ++ "+ (x-"++show low1++"))*8") n
                ++ ptrArg (" + (y*" ++ show oldDimX ++ "+ x)*8") arg ++ ";}}}"]
           (SSCrop [(low1,high1),(low2,high2),(low3,high3)] _) -> -- Crop 3D
             let
               Dim3 (oldDimX,oldDimY,_) = getDimE exprs arg
               Dim3 (dimX,dimY,_) = dims
             in
             ["/* SSCrop 3D in " ++ pretty (Expression n exprs) ++ " */"
             ,"{int x,y,z;"
             ,"for (z=" ++ show low3 ++ "; z<=" ++ show high3 ++ "; z++) {"
             ,"for (y=" ++ show low2 ++ "; y<=" ++ show high2 ++ "; y++) {"
             ,"for (x=" ++ show low1 ++ "; x<=" ++ show high1 ++ "; x++) {"
             ,ptrOut (" + ((z-"++show low3++")*" ++ show (dimY*dimX)
                    ++ "+ (y-"++show low2++")*" ++ show dimX
                    ++ "+ (x-"++show low1++"))*8 ") n
              ++ ptrArg (" + (z*" ++ show (oldDimY*oldDimX)
              ++ "+ y*" ++ show oldDimX ++ "+ x)*8 ")  arg ++ ";}}}}"]

           (SSNyquist [(p,(start,stop))]) -> -- NyQuist 1D
             let
               Dim1 oldDimX = getDimE exprs arg
             in
               ["/* SSNyquist 1D in " ++ pretty (Expression n exprs) ++ " */"
               ,"{int i;"
               ,"for (i=" ++ show start ++ "; i<" ++ show (oldDimX - stop) ++ "; i +=" ++ show p ++ ") {"
               ,ptrOut " + 8*i " n ++ " " ++ ptrArg " + 8*i " arg ++ ";}}"]
           (SSNyquist [(p1,(start1,stop1)),(p2,(start2,stop2))]) -> -- NyQuist 2D
             let
               Dim2 (oldDimX,oldDimY) = getDimE exprs arg
             in
               ["/* SSNyquist 2D in " ++ pretty (Expression n exprs) ++ " */"
               ,"{int y,x;"
               ,"for (y=" ++ show start2 ++ "; y<" ++ show (oldDimY - stop2) ++ "; y +=" ++ show p2 ++ ") {"
               ,"for (x=" ++ show start1 ++ "; x<" ++ show (oldDimX - stop1) ++ "; x +=" ++ show p1 ++ ") {"
               ,ptrOut (" + (y*" ++ show oldDimX ++ "+ x)*8") n ++ " " ++ ptrArg (" + (y*"
                ++ show oldDimX ++ "+ x)*8") arg ++ ";}}}"]
           (SSNyquist [(p1,(start1,stop1)),(p2,(start2,stop2)),(p3,(start3,stop3))]) -> -- NyQuist 3D
             let
               Dim3 (oldDimX,oldDimY,oldDimZ) = getDimE exprs arg
             in
               ["/* NyQuist 3D in " ++ pretty (Expression n exprs) ++ " */"
               ,"{int x,y,z;"
               ,"for (z=" ++ show start3 ++ "; z<" ++ show (oldDimZ - stop3) ++ "; z+=" ++ show p3 ++ ") {"
               ,"for (y=" ++ show start2 ++ "; y<" ++ show (oldDimY - stop2) ++ "; y+=" ++ show p2 ++ ") {"
               ,"for (x=" ++ show start1 ++ "; x<" ++ show (oldDimX - stop1) ++ "; x+=" ++ show p1 ++ ") {"
               ,ptrOut (" + (z*" ++ show (oldDimY*oldDimX) ++ "+ y*" ++ show oldDimX ++ "+ x)*8 ") n
                ++ " " ++ ptrArg (" + (z*" ++ show (oldDimY*oldDimX) ++ "+ y*" ++ show oldDimX
                ++ "+ x)*8 ")  arg ++ ";}}}}"]

           (SSList2D (SparseList2D len Dii1 toKeep _)) ->
             let
              Dim3 (dimX,_,_) = getDimE exprs arg
             in
               ["/* SSList1D in " ++ pretty (Expression n exprs) ++ " */"
               ,"{int i;"
               ,"int toKeep[] = {" ++ (concat $ intersperse "," $ map (show . ( * (8 * dimX) )) $ elems toKeep) ++ "};"
               ,"for (i=0; i<" ++ show len ++ "; i ++) {"
               ,"memcpy(ptr + "++(memOffset' n) ++ "+i*8, "
                               ++(memOffset' arg) ++ " + toKeep[i]*8, "
                               ++show dimX ++ "*8);}}"]
           (SSList3D (SparseList3D len Diii1 toKeepY _ toKeepZ _)) -> let
               [dimX,dimY,_] = dimList $ getDimE exprs arg
               linIdx (y,z) = 8 * ( y * dimX + z * dimX * dimY )
             in
               ["/* SSList2D in " ++ pretty (Expression n exprs) ++ " */"
               ,"{int i;"
               ,"int toKeep[] = {" ++ (concat $ intersperse "," $ map (show . linIdx) $ zip (elems toKeepY) (elems toKeepZ) ) ++ "};"
               ,"for (i=0; i<" ++ show len ++ "; i ++) {"
               ,"memcpy(ptr + "++(memOffset' n) ++ "+i*8, "
                               ++(memOffset' arg) ++ " + toKeep[i]*8,"
                               ++show dimX ++ "*8);}}"]

           x -> error $ "HTC.simpleComp not implemented " ++ show x


      comp (Op dims (Inject ss) [arg]) n =
         case ss of
           (SSCrop [(low,high)] [d]) -> -- Crop  1D
             ["/* Inject SSCrop 1D in " ++ pretty (Expression n exprs) ++ " */"
             ,"{int i;"
             ,"for (i=0; i<" ++ show d ++ "; i++) {"
             ,ptrOut (" + 8*i") n ++ " i<" ++ show low ++ " || i>" ++ show (d-high-1) ++ "? 0 : "
              ++ ptrArg (" + 8*(i-" ++ show low ++ ")") arg ++ ";}}"]
           (SSCrop [(low1,high1),(low2,high2)] [fdX,fdY]) -> -- Crop 2D
             let
               Dim2 (oldDimX,_) = getDimE exprs arg
             in
               ["/* SSCrop 2D in " ++ pretty (Expression n exprs) ++ " */"
               ,"{int y,x;"
               ,"for (y=0; y<=" ++ show fdY ++ "; y++) {"
               ,"for (x=0; x<=" ++ show fdX ++ "; x++) {"
               ,ptrOut (" + ((y*" ++ show fdX ++ "+ x*8") n
                ++    "x<" ++ show low1 ++ "|| x>"++ show (fdX-high1-1)
                ++ "|| y<" ++ show low2 ++ "|| y>"++ show (fdY-high2-1) ++ "? 0 :"
                ++ ptrArg (" + ((y-"++show low2++")*" ++ show oldDimX ++ "+ (x-"++show low1++"))*8") arg ++ ";}}}"]
           (SSCrop [(low1,high1),(low2,high2),(low3,high3)] [fdX,fdY,fdZ]) -> -- Crop 3D
             let
               Dim3 (oldDimX,oldDimY,_) = getDimE exprs arg
             in
             ["/* SSCrop 3D in " ++ pretty (Expression n exprs) ++ " */"
               ,"{int x,y,z;"
               ,"for (z=0; z<=" ++ show fdZ ++ "; z++) {"
               ,"for (y=0; y<=" ++ show fdY ++ "; y++) {"
               ,"for (x=0; x<=" ++ show fdX ++ "; x++) {"
               ,ptrOut ("+ z*" ++ show (fdY*fdX) ++ " + y*" ++ show fdX ++ "+ x*8") n
                ++    "x<" ++ show low1 ++ "|| x>"++ show (fdX-high1-1)
                ++ "|| y<" ++ show low2 ++ "|| y>"++ show (fdY-high2-1)
                ++ "|| z<" ++ show low3 ++ "|| z>"++ show (fdZ-high3-1) ++ "? 0 :"
                ++ ptrArg ("((z-"++show low3++")*" ++ show (oldDimY*oldDimX) ++ " + (y-"++show low2++")*"
                          ++ show oldDimX ++ "+ (x-"++show low1++"))*8") arg ++ ";}}}"]

           (SSNyquist [(p,(start,stop))]) -> -- NyQuist 1D
             let
               Dim1 dim = dims
             in
               ["/* SSNyquist 1D in " ++ pretty (Expression n exprs) ++ " */"
               ,"{int i;"
               ,"for (i=0; i<" ++ show dim ++ "; i++) {"
               ,ptrOut " + 8*i " n
                  ++ "i>=" ++ show start ++ " && i<= " ++ show (dim-stop)
                  ++  " && (i-"++show start ++ ")% "++show p++"==0 ?"
                  ++ ptrArg (" + 8*(i/"++ show p ++")") arg ++ " : 0;}}"]
           (SSNyquist [(p1,(start1,stop1)),(p2,(start2,stop2))]) -> -- NyQuist 2D
             let
               cDimX = start1 + (div dimX p1) + stop1
               Dim2 (dimX,dimY) = dims
             in
               ["/* SSNyquist 2D in " ++ pretty (Expression n exprs) ++ " */"
               ,"{int y,x;"
               ,"for (y=0; y<" ++ show dimY ++ "; y ++) {"
               ,"for (x=0; x<" ++ show dimX ++ "; x ++) {"
               ,ptrOut (" + (y*" ++ show dimX ++ "+ x)*8") n
                  ++ "   x>=" ++ show start1 ++ " && x<=" ++ show (dimX-stop1)
                  ++ "&& y>=" ++ show start2 ++ " && y<=" ++ show (dimY-stop2)
                  ++ "&& (x-" ++ show start1 ++ ")%" ++ show p1 ++ "==0 && (y-" ++ show start2 ++ ")%"++ show p2 ++ "==0 ?"
                  ++ ptrArg (" + ((y/" ++ show p2 ++ ")*" ++ show cDimX ++ "+ (x/"++show p1 ++"))*8") arg ++ ": 0;}}}"]
           (SSNyquist [(p1,(start1,stop1)),(p2,(start2,stop2)),(p3,(start3,stop3))]) -> -- NyQuist 3D
             let
               Dim3 (dimX,dimY,dimZ) = dims
               cDimX = start1 + (div dimX p1) + stop1
               cDimY = start2 + (div dimX p2) + stop2
             in
               ["/* NyQuist 3D in " ++ pretty (Expression n exprs) ++ " */"
               ,"{int x,y,z;"
               ,"for (z=0; z<" ++ show dimZ ++ "; z ++) {"
               ,"for (y=0; y<" ++ show dimY ++ "; y ++) {"
               ,"for (x=0; x<" ++ show dimX ++ "; x ++) {"
               ,ptrOut (" + (z*" ++ show (dimX*dimY) ++ "+ y*" ++ show dimX ++ "+ x)*8") n
                  ++ "   x>=" ++ show start1 ++ " && x<=" ++ show (dimX-stop1)
                  ++ "&& y>=" ++ show start2 ++ " && y<=" ++ show (dimY-stop2)
                  ++ "&& z>=" ++ show start3 ++ " && z<=" ++ show (dimZ-stop3)
                  ++ "&& (x-" ++ show start1 ++ ")%" ++ show p1 ++ "==0 "
                  ++ "&& (y-" ++ show start2 ++ ")%" ++ show p2 ++ "==0 "
                  ++ "&& (z-" ++ show start3 ++ ")%" ++ show p3 ++ "==0 ?"
                  ++ ptrArg ("+ ((z/" ++ show p3 ++ ")*" ++ show (cDimY*cDimX) ++
                          "+  (y/" ++ show p2 ++ ")*" ++ show cDimX ++
                          "+  (x/" ++ show p1 ++ "))*8") arg ++ ": 0;}}}"]

           (SSList2D (SparseList2D len Dii1 toKeep _)) -> let
                Dim2 (dimX,_) = dims
              in
                ["/* SSList1D in " ++ pretty (Expression n exprs) ++ " */"
                ,"{int i;"
                ,"int toKeep[] = {" ++ (concat $ intersperse "," $ map (show . (*8)) $ elems toKeep) ++ "};"
                ,"memset (ptr + " ++ (memOffset' n) ++ ", 0 "
                          ++ show (dimProd dims) ++ ");"
                ,"for (i=0; i<" ++ show len ++ "; i ++) {"
                ,"memcpy(ptr + "++(memOffset' n) ++ "+toKeep[i]*8, "
                                ++(memOffset' arg) ++ " + i*8, "
                                ++show dimX ++ "*8);}}"]

           (SSList3D (SparseList3D len Diii1 toKeepY _ toKeepZ _)) -> let
               Dim3 (dimX,_,_) = dims
               linIdx (x,y) = 8 * ( x + y * dimX)
             in
               ["/* SSList2D in " ++ pretty (Expression n exprs) ++ " */"
               ,"{int i;"
               ,"int toKeep[] = {" ++ (concat $ intersperse "," $ map (show . linIdx) $ zip (elems toKeepY) (elems toKeepZ)) ++ "};"
               ,"memset (ptr + " ++ (memOffset' n) ++ ", "
                          ++ show (dimProd dims) ++ ", 0);"
               ,"for (i=0; i<" ++ show len ++ "; i ++) {"
               ,"memcpy(ptr + "++(memOffset' n) ++ "+toKeep[i]*8, "
                               ++(memOffset' arg) ++ " + i*8,"
                               ++show dimX ++ "*8);}}"]

           x -> error $ "HTC.simpleComp not implemented " ++ show x

      comp (Op (Dim2 (d1,d2)) (Transpose SCR) [arg]) n =
        ["/* 2D row column transpose on " ++ pretty (Expression n exprs) ++ "*/"
        ,"{int i,j;"
        ,"for (j=0; j<" ++ show d1 ++ "; j++) "
        ,"for (i=0; i<" ++ show d2 ++ "; i++) "
        ,ptrOut (" + (8*i)*" ++ show d1 ++ " + j*8 ") n
          ++ ptrArg (" + (8*j)*" ++ show d2 ++ " + 8*i") arg ++ ";"
        ,"}"]
      comp (Op (Dim3 (d1,d2,d3)) (Transpose SCRS) [arg]) n =
        ["/* 3D row column transpose  (swap columns with rows) on " ++ pretty (Expression n exprs) ++ "*/"
         ,"{int i,j,k;"
        ,"for (k=0; k<" ++ show d3 ++ "; k++) "
        ,"for (j=0; j<" ++ show d1 ++ "; j++) "
        ,"for (i=0; i<" ++ show d2 ++ "; i++) "
        ,ptrOut (" + (8*k)*" ++ show d2 ++ "*" ++ show d1 ++ " + 8*i*" ++ show d1 ++ " + 8*j ") n
          ++ ptrArg (" + (8*k)*" ++ show d2 ++ "*" ++ show d1 ++  " + 8*j*" ++ show d2 ++ " + 8*i ") arg ++ ";"
        ,"}"]
      comp (Op (Dim3 (d2,d3,d1)) (Transpose SCSR) [arg]) n =
        ["/* 3D row column transpose  (swap columns with rows) on " ++ pretty (Expression n exprs) ++ "*/"
         ,"{int i,j,k;"
        ,"for (k=0; k<" ++ show d3 ++ "; k++) "
        ,"for (j=0; j<" ++ show d2 ++ "; j++) "
        ,"for (i=0; i<" ++ show d1 ++ "; i++) "
        ,ptrOut (" + (8*i)*" ++ show d3 ++ "*" ++ show d2 ++ " + 8*k*" ++ show d2 ++ " + j*8 ") n
          ++ ptrArg (" + (8*k)*" ++ show d2 ++ "*" ++ show d1 ++  " + 8*j*" ++ show d1 ++ " + 8*i ") arg ++ ";"
        ,"}"]
      comp (Op (Dim3 (d3,d1,d2)) (Transpose SSRC) [arg]) n =
        ["/* 3D row column transpose  (swap columns with rows) on " ++ pretty (Expression n exprs) ++ "*/"
         ,"{int i,j,k;"
        ,"for (k=0; k<" ++ show d3 ++ "; k++) "
        ,"for (j=0; j<" ++ show d2 ++ "; j++) "
        ,"for (i=0; i<" ++ show d1 ++ "; i++) "
        ,ptrOut (" + (8*j)*" ++ show d1 ++ "*" ++ show d3 ++ " + 8*i*" ++ show d3 ++ " + k*8 ") n
          ++ ptrArg (" + (8*k)*" ++ show d2 ++ "*" ++ show d1 ++  " + 8*j*" ++ show d1 ++ " + 8*i ") arg ++ ";"
        ,"}"]
      comp (Op (Dim3 (d3,d2,d1)) (Transpose SSCR) [arg]) n =
        ["/* 3D row column transpose  (swap columns with rows) on " ++ pretty (Expression n exprs) ++ "*/"
         ,"{int i,j,k;"
        ,"for (k=0; k<" ++ show d3 ++ "; k++) "
        ,"for (j=0; j<" ++ show d2 ++ "; j++) "
        ,"for (i=0; i<" ++ show d1 ++ "; i++) "
        ,ptrOut (" + (8*i)*" ++ show d2 ++ "*" ++ show d3 ++ " + 8*j*" ++ show d3 ++ " + k*8 ") n
          ++ ptrArg (" + (8*k)*" ++ show d2 ++ "*" ++ show d1 ++  " + 8*j*" ++ show d1 ++ " + 8*i ") arg ++ ";"
        ,"}"]
      comp (Op (Dim3 (d1,d3,d2)) (Transpose SRSC) [arg]) n =
        ["/* 3D row column transpose  (swap columns with rows) on " ++ pretty (Expression n exprs) ++ "*/"
         ,"{int i,j,k;"
        ,"for (k=0; k<" ++ show d3 ++ "; k++) "
        ,"for (j=0; j<" ++ show d2 ++ "; j++) "
        ,"for (i=0; i<" ++ show d1 ++ "; i++) "
        ,ptrOut (" + (8*j)*" ++ show d3 ++ "*" ++ show d1 ++ " + 8*k*" ++ show d1 ++ " + i*8 ") n
          ++ ptrArg (" + (8*k)*" ++ show d2 ++ "*" ++ show d1 ++  " + 8*j*" ++ show d1 ++ " + 8*i ") arg ++ ";"
        ,"}"]


      comp (Op _dims RealImag [_arg1,_arg2]) n =
        ["/* RealImag in " ++ pretty (Expression n exprs) ++ " */"]
      comp (Op dims RealPart [arg]) n =
        case (I.lookup arg exprs) of
          Just (Op _dimft (FT _dir) [argFT]) ->
            case (I.lookup argFT exprs) of
              Just (Op _dimri RealImag [_rN,_iN]) ->
                  -- copy real part only
                  ["/* RealPart in " ++ pretty (Expression n exprs) ++ " */"
                  ,"{int i;"
                  ,"for (i=0; i<" ++ show (dimProd dims) ++"; i++) { "
                  ,ptrOut " + 8*i " n ++ ptrArg " + 8*i " arg ++ ";"
                  ,"}}"]
              _ -> error $ "HashedToC.DebugMemMap.simpleComp found RealPart not preceded by RealImag\n"
                             ++ pretty' (exprs,arg)
          _ -> error $ "HashedToC.DebugMemMap.simpleComp found RealPart not preceded by FT\n"
                             ++ pretty' (exprs,arg) ++ pretty' (simplify' (exprs,arg))
      comp (Op dims ImagPart [arg]) n =
        case (I.lookup arg exprs) of
          Just (Op _dimft (FT _dir) [argFT]) ->
            case (I.lookup argFT exprs) of
              Just (Op _dimri RealImag [_rN,_iN]) ->
                  -- copy imaginary part only
                  ["/* ImagPart in " ++ pretty (Expression n exprs) ++ " */"
                  ,"{int i;"
                  ,"for (i=0; i<" ++ show (dimProd dims) ++"; i++) { "
                  ,ptrOut " + 8*i " n ++ ptrArg (" + 8*i + 8*" ++ show (dimProd dims)) arg ++ ";"
                  ,"}}"]
              _ -> error "HashedToC.DebugMemMap.simpleComp found RealPart not preceded by RealImag"
          _ -> error "HashedToC.DebugMemMap.simpleComp found ImagPart not preceded by FT"
      comp (Op (Dim1 dim) (FT dir) [arg]) n =
        case (I.lookup arg exprs) of
           Just (Op _dimri RealImag [rN,iN]) -> let
                cDir :: Bool -> Int
                cDir True  = 1
                cDir False = -1
                length = ceiling $ logBase 2 (toEnum dim)
               in
                ["/* FFT 1d in " ++ pretty (Expression n exprs) ++ " */"
                -- build complex out of real and imaginary parts
                ,"{DSPDoubleSplitComplex complexD, outD;"
                ,"complexD.realp=&" ++ ptrArg "" rN ++ ";"
                ,"complexD.imagp=&" ++ ptrArg "" iN ++ ";"
                ,"outD.realp=&" ++ ptrArg "" n ++ ";"
                ,"outD.imagp=&" ++ ptrArg (" + " ++ show (8*dim)) n ++ ";"
                ,"FFTSetupD setup = vDSP_create_fftsetupD( " ++ show length ++ ", 0 );"
                ,"vDSP_fft_zopD(setup,&complexD,1,&outD,1," ++ show length ++ "," ++ show (cDir dir) ++ ");}"
                ]
           _ -> error "HashedToC.DebugMemMap.simpleComp found FT not preceded by RealImag "

      comp (Op (Dim2 (dimX,dimY)) (FT dir) [arg]) n =
        case (I.lookup arg exprs) of
           Just (Op _dims RealImag [rN,iN]) -> let
                cDir :: Bool -> Int
                cDir True  = 1
                cDir False = -1
                lengthX = ceiling $ logBase 2 (toEnum dimX)
                lengthY = ceiling $ logBase 2 (toEnum dimY)
               in
                ["/* FFT 2d in " ++ pretty (Expression n exprs) ++ " */"
                -- build complex out of real and imaginary parts
                ,"{DSPDoubleSplitComplex complexD, outD;"
                ,"complexD.realp=&" ++ ptrArg "" rN ++ ";"
                ,"complexD.imagp=&" ++ ptrArg "" iN ++ ";"
                ,"outD.realp=&" ++ ptrArg "" n ++ ";"
                ,"outD.imagp=&" ++ ptrArg (" + " ++ show (dimX*dimY*8)) n ++ ";"
                ,"FFTSetupD setup = vDSP_create_fftsetupD( " ++ show (maximum [lengthX,lengthY]) ++ ", 0 );"
                ,"vDSP_fft2d_zopD(setup,&complexD,1,0,&outD,1,0," ++ show lengthX ++ ","
                              ++ show lengthY ++ ","
                              ++ show (cDir dir) ++ ");}"
                ]
           _ -> error "HashedToC.DebugMemMap.simpleComp found FT not preceded by RealImag "

      comp (Op (Dim3 (dimX,dimY,dimZ)) (FT dir) [arg]) n =
        case (I.lookup arg exprs) of
           Just (Op _dims RealImag [rN,iN]) -> let
                cDir :: Bool -> Int
                cDir True  = 1
                cDir False = -1
                lengthX = ceiling $ logBase 2 (toEnum dimX)
                lengthY = ceiling $ logBase 2 (toEnum dimY)
                lengthZ = ceiling $ logBase 2 (toEnum dimZ)
               in
                ["/* FFT 3d in " ++ pretty (Expression n exprs) ++ " */"
                -- build complex out of real and imaginary parts
                ,"{DSPDoubleSplitComplex complexD;"
                ,"int idx;"
                ,"complexD.realp=&" ++ ptrArg "" n ++ ";"
                ,"complexD.imagp=&" ++ ptrArg (" + " ++ show (dimX*dimY*dimZ*8)) n ++ ";"
                ,"memcpy(&" ++ ptrArg "" n ++ ",&" ++ ptrArg "" rN ++ "," ++ show (dimX*dimY*dimZ*8) ++ ");"
                ,"memcpy(&" ++ ptrArg (" + " ++ show (dimX*dimY*dimZ*8)) n ++ ",&" ++ ptrArg  "" iN ++ "," ++ show (dimX*dimY*dimZ*8) ++ ");"
                ,"FFTSetupD setup = vDSP_create_fftsetupD( " ++ show (maximum [lengthX,lengthY,lengthZ]) ++ ", 0 );"
                ,"// FFT in x-rows, all at once"
                ,"vDSP_fftm_zipD(setup,&complexD,1," -- step between elements in one input vector
                              ++ show (dimX) ++ "," -- stride between first elements of two transforms
                              ++ show lengthX ++ ","
                              ++ show (dimY * dimZ) ++ ","
                              ++ show (cDir dir) ++ ");"
                ,"// FFT in column direction, one x-y plane at a time"
                ,"for (idx = 0; idx < "++ show dimZ ++ "; idx++) {"
                ,"  complexD.realp=(&" ++ ptrArg (" + idx * 8 * " ++ show (dimX * dimY)) n ++ ");"
                ,"  complexD.imagp=(&" ++ ptrArg (" + idx * 8 * " ++ show (dimX * dimY) ++ "+" ++ show (8*dimX*dimY*dimZ)) n ++ ");"
                ,"  vDSP_fftm_zipD(setup,&complexD,"
                              ++ show (dimX) ++ "," -- stride between elements in one input vector
                              ++ show 1 ++ "," -- stride between first elements of two transforms
                              ++ show lengthY ++ ","
                              ++ show (dimX) ++ ","
                              ++ show (cDir dir) ++ ");"
                ,"}"
                ,"// FFT in z-columns, all at once"
                ,"complexD.realp=&" ++ ptrArg "" n ++ ";"
                ,"complexD.imagp=&" ++ ptrArg (" + " ++ show (8*dimX*dimY*dimZ)) n ++ ";"
                ,"vDSP_fftm_zipD(setup,&complexD,"
                              ++ show (dimX*dimY) ++ "," -- stride between elements in one input vector
                              ++ show 1 ++ "," -- stride between first elements of two transforms
                              ++ show lengthZ ++ ","
                              ++ show (dimX*dimY) ++ ","
                              ++ show (cDir dir) ++ ");"
                ,"}"
                ]
           _ -> error "HashedToC.DebugMemMap.simpleComp found FT not preceded by RealImag "
      comp (Op dims (SCZ sczExpr) inputs) n
        = case dims of
                   Dim0 -> let (comp1,result) = sczComp mem exprs sczExpr inputs ""
                         in ["/* SCZ in " ++ pretty (Expression n exprs) ++ " */"] ++ comp1 ++ [ptrOut "" n ++ result ++ ";"]
                   _ -> let (comp1,result) = sczComp mem exprs sczExpr inputs " + 8*i "
                        in  ["/* SCZ in " ++ pretty (Expression n exprs) ++ " */"
                            ,"{int i;"
                            ,"for (i=0; i<" ++ show (dimProd dims) ++"; i++) { "
                            ] ++ comp1
                            ++ [ptrOut " + 8*i " n ++ result ++ ";}}"]
      comp op n = error $ "HashedToC.DebugMemMap.simpleComp unimplemented " ++ show (n,op)

     in concatMap comp' $ topSort exprs node
\end{code}

\begin{code}
sczComp mem exprs (Expression node sczExprs) inputs outerOffset = let

      ptrArg arg
        | Just (Const Dim0 d) <- I.lookup arg sczExprs
        = "(" ++ show d ++ ")"
        | True
        =  "temp" ++ if ( arg < 0 ) then "n" ++ show (-1*arg) else show arg

      ptrOut n = "double temp" ++ (if n < 0 then "n" ++ show (-1*n) else show n) ++ " = "

      memOffset' n = case memOffset mem n [] of
        Just x -> show x
        Nothing -> error $ "HashedToC.sczComp missing node in memmap " ++ show (n,mem,pretty $ Expression node sczExprs,sczExprs)

      comp' :: Node -> [String]
      comp' n = case I.lookup n sczExprs of
         Just (RelElem aIdx ZeroMargin offsets) ->
           let
             arrayN = inputs !! aIdx
             dims = getDimE exprs arrayN
             relOffset = (" + " ++) $ show $ sum $ zipWith (*) offsets $ [product $ take (dimLength dims - i) (1:(dimList dims)) | i <- [0..]]

           in [ptrOut n ++ (if length offsets == dimLength dims
                            then "( *((double*)(ptr + "++ memOffset' arrayN ++ outerOffset ++ relOffset ++"*8) ));"
                            else error $ "sczComp length of offsets doesn't match dims on input " ++ show (aIdx,arrayN,dims,offsets))]
         Just (Const _ d) -> [ptrOut n ++ show d ++ ";"]
         Just (Op Dim0 Sum args) -> [ptrOut n ++ (concat $ intersperse " + " (map ptrArg args)) ++ ";"]
         Just (Op Dim0 Prod args) -> [ptrOut n ++ (concat $ intersperse " * " (map ptrArg args)) ++ ";"]
         Just (Op Dim0 Div [arg1,arg2]) -> [ptrOut n ++ (ptrArg arg1) ++ " / " ++ (ptrArg arg2) ++ ";"]
         Just (Op Dim0 op [arg]) -> [ptrOut n
                                     ++ (case op of
                                           Abs -> "abs"
                                           Signum -> "signum"
                                           Sqrt -> "sqrt"
                                           Sin -> "sin"
                                           Cos -> "cos"
                                           Tan -> "tan"
                                           Exp -> "exp"
                                           Log -> "log"
                                           Sinh -> "sinh"
                                           Cosh -> "cosh"
                                           Tanh -> "tanh"
                                           Asin -> "asin"
                                           Acos -> "acos"
                                           Atan -> "atan"
                                           Asinh -> "asinh"
                                           Acosh -> "acosh"
                                           Atanh -> "atanh"
                                           _ -> error $ "HashedToC.DebugMemMap.sczComp []->[] Op found " ++ show op
                                        ) ++ "(" ++ (ptrArg arg) ++ ");"
                                  ]
         Just op -> error $ "HTC.sczComp found op " ++ show op
         Nothing -> []

    in (concatMap comp' $ topSort sczExprs node,ptrArg node)

log2i n = let (n',rem) = divMod n 2
          in  if n == 1 then 0
              else  if rem == 0
                    then 1 + log2i n'
                    else error "log2i given non power of 2"
\end{code}

Store variables in |MemMap|.
\begin{code}
preload :: MemMap mem => mem -> Expression -> ValMaps -> [String]
preload mem (Expression _node exprs) (ValMaps m0 m1 m2 m3 m4) =
     concatMap (\ (n,a) -> map (combineOffsets n) a)
     $ filter (not . null . snd)
     $ map (\ (n,v) -> (n,mkAssign v) )
     $ filter (\ (_,x) -> isVar x) $ I.toList exprs

  where

    combineOffsets :: Node -> (Int,Double) -> String
    combineOffsets node (idx,val) = case memOffset mem node [] of
      Just x -> "*((double*)(ptr + " ++ show (x + sizeOfDouble * idx) ++ ")) = " ++ show val ++ ";"
      Nothing -> error $ "HTC.preload memOffset " ++ show (node,exprs)

    mkAssign :: ExpressionEdge -> [(Int,Double)]
    mkAssign (Var Dim0 name) = case Map.lookup name m0 of
                               Just vals -> [(0,vals)]
                               Nothing -> flip trace [] $ "HTC.preload missing " ++ show (name {-,Expression node exprs -})
    mkAssign (Var (Dim1 d1) name) = case Map.lookup name m1 of
                               Just vals -> zip [0..d1-1] $ elems vals
                               Nothing -> flip trace [] $ "HTC.preload missing " ++ show (name {-,Expression node exprs -})
    mkAssign (Var (Dim2 (d1,d2)) name) = case Map.lookup name m2 of
                               Just vals -> zip  [i1+i2*d1 | i1 <-[0..d1-1], i2 <- [0..d2-1]]
                                                 $ elems vals
                               Nothing -> flip trace [] $ "HTC.preload missing " ++ show (name {-,Expression node exprs -})
    mkAssign (Var (Dim3 (d1,d2,d3)) name) = case Map.lookup name m3 of
                               Just vals -> zip  [i1+i2*d1 + i3*d1*d2
                                                 | i1 <-[0..d1-1], i2 <- [0..d2-1], i3 <- [0..d3-1]]
                                                 $ elems vals
                               Nothing -> flip trace [] $ "HTC.preload missing " ++ show (name {-,Expression node exprs -})
    mkAssign (Var (Dim4 (d1,d2,d3,d4)) name) = case Map.lookup name m4 of
                               Just vals -> zip  [i1 + i2 * d1 + i3 * d1*d2 + i4 * d1*d2*d3
                                                 | i1 <-[0..d1-1], i2 <- [0..d2-1], i3 <- [0..d3-1], i4 <- [0..d4-1]]
                                                 $ elems vals
                               Nothing -> flip trace [] $ "HTC.preload missing " ++ show (name {-,Expression node exprs -} )
    mkAssign ee = error $ "HTC.preload got nonVar " ++ show ee
\end{code}

FIll-in sparse sampling pattern |MemMap|.
\begin{code}
sparseFill2d :: MemMap mem => mem -> Expression -> (C.ByteString,[(Int,Int)]) -> [String]
sparseFill2d mem (Expression n exprs) (sparseName,sparseList) =
     ["{ int i; int tempIdxs["++show (length sparseList)++"] ={"
     ,concat $ L.intersperse "," [show $ i1 + d1 * i2 | (i1,i2) <- sparseList]
     ,"};"
     ,"for (i=0;i<"++show (length sparseList)++";i++) ((double*)(ptr+"
       ++ (case memOffset mem sparseNode [] of
            Just x -> show x
            Nothing -> error $ "HTC.sparseFill2d memOffset " ++ show (sparseNode,exprs)
          )
       ++"[tempIdxs[i]]=1;"
     ,"}"
     ]

  where
    sparseNode = case lookup sparseName (variables exprs n) of
      Just x -> x
      Nothing -> error $ "HTC.sparseFill2d no variable " ++ show (sparseName,exprs)
    Dim2 (d1,_d2) = getDimE exprs sparseNode
\end{code}


Generate file to be merged with top.c and bottom.c which contain headers and l-BFGS code.
The tricky part is to marshall the components of the gradient for the gradient function.
\begin{code}
lbfgsSetup :: FGrad -> Double -> ValMaps -> [String]
--FIXME guess is not used, because of hack, and needs to be made more general, probably usining a second ValMaps, or loading from a file
lbfgsSetup (FGrad exprs fgNode fNode params varDiffs) _guess vals = concat [main,memDebug,func,gradFun]
{-
 FGrad = FGrad  Internal             -- expression map
                    Node                 -- Compound node, contains compound grad and function
                    Node                 -- the function node
                    [(Node,ByteString)]  -- parameters (with respect to which we don't differentiate)
                    [(Node,ByteString)]  -- variables (with respect to which we do differentiate)

-}
  where
    -- the size of the l-BFGS vector is the sum of sizes of the gradientNodes
    size = if (sum $ map (nodeSize . snd . fst) varDiffs) == totalSize
           then totalSize else error $ "lbfgsSetup sizes don't match " ++ show (totalSize,map (nodeSize . snd . fst) varDiffs)

    eFun = Expression fNode exprs
    eFunGrad = Expression fgNode exprs

    nodeSize node = (8*) $ dimProdE (exprs,node)

    createFiles = concat $ intersperse "\n" $ ["FILE *fs" ++ show n ++ ";" | n <- [0..length params - 1]]
                                              ++ ["FILE *fsInit" ++ show n ++ ";" | n <- [0..length varDiffs - 1]]
    openFiles   = concat $ intersperse "\n" $ ["fs" ++ show n ++ " = fopen(\"" ++ C.unpack name ++ "\", \"r\");" | (n,(_,name)) <- zip [0..] params]
                                            ++ ["fsInit" ++ show n ++ " = fopen(\"" ++ C.unpack name ++ "_init\", \"r\");" | (n,(_,name)) <- zip [0..] varDiffs]
    testFiles   = concat $ intersperse "\n" ["if (fs" ++ show n ++ "==NULL) {fputs(\"File error <"++ C.unpack name ++">.\",stderr); exit (1);}"
                                            | (n,(_,name)) <- zip [0..] params]
    -- FIXME:  check succesful read in
    readFiles   = concat $ intersperse "\n" ["fread(ptr + "++ memOffset' node ++
       ",1,"++ show (nodeSize node) ++",fs" ++ show  n ++ ");"
                                            | (n,(node,_)) <- zip [0..] params]
    readInitFiles   = concat $ intersperse "\n" [unlines
      ["if (fsInit" ++ show n ++ "!=NULL) {"
      ,let (_size,offsetCurr,_offsetPtr) = lookupByName name
       in "fread(((char*)currentVal) + "++ show offsetCurr ++ ",1,"++ show (nodeSize node) ++",fsInit"
          ++ show  n ++ ");"
      ,"} else {printf(\"init not found for "++ C.unpack name ++"\\n\");}"
      ]
     | (n,((node,_),name)) <- zip [0..] varDiffs]
    closeFiles  = concat $ intersperse "\n" $ ["fclose(fs" ++ show n ++ ");" | n <- [0..length params - 1]]
                                            ++ ["fclose(fsInit" ++ show n ++ ");" | n <- [0..length varDiffs - 1]]


    -- Result files
    createResultFiles = concat $ intersperse "\n" ["FILE *fs" ++ C.unpack name ++ ";" | (_,name) <- varDiffs]
    openResultFiles   = concat $ intersperse "\n" ["fs" ++ C.unpack name ++ " = fopen(\"" ++ C.unpack name ++ "\", \"w\");"| (_,name) <- varDiffs]
    testResultFiles   = concat $ intersperse "\n" ["if (fs" ++ C.unpack name ++ "==NULL) {fputs(\"File error <"++ C.unpack name ++">.\",stderr); exit (1);}"
                                            | (_,name) <- varDiffs]
\end{code}
\begin{code}
    -- for each of the result vectors, write the resulting pointer to a file
    -- ret = fwrite( ptr, size, nitems, f );
    -- FIXME:  check successful file filling
    writeResultFiles  = concat $ intersperse "\n" [let (_size,offsetCurr,_offsetPtr) = lookupByName name
       in "fwrite(((char*)nextVal) + "++ show offsetCurr ++
       ",1,"++ show (nodeSize node) ++",fs" ++  C.unpack name ++ ");" | ((node,_),name) <- varDiffs]
    closeResultFiles  = concat $ intersperse "\n" ["fclose(fs" ++ C.unpack name ++ ");" | (_,name) <- varDiffs]

    storeResultFiles = [createResultFiles,openResultFiles,testResultFiles,writeResultFiles,closeResultFiles]

    mem = mkMap eFunGrad :: DebugMemMap
    memOffset' n = case memOffset mem n [] of
        Just x -> show x
        Nothing -> error $ "HashedToC.lbfgsSetup missing node in memmap " ++ show (n,mem,pretty $ Expression fgNode exprs,exprs)
    memDebug = map ("// "++) $ prettyMem exprs mem
    mainpre = ["#include <math.h>","#include <stdlib.h>","#include <stdio.h>"
          ,"#include <Accelerate/Accelerate.h>","#include <CoreServices/CoreServices.h>"
          ,"#define size (" ++ show (div size sizeOfDouble) ++ ")"
          ,"#define historySize 10"
          ,"#define epsfun 1e-7"
          ,"#define epsx 1e-7"
          ,"#define epsgrad 1e-7"
          ,"#define maxIter 15"
          ,"#define c1 0.001"
          ,"#define c2 0.975"
          ,"#define wolfeTol 1e-5"
          ,""
          ,"char *ptr;"
          ,"double *currentVal,*nextVal;"
          ,""
          ,"int main(void) {"
          ,"ptr = calloc(" ++ show (memTotal mem) ++ ",1);"
          , createFiles
          , openFiles
          , testFiles
          ,"currentVal = (double*) calloc(size,sizeof(double));"
          ,"int i;"
          {- ,"for (i=0;i<size;i++) currentVal[i]=" ++ show guess ++ ";" -}
          ,"for (i=0;i<"++ show (size `div` 8) ++ ";i++) currentVal[i]=0.0;"
          ,"nextVal = (double*) calloc(size,sizeof(double));"
          , readFiles
          , readInitFiles
          ]




    init = (preload mem eFunGrad vals)
    solve = ["lbfgs(currentVal,nextVal);"]
    mainpost = [closeFiles,"exit(0);","}"]
    main = concat [mainpre,init,solve,storeResultFiles,mainpost]


    {-result exprs' n = -- FIXME: take out the display one
        concatMap (displayOne mem exprs . fst) $ filter (\ (_,x) -> isRealOp x) $ L.nub $ L.sort  $ depthFirst exprs' n
-}
    (totalSize,offsetByName) = mapAccumR (\ soFar (name,block,offset) -> (soFar + block, (name,(block,soFar,offset)))) 0
                                         [ (name,nodeSize node,memOffset' node) | ((node,_),name) <- varDiffs]
    -- JLMP switch to faster lookup using Data.Map
    lookupByName name = case L.lookup name offsetByName of
                              Just x -> x
                              Nothing -> error "lbfgsSetup couldn't find name for offset"

    copyIn    = [ let (size,offsetCurr,offsetPtr) = lookupByName name
                  in "memcpy(ptr + "++ offsetPtr ++",((char*)currentGuess) + "++ show offsetCurr ++","++ show size ++");\n"
                | (_,name) <- varDiffs]

    gNameNode = map (\ ((_,n),name) -> (name,n) ) varDiffs

    lookupGradOffset name = case L.lookup name gNameNode of
       Just n -> memOffset' n ++ "/* " ++ show (name,gNameNode,n) ++ "*/"
       Nothing -> error "lookupGradOffset"

    -- JLMP why does copIn use offsetPtr but copyOut doesn't?  a comment would help
    copyOut    = [ let (size,offsetCurr,_offsetPtr) = lookupByName name
                  in "memcpy(((char*)gradientReturn) + "++ show offsetCurr ++",ptr + "++ lookupGradOffset name ++","++ show size ++");\n"
                | (_,name) <- varDiffs]

    printGrad    = [ "{int i;"
                   , "printf(\"gradientReturn\\n\");"
                   , "  for (i=0;i<size;i++) printf(\"%f \",gradientReturn[i]);"
                   , "printf(\"\\n\");"
                   , "}"
                   ]

    funpre = ["double myFunction(double* currentGuess) {"]
    funmiddle = (simpleComp mem eFun)  -- ++ (result exprs fNode)
    --funpost = ["printf(\"function return %f\\n\",(*((double*)(ptr+"++ memOffset' fNode ++"))));\nreturn (*((double*)(ptr+"++ memOffset' fNode ++")));\n}\n"]
    funpost = ["return (*((double*)(ptr+"++ memOffset' fNode ++")));\n}\n"]
    func = concat [funpre,copyIn,funmiddle,funpost]

    gradFunpre    = ["double myFunctionAndGradient(double* currentGuess, double* gradientReturn) {"]
    gradFunmiddle = (simpleComp mem eFunGrad) -- ++ (result exprs fgNode)
    --gradFunpost = ["printf(\"grad function return %f\\n\",(*((double*)(ptr+"++ memOffset' fNode ++"))));\nreturn (*((double*)(ptr+"++ memOffset' fNode ++")));\n}\n"]
    gradFunpost = ["return (*((double*)(ptr+"++ memOffset' fNode ++")));\n}\n"]
    gradFun = concat [gradFunpre,copyIn,gradFunmiddle,if False then printGrad else [],copyOut,gradFunpost]
\end{code}



Runs simpleComp and puts result in files
- params are name of vars that will be pulled from files: node and varname pairs
\begin{code}
compWithFiles :: (Node,Internal) -> [C.ByteString] -> [(Node,C.ByteString)] -> Dims -> [String]
compWithFiles (node,exprs) parNames outputs vDims = concat [main,memDebug]

  where
    getNode name = case I.lookup node exprs of
          _ -> node -- FIXME: doule check this
      where
        (_e,node) = addEdge exprs (Var vDims name)

    params = map (\ name -> (getNode name,name)) parNames

\end{code}
    lookupByName name = case L.lookup name offsetByName of
                              Just x -> x
                              Nothing -> error "HC.compWithFiles couldn't find name for offset "
\begin{code}

    (totalSize,_offsetByName) = mapAccumR (\ soFar (name,block,offset) -> (soFar + block, (name,(block,soFar,offset)))) 0
                                         [ (name,nodeSize node,memOffset' node) | (node,name) <- outputs]

    size = totalSize

    nodeSize node = (8*) $ dimProdE (exprs,node)

    createFiles = concat $ intersperse "\n" ["FILE *fs" ++ show n ++ ";" | n <- [0..length params - 1]]
    openFiles   = concat $ intersperse "\n" ["fs" ++ show n ++ " = fopen(\"" ++ C.unpack name ++ "\", \"r\");" | (n,(_,name)) <- zip [0..] params]
    testFiles   = concat $ intersperse "\n" ["if (fs" ++ show n ++ "==NULL) {fputs(\"File error <"++ C.unpack name ++">.\",stderr); exit (1);}"
                                            | (n,(_,name)) <- zip [0..] params]
    -- FIXME:  check succesful read in
    readFiles   = concat $ intersperse "\n" ["fread(ptr + "++ memOffset' node ++
       ",1,"++ show (nodeSize node) ++",fs" ++ show  n ++ ");"
                                            | (n,(node,_)) <- zip [0..] params]
    closeFiles  = concat $ intersperse "\n" ["fclose(fs" ++ show n ++ ");" | n <- [0..length params - 1]]

    f = Expression node exprs
    mem = mkMap f :: DebugMemMap
    memOffset' n = case memOffset mem n [] of
        Just x -> show x
        Nothing -> error $ "HashedToC.compWithFiles missing node in memmap " ++ show (n,mem,pretty $ Expression n exprs,exprs)
    memDebug = map ("// "++) $ prettyMem exprs mem
    mainpre = ["#include <math.h>","#include <stdlib.h>","#include <stdio.h>"
          ,"#include <Accelerate/Accelerate.h>","#include <CoreServices/CoreServices.h>"
          ,"#define size (" ++ show (div size sizeOfDouble) ++ ")"
          ,""
          ,"char *ptr;"
          ,"double *currentVal,*nextVal;"
          ,""
          ,"int main(void) {"
          ,"ptr = calloc(" ++ show (memTotal mem) ++ ",1);"
          , createFiles
          , openFiles
          , testFiles
          , readFiles
          ]


    -- FIXME, should probably put this back in, but don't need it right now
    --init = foldl (\ (_n,e) -> (preload mem e vals)) outputs
    bodies = concat $  map (\ (n,_) -> simpleComp mem (Expression n exprs)) outputs

    -- Result files
    createResultFiles = concat $ intersperse "\n" ["FILE *fs" ++ C.unpack name ++ ";" | (_,name) <- outputs]
    openResultFiles   = concat $ intersperse "\n" ["fs" ++ C.unpack name ++ " = fopen(\"" ++ C.unpack name ++ "\", \"w\");"| (_,name) <- outputs]
    testResultFiles   = concat $ intersperse "\n" ["if (fs" ++ C.unpack name ++ "==NULL) {fputs(\"File error <"++ C.unpack name ++">.\",stderr); exit (1);}"
                                            | (_,name) <- outputs]

    -- for each of the result vectors, write the resulting pointer to a file
    -- ret = fwrite( ptr, size, nitems, f );
    -- FIXME:  check successful file filling
    writeResultFiles  = concat $ intersperse "\n" ["fwrite(ptr + "++ memOffset' n ++
       ",1,"++ show (nodeSize n) ++",fs" ++  C.unpack name ++ ");" | (n,name) <- outputs]
    closeResultFiles  = concat $ intersperse "\n" ["fclose(fs" ++ C.unpack name ++ ");" | (_,name) <- outputs]

    storeResultFiles = [createResultFiles,openResultFiles,testResultFiles,writeResultFiles,closeResultFiles]

    mainpost = [closeFiles,"exit(0);","}"]
    main = concat [mainpre,{-init,-} bodies,storeResultFiles,mainpost]

\end{code}








Generate a test function, using debug memory allocation.
\begin{code}
mkSimpleTest :: Expression -> ValMaps -> [String]
mkSimpleTest (Expression n1 e1) vals = concat [pre,init,comp,results,vars,tests,post,["/* ",show mem,show exprs,"*/"]]
  where
    e@(Expression node exprs) = bigE $ simplify' (e1,n1)
    mem = mkMap e :: DebugMemMap
    pre = ["#include <math.h>","#include <stdlib.h>","#include <stdio.h>","#include <Accelerate/Accelerate.h>","#include <CoreServices/CoreServices.h>","int main(void) {"]
    init = ("char *ptr = calloc(" ++ show (memTotal mem) ++ ",1);") : (preload mem e vals)
    comp = simpleComp mem e
    results = concatMap (displayOne mem exprs . fst) $ filter (\ (_,x) -> isRealOp x || isVar x) $ L.nub $ L.sort  $ depthFirst exprs node
    vars  = ["int result = 0;"];
    tests = concatMap  (compareOne 0.1 vals mem exprs . fst)
                       $ filter (\ (_,x) -> isRealOp x || isVar x) $ L.nub $ L.sort $ depthFirst exprs node
    post = ["exit(result);","}"]
\end{code}


\begin{code}
mkCompPlot :: Expression -> ValMaps -> [(C.ByteString,[(Int,Int)])] -> [String]
mkCompPlot e@(Expression node exprs) vals sparseLists = concat [pre,init,comp,plot,post,["/* ",show mem,show exprs,"*/"]]
  where
    mem = mkMap e :: DebugMemMap
    pre = ["#include <math.h>","#include <stdlib.h>","#include <stdio.h>","#include <Accelerate/Accelerate.h>","#include <CoreServices/CoreServices.h>","int main(void) {"]
    init = ("char *ptr = calloc(" ++ show (memTotal mem) ++ ",1);")
           : (preload mem e vals ++ concatMap (sparseFill2d mem e) sparseLists)
    comp = simpleComp mem e
    plot = plotOne mem exprs (node,"plot.dat")
    post = ["exit(0);","}"]
\end{code}

\begin{code}
displayOne :: DebugMemMap -> Internal -> Node -> [String]
displayOne dmm exprs node = ("printf(\"\\n----- " ++ (pretty $ Expression node exprs) ++ " -----\\n\");") : disp
  where
    dispOne idxs = case memOffset dmm node idxs of
      Just x -> "printf(\"%10.3f \", *((double*)(ptr +"++ show x ++")));"
      Nothing -> error $ "HTC.displayOne node not in MM "++show(node,dmm,exprs)

    disp = case I.lookup node exprs of
      Nothing -> ["HTC.displayOne node not found " ++ show (node,exprs)]
      Just (Op Dim0 (Compound _) _) -> []
      Just (Op dim (Compound _) _) -> [] -- error ("HTC.plotOne found Compound with " ++ show dim ++ " and does not know what to do.")  --TB added this case since making CDims
      Just (Op Dim0 _ _) -> ("printf(\"\\n\");") : [dispOne []]
      Just (Var Dim0 _) -> ("printf(\"\\n\");") : [dispOne []]
      Just (Op (Dim1 d1) _ _) ->
        [ dispOne [i] | i <- [0..d1-1]] ++ ["printf(\"\\n\");"]
      Just (Var (Dim1 d1) _) ->
        [ dispOne [i] | i <- [0..d1-1]] ++ ["printf(\"\\n\");"]
      Just (Op (Dim2 (d1,d2)) _ _) ->
        concat ([("printf(\"\\n\");") :[ dispOne [i1,i2] | i1 <- [0..d1-1]]
                | i2<-[0..d2-1]]) ++ ["printf(\"\\n\");"]
      Just (Var (Dim2 (d1,d2)) _) ->
        concat ([("printf(\"\\n\");") :[ dispOne [i1,i2] | i1 <- [0..d1-1]]
                | i2<-[0..d2-1]]) ++ ["printf(\"\\n\");"]
      Just (Op (Dim3 (d1,d2,d3)) _ _) -> concatMap concat $
        [[("printf(\"\\n\");")] :[("printf(\"\\n\");") : [ dispOne [i1,i2,i3] | i1 <- [0..d1-1]] | i2<-[0..d2-1]] | i3<-[0..d3-1]]
           ++ [[["printf(\"\\n\");"]]]
      Just (Var (Dim3 (d1,d2,d3)) _) -> concatMap concat $
        [[("printf(\"\\n\");")] :[("printf(\"\\n\");") : [ dispOne [i1,i2,i3] | i1 <- [0..d1-1]] | i2<-[0..d2-1]] | i3<-[0..d3-1]]
           ++ [[["printf(\"\\n\");"]]]
      -- FIXME: if this works add the rest  --- this certainly doesn't work
      Just (Op (Dim3SL1 (_d1,_d2,_d3) _sd) _ _) -> error "compare Dim3SL1"
      Just (Op (Dim4 (d1,d2,d3,d4)) _ _) ->
        [ dispOne [i1,i2,i3,i4] | i4 <- [0..d4-1], i3<-[0..d3-1]
                  , i2<-[0..d2-1], i1<-[0..d1-1]] ++ ["printf(\"\\n\");"]
      Just (Var (Dim4 (d1,d2,d3,d4)) _) ->
        [ dispOne [i1,i2,i3,i4] | i4 <- [0..d4-1], i3<-[0..d3-1]
                  , i2<-[0..d2-1], i1<-[0..d1-1]] ++ ["printf(\"\\n\");"]
      x -> error $ "HTC.displayOne " ++ show x
\end{code}

\begin{code}
plotOne :: DebugMemMap -> Internal -> (Node,String) -> [String]
plotOne dmm exprs (node,filename) = ("printf(\"\\n----- " ++ (pretty $ Expression node exprs) ++ " -----\\n\")")
   : open : (disp ++ close)
  where
    dispOne idxs = case memOffset dmm node idxs of
      Just x -> "fprintf(f,\"%10.3f \", *((double*)(ptr +"++ show x ++")));"
      Nothing -> error $ "HTC.plotOne node not in MM "++show(node,dmm,exprs)

    open = "file * f = fopen("++show filename++",\"w\");\n"
    close = ["fclose(f);\n"]
    disp = case I.lookup node exprs of
      Nothing -> ["HTC.plotOne node not found " ++ show (node,exprs)]
      Just (Op Dim0 (Compound _) _) -> []
      Just (Op dim (Compound _) _) -> error ("HTC.plotOne found Compound with " ++ show dim ++ " and does not know what to do.")  --TB added this case since making CDims
      Just (Op Dim0 _ _) -> ("fprintf(f,\"\\n\");") : [dispOne []]
      Just (Op (Dim1 d1) _ _) ->
        [ dispOne [idx] | idx <- [0..d1-1]] ++ ["fprintf(f,\"\\n\");"]
      Just (Op (Dim2 (d1,d2)) _ _) ->
        concat ([("fprintf(f,\"\\n\");") :[ dispOne [idx1,idx2] | idx2 <- [0..d2-1]]
                | idx1<-[0..d1-1]]) ++ ["fprintf(f,\"\\n\");"]
      Just (Op (Dim3 (d1,d2,d3)) _ _) -> concatMap concat $
        [[("fprintf(f,\"\\n\");")] :[("fprintf(f,\"\\n\");") : [ dispOne [i1,i2,i3] | i3 <- [0..d3-1]] | i2<-[0..d2-1]] | i1<-[0..d1-1]]
           ++ [[["fprintf(f,\"\\n\");"]]]
      Just (Op (Dim4 (d1,d2,d3,d4)) _ _) ->
        [ dispOne [i1,i2,i3,i4] | i4 <- [0..d4-1], i3<-[0..d3-1]
                  , i2<-[0..d2-1], i1<-[0..d1-1]] ++ ["fprintf(f,\"\\n\");"]
      x -> error $ "HTC.plotOne " ++ show x
\end{code}

\begin{code}
compareOne :: Double -> ValMaps -> DebugMemMap -> Internal -> Node -> [String]
compareOne tol vals dmm exprs node = ("printf(\"\\n>>>>>> checking ----- " ++ (pretty $ Expression node exprs) ++ " -----\");") : disp
  where
    dispOne interpVal idxs = case memOffset dmm node idxs of
      Just x -> "if (fabs("++ show interpVal ++ " - (*((double*)(ptr +"++ show x ++")))) < "
                ++ show tol ++ ") printf(\"%10.3f \", " ++ show interpVal
                ++ "); else { printf(\"           \"); result = 1; }"
      Nothing -> error $ "HTC.compareOne node not in MM "++show(node,dmm,exprs)

    disp = case getDimE exprs node of
      Dim0 -> let interp = evalScalar (Scalar $ Expression node exprs) vals
        in ("printf(\"\\n\");") : [dispOne interp []]

      (Dim1 d1) -> let interp = evalOneD (OneD $ Expression node exprs) vals
        in [ dispOne (interp ! idx) [idx] | idx <- [0..d1-1]] ++ ["printf(\"\\n\");"]

      (Dim2 (d1,d2)) -> let interp i1 i2 = (evalTwoD (TwoD
                                                      $ Expression node exprs) vals) ! (i1,i2)
        in
        concat ([("printf(\"\\n\");") :[ dispOne (interp idx1 idx2) [idx1,idx2]
                                       | idx2<-[0..d2-1]]
                                       | idx1<-[0..d1-1]])
                 ++ ["printf(\"\\n\");"]

      (Dim3 (d1,d2,d3)) ->
        let interp i1 i2 i3
              = (evalThreeD (ThreeD $ Expression node exprs) vals) ! (i1,i2,i3)
        in
        concat ([("printf(\"\\n\");") : (concat [("printf(\"\\n\");") :[ dispOne (interp idx1 idx2 idx3) [idx1,idx2,idx3]
                                       | idx3<-[0..d3-1]]
                                       | idx2<-[0..d2-1]])
                                       | idx1<-[0..d1-1]])
                 ++ ["printf(\"\\n\");"]
      (Dim3SL1 (_d1,_,_) _sd) ->  case I.lookup node exprs of
              Just (Op (Dim3SL1 (_d1,d2,d3) sd) (Project ss) _)  ->
                   let interp i1 i2 i3
                         = (evalThreeDSparse (ThreeDSparse sL $ Expression node exprs) vals) Map.! (i2,i3) ! i1
                       SSList3D sL = ss
                   in
                   concat ([("printf(\"\\n\");") : (concat [("printf(\"\\n\");") :[ dispOne (interp idx1 idx2 idx3) [idx1,idx2,idx3]
                                                  | idx3<-[0..d3-1]]
                                                  | idx2<-[0..d2-1]])
                                                  | idx1<-[0..sd-1]])
                            ++ ["printf(\"\\n\");"]
              x -> error $ "compareOne: can only compare Op generating SL " ++ show x
\end{code}
\begin{code}


      -- TODO JLMP/CA:  Add Sparse Lists

 --     Just (Op [d1,d2,d3,d4] _ _) ->
 --       [ dispOne [idx1,idx2,i3,i4] | idx1 <- [0..d1-1], idx2<-[0..d2-1]
 --                 , i3<-[0..d3-1], i4<-[0..d4-1]] ++ ["printf(\"\\n\");"]
      x -> ["printf(\"HTC.compareOne " ++ show x ++ "\");"]
      --x -> error $ "HTC.compareOne " ++ show x
\end{code}

\begin{code}
sizeOfDouble = 8
memBuffer = 128*128*4*8
\end{code}
