module HashedExpression.Internal.ToLLVM 
  (     LLVMMemMap(..)
      , LLVMMemMapEntry
      , Code
      , makeLLVMMemMap
      , mkModule
  ) where 

import Data.Array
import Data.Graph (buildG, topSort)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (sort, map, foldl', intercalate, intersperse, tails)
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
import LLVM.AST hiding (Mul,FMul)
import qualified LLVM.AST as AST
import LLVM.AST.Type
import LLVM.AST.Global
import LLVM.Context
import LLVM.Module
import qualified LLVM.AST.Constant as C
import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS
import LLVM.AST.Float
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.ParameterAttribute as PA 
import qualified LLVM.AST.FunctionAttribute as FA 
import LLVM.AST.Linkage
--offset/key into the allocated memory
offsetType :: AST.Type
offsetType = AST.IntegerType 64

--all elements used here are of double type
elemType :: AST.Type
--elementType = Double
elemType = AST.FloatingPointType AST.DoubleFP

type LLVMMemMapEntry = (Int, EntryType, Shape)

data EntryType
    = EntryR
    | EntryC
    deriving (Show, Eq, Ord)

type Code = [Named AST.Instruction]

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
generateEvaluatingCodes :: LLVMMemMap -> (ExpressionMap, [Int]) -> AST.Definition--Module
generateEvaluatingCodes memMap (mp, rootIds) =
  let
    sortedVars = sort $ varNodesWithId mp
    parameterMap = zip (map fst sortedVars) [0..]

    -- | create a temporary variable out of the node id
    -- mkTemp :: Int -> AST.Name
    mkTemp n =
      case retrieveInternal n mp of
        ([],Var name) -> case lookup name parameterMap of
                            Just idx -> mkName (name)
                            _ -> error "variable name missing from parameterMap"
        _ -> mkName ("t" ++ show n)
    mkTemp2 n m = mkName ("s"++show n++"_"++show m)

    callFun :: String -> Int -> Int -> [Named AST.Instruction]
    callFun funName n arg = [mkTemp n AST.:= AST.Call  { tailCallKind = Nothing -- :: Maybe TailCallKind,
                                                           , callingConvention = CC.C -- :: CallingConvention, Found in IR construction Instruction.hs
                                                           , returnAttributes = [] -- :: [PA.ParameterAttribute],
                                                           , function = Right (AST.ConstantOperand $ C.GlobalReference funcType (mkName $ "llvm."++funName++".f64") ) -- :: CallableOperand,
                                                           , arguments = [(AST.LocalReference elemType $ mkTemp arg, [])] --  :: [(Operand, [PA.ParameterAttribute])],
                                                           , functionAttributes = [ ] -- :: [Either FA.GroupID FA.FunctionAttribute],
                                                           , metadata = [] -- :: InstructionMetadata
                                                           } ]
    arithFun :: (AST.FastMathFlags -> AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction ) -> Int -> [Int] -> [Named AST.Instruction]
    arithFun instr n [] = [ mkTemp n AST.:=  instr AST.noFastMathFlags
                               (AST.ConstantOperand (C.Float $ LLVM.AST.Float.Double 0)) -- Check me - Type of var and const
                               (AST.ConstantOperand $ C.Float $ LLVM.AST.Float.Double 0)
                               []]
    arithFun instr n (x:[]) = let argName = mkTemp x
                        in[ mkTemp n AST.:=  instr AST.noFastMathFlags
                                     (AST.ConstantOperand (C.Float $ LLVM.AST.Float.Double 0)) -- Check me - Type of var and const
                                     (AST.LocalReference elemType (argName))
                                     []]
    arithFun instr n (x:y:[]) = let argName1 = mkTemp x
                                    argName2 = mkTemp y
                                in[ mkTemp n AST.:=  instr AST.noFastMathFlags
                                     (AST.LocalReference elemType (argName1)) -- Check me - Type of var and const
                                     (AST.LocalReference elemType (argName2))
                                     []]
    arithFun instr n (x:y:xs) =
      let
        helper (w1:w2:[]) =[ mkTemp2 w1 w2 AST.:=  instr AST.noFastMathFlags
                                      (AST.LocalReference elemType (mkTemp w1)) -- Check me - Type of var and const
                                      (AST.LocalReference elemType (mkTemp w2))
                                      []]
        helper (w1:w2:w3:ws) = helper (w2:ws)
                            ++ [ mkTemp2 w1 w2 AST.:=  instr AST.noFastMathFlags
                                                                 (AST.LocalReference elemType (mkTemp w1)) -- Check me - Type of var and const
                                                                 (AST.LocalReference elemType (mkTemp2 w2 w3))
                                                                 []]
      in
        helper (y:xs)
        ++ ( [ mkTemp n AST.:= instr AST.noFastMathFlags
                                              (AST.LocalReference elemType (mkTemp2 n y)) -- Check me - Type of var and const
                                              (AST.LocalReference elemType (mkTemp x))
                                              []] )
    body = BasicBlock
       (Name "entry")
       (concatMap genCode $ topologicalSortManyRoots (mp, rootIds))
       (Do $ Ret (Just (LocalReference elemType (head $ map mkTemp rootIds))) [])
    getShape :: Int -> Shape
    getShape nId = retrieveShape nId mp
    genCode :: Int -> Code -- From node id to codes
    genCode n =
      let (shape, op) = retrieveInternal n mp
          elementType nId = retrieveElementType nId mp
      in case op of
          Var nam -> let--varName = mkTemp nam
                         parIdx = lookup nam parameterMap
                     in [] 
                     -- For scalars we dont allocate space for local variables. We use mkTemp to refer to the variable name directly.
          DVar _ -> error "DVar should not be here"
          Const val -> [ mkTemp n AST.:=   AST.FAdd AST.noFastMathFlags
                                              (AST.ConstantOperand (C.Float $ LLVM.AST.Float.Double 0)) -- Check me - Type of var and const
                                              (AST.ConstantOperand $ C.Float $ LLVM.AST.Float.Double val)
                                              []] --error "for i n [n `at` i <<- show val]"
          Sum _ args -> arithFun AST.FAdd n args
          Mul _ args -> arithFun AST.FMul n args
          Power x arg  -> [mkTemp n AST.:= AST.Call  { tailCallKind = Nothing -- :: Maybe TailCallKind,
                                                      , callingConvention = CC.C -- :: CallingConvention, Found in IR construction Instruction.hs
                                                      , returnAttributes = [] -- :: [PA.ParameterAttribute],
                                                      , function = Right (AST.ConstantOperand $ C.GlobalReference funcType (mkName "llvm.pow.f64") ) -- :: CallableOperand,
                                                      , arguments = [(AST.LocalReference elemType $ mkTemp arg, []),(AST.LocalReference elemType $ mkTemp x, [])] --  :: [(Operand, [PA.ParameterAttribute])],
                                                      , functionAttributes = [] -- :: [Either FA.GroupID FA.FunctionAttribute],
                                                      , metadata = [] -- :: InstructionMetadata
                                                      } ]
          Neg _ arg -> let argName = mkTemp arg
                       in [ mkTemp n AST.:= AST.FSub AST.noFastMathFlags
                                                  (AST.ConstantOperand (C.Float $ LLVM.AST.Float.Double 0))
                                                  (AST.LocalReference elemType (argName))
                                     []]
                   -- need to generate address, load, negate, address and store (and two temporary)
          Scale _ scalar arg -> error "Scale should not be here" -- Not relevant to scalar operations
          -- MARK: only apply to R
          Div arg1 arg2  -> [ mkTemp n AST.:= AST.FDiv AST.noFastMathFlags
                                                   (AST.LocalReference elemType (mkTemp arg1))
                                                   (AST.LocalReference elemType (mkTemp arg2))
                                                   []]
                         --error "Div not implemented"
          Sqrt arg -> callFun "sqrt" n arg

          {- [mkTemp n AST.:= AST.Call  { tailCallKind = Nothing -- :: Maybe TailCallKind,
                                 , callingConvention = CC.C -- :: CallingConvention, Found in IR construction Instruction.hs
                                 , returnAttributes = [] -- :: [PA.ParameterAttribute],
                                 , function = Right (AST.ConstantOperand $ C.GlobalReference funcType (mkName "llvm.sqrt.f64") ) -- :: CallableOperand,
                                 --, arguments = [(AST.LocalReference elemType $ mkTemp arg, [PA.ByVal])] --  :: [(Operand, [PA.ParameterAttribute])],
                                 , arguments = [(AST.LocalReference elemType $ mkTemp arg, [])] --  :: [(Operand, [PA.ParameterAttribute])],
                                 --, functionAttributes = [Right FA.NoUnwind, Right FA.ReadNone, Right FA.Speculatable] -- :: [Either FA.GroupID FA.FunctionAttribute],
                                 , functionAttributes = [ ] -- :: [Either FA.GroupID FA.FunctionAttribute],
                                 , metadata = [] -- :: InstructionMetadata
                                 } ] -}

           --error "for i n [n `at` i <<- "sqrt" ++ arg `at` i]"
          Sin arg -> callFun "sin" n arg

          {- [mkTemp n AST.:= AST.Call  { tailCallKind = Nothing -- :: Maybe TailCallKind,
                                                 , callingConvention = CC.C -- :: CallingConvention, Found in IR construction Instruction.hs
                                                 , returnAttributes = [] -- :: [PA.ParameterAttribute],
                                                 , function = Right (AST.ConstantOperand $ C.GlobalReference funcType (mkName "llvm.sin.f64") ) -- :: CallableOperand,
                                                 , arguments = [(AST.LocalReference elemType $ mkTemp arg, [])] --  :: [(Operand, [PA.ParameterAttribute])],
                                                 , functionAttributes = [ ] -- :: [Either FA.GroupID FA.FunctionAttribute],
                                                 , metadata = [] -- :: InstructionMetadata
                                                } ]-}
                    --error "for i n [n `at` i <<- "sin" ++ arg `at` i]"
          Cos arg -> callFun "cos" n arg

          {- [mkTemp n AST.:= AST.Call  { tailCallKind = Nothing -- :: Maybe TailCallKind,
                                                 , callingConvention = CC.C -- :: CallingConvention, Found in IR construction Instruction.hs
                                                 , returnAttributes = [] -- :: [PA.ParameterAttribute],
                                                 , function = Right (AST.ConstantOperand $ C.GlobalReference funcType (mkName "llvm.cos.f64") ) -- :: CallableOperand,
                                                 , arguments = [(AST.LocalReference elemType $ mkTemp arg, [])] --  :: [(Operand, [PA.ParameterAttribute])],
                                                 , functionAttributes = [ ] -- :: [Either FA.GroupID FA.FunctionAttribute],
                                                 , metadata = [] -- :: InstructionMetadata
                                                 } ]-}
                    --error "for i n [n `at` i <<- "cos" ++ arg `at` i]"
          Tan arg -> callFun "tan" n arg
          {- [mkTemp n AST.:= AST.Call  { tailCallKind = Nothing -- :: Maybe TailCallKind,
                                                 , callingConvention = CC.C -- :: CallingConvention, Found in IR construction Instruction.hs
                                                 , returnAttributes = [] -- :: [PA.ParameterAttribute],
                                                 , function = Right (AST.ConstantOperand $ C.GlobalReference funcType (mkName "llvm.tan.f64") ) -- :: CallableOperand,
                                                 , arguments = [(AST.LocalReference elemType $ mkTemp arg, [])] --  :: [(Operand, [PA.ParameterAttribute])],
                                                 , functionAttributes = [ ] -- :: [Either FA.GroupID FA.FunctionAttribute],
                                                 , metadata = [] -- :: InstructionMetadata
                                                  } ]-}
                    --error "for i n [n `at` i <<- "tan" ++ arg `at` i]"
          Exp arg -> callFun "exp" n arg
          {- [mkTemp n AST.:= AST.Call  { tailCallKind = Nothing -- :: Maybe TailCallKind,
                                                , callingConvention = CC.C -- :: CallingConvention, Found in IR construction Instruction.hs
                                                , returnAttributes = [] -- :: [PA.ParameterAttribute],
                                                , function = Right (AST.ConstantOperand $ C.GlobalReference funcType (mkName "llvm.exp.f64") ) -- :: CallableOperand,
                                                , arguments = [(AST.LocalReference elemType $ mkTemp arg, [])] --  :: [(Operand, [PA.ParameterAttribute])],
                                                , functionAttributes = [ ] -- :: [Either FA.GroupID FA.FunctionAttribute],
                                                , metadata = [] -- :: InstructionMetadata
                                                } ]-}
                    --error "for i n [n `at` i <<- "exp" ++ arg `at` i]"
          Log arg -> callFun "log" n arg
          {-[mkTemp n AST.:= AST.Call  { tailCallKind = Nothing -- :: Maybe TailCallKind,
                                                , callingConvention = CC.C -- :: CallingConvention, Found in IR construction Instruction.hs
                                                , returnAttributes = [] -- :: [PA.ParameterAttribute],
                                                , function = Right (AST.ConstantOperand $ C.GlobalReference funcType (mkName "llvm.log.f64") ) -- :: CallableOperand,
                                                , arguments = [(AST.LocalReference elemType $ mkTemp arg, [])] --  :: [(Operand, [PA.ParameterAttribute])],
                                                , functionAttributes = [ ] -- :: [Either FA.GroupID FA.FunctionAttribute],
                                                , metadata = [] -- :: InstructionMetadata
                                                } ]-}
                    --error "for i n [n `at` i <<- "log" ++ arg `at` i]"
          Sinh arg -> callFun "sinh" n arg
          {- [mkTemp n AST.:= AST.Call  { tailCallKind = Nothing -- :: Maybe TailCallKind,
                                                  , callingConvention = CC.C -- :: CallingConvention, Found in IR construction Instruction.hs
                                                  , returnAttributes = [] -- :: [PA.ParameterAttribute],
                                                  , function = Right (AST.ConstantOperand $ C.GlobalReference funcType (mkName "llvm.sinh.f64") ) -- :: CallableOperand,
                                                  , arguments = [(AST.LocalReference elemType $ mkTemp arg, [])] --  :: [(Operand, [PA.ParameterAttribute])],
                                                  , functionAttributes = [ ] -- :: [Either FA.GroupID FA.FunctionAttribute],
                                                  , metadata = [] -- :: InstructionMetadata
                                                   } ]-}
                    --error "for i n [n `at` i <<- "sinh" ++ arg `at` i]"
          Cosh arg -> callFun "cosh" n arg
          {- [mkTemp n AST.:= AST.Call  { tailCallKind = Nothing -- :: Maybe TailCallKind,
                                                 , callingConvention = CC.C -- :: CallingConvention, Found in IR construction Instruction.hs
                                                 , returnAttributes = [] -- :: [PA.ParameterAttribute],
                                                 , function = Right (AST.ConstantOperand $ C.GlobalReference funcType (mkName "llvm.sin.f64") ) -- :: CallableOperand,
                                                 , arguments = [(AST.LocalReference elemType $ mkTemp arg, [])] --  :: [(Operand, [PA.ParameterAttribute])],
                                                 , functionAttributes = [ ] -- :: [Either FA.GroupID FA.FunctionAttribute],
                                                 , metadata = [] -- :: InstructionMetadata
                                                 } ]-}
                      --error "for i n [n `at` i <<- "cosh" ++ arg `at` i]"
          Tanh arg -> callFun "tanh" n arg
          {--[mkTemp n AST.:= AST.Call  { tailCallKind = Nothing -- :: Maybe TailCallKind,
                                                 , callingConvention = CC.C -- :: CallingConvention, Found in IR construction Instruction.hs
                                                 , returnAttributes = [] -- :: [PA.ParameterAttribute],
                                                 , function = Right (AST.ConstantOperand $ C.GlobalReference funcType (mkName "llvm.tanh.f64") ) -- :: CallableOperand,
                                                 , arguments = [(AST.LocalReference elemType $ mkTemp arg, [])] --  :: [(Operand, [PA.ParameterAttribute])],
                                                 , functionAttributes = [ ] -- :: [Either FA.GroupID FA.FunctionAttribute],
                                                 , metadata = [] -- :: InstructionMetadata
                                                 } ]-}
                      --error "for i n [n `at` i <<- "tanh" ++ arg `at` i]"
          Asin arg -> callFun "asin" n arg
          {--[mkTemp n AST.:= AST.Call  { tailCallKind = Nothing -- :: Maybe TailCallKind,
                                                  , callingConvention = CC.C -- :: CallingConvention, Found in IR construction Instruction.hs
                                                  , returnAttributes = [] -- :: [PA.ParameterAttribute],
                                                  , function = Right (AST.ConstantOperand $ C.GlobalReference funcType (mkName "llvm.asin.f64") ) -- :: CallableOperand,
                                                  , arguments = [(AST.LocalReference elemType $ mkTemp arg, [])] --  :: [(Operand, [PA.ParameterAttribute])],
                                                  , functionAttributes = [ ] -- :: [Either FA.GroupID FA.FunctionAttribute],
                                                  , metadata = [] -- :: InstructionMetadata
                                                  } ]-}
                      --error "for i n [n `at` i <<- "asin" ++ arg `at` i]"
          Acos arg -> callFun "acos" n arg
          {--[mkTemp n AST.:= AST.Call  { tailCallKind = Nothing -- :: Maybe TailCallKind,
                                                 , callingConvention = CC.C -- :: CallingConvention, Found in IR construction Instruction.hs
                                                 , returnAttributes = [] -- :: [PA.ParameterAttribute],
                                                 , function = Right (AST.ConstantOperand $ C.GlobalReference funcType (mkName "llvm.acos.f64") ) -- :: CallableOperand,
                                                 , arguments = [(AST.LocalReference elemType $ mkTemp arg, [])] --  :: [(Operand, [PA.ParameterAttribute])],
                                                 , functionAttributes = [ ] -- :: [Either FA.GroupID FA.FunctionAttribute],
                                                 , metadata = [] -- :: InstructionMetadata
                                                  } ]-}
                      --error "for i n [n `at` i <<- "acos" ++ arg `at` i]"
          Atan arg -> callFun "atan" n arg
          {--[mkTemp n AST.:= AST.Call  { tailCallKind = Nothing -- :: Maybe TailCallKind,
                                                  , callingConvention = CC.C -- :: CallingConvention, Found in IR construction Instruction.hs
                                                  , returnAttributes = [] -- :: [PA.ParameterAttribute],
                                                  , function = Right (AST.ConstantOperand $ C.GlobalReference funcType (mkName "llvm.atan.f64") ) -- :: CallableOperand,
                                                   , arguments = [(AST.LocalReference elemType $ mkTemp arg, [])] --  :: [(Operand, [PA.ParameterAttribute])],
                                                  , functionAttributes = [ ] -- :: [Either FA.GroupID FA.FunctionAttribute],
                                                  , metadata = [] -- :: InstructionMetadata
                                                  } ]-}
                      --error "for i n [n `at` i <<- "atan" ++ arg `at` i]"
          Asinh arg -> callFun "asinh" n arg
          {--[mkTemp n AST.:= AST.Call  { tailCallKind = Nothing -- :: Maybe TailCallKind,
                                                   , callingConvention = CC.C -- :: CallingConvention, Found in IR construction Instruction.hs
                                                   , returnAttributes = [] -- :: [PA.ParameterAttribute],
                                                   , function = Right (AST.ConstantOperand $ C.GlobalReference funcType (mkName "llvm.asinh.f64") ) -- :: CallableOperand,
                                                   , arguments = [(AST.LocalReference elemType $ mkTemp arg, [])] --  :: [(Operand, [PA.ParameterAttribute])],
                                                   , functionAttributes = [ ] -- :: [Either FA.GroupID FA.FunctionAttribute],
                                                   , metadata = [] -- :: InstructionMetadata
                                                   } ]-}
                      --error "for i n [n `at` i <<- "asinh" ++ arg `at` i]"
          Acosh arg -> callFun "acosh" n arg
          {- -[mkTemp n AST.:= AST.Call  { tailCallKind = Nothing -- :: Maybe TailCallKind,
                                                  , callingConvention = CC.C -- :: CallingConvention, Found in IR construction Instruction.hs
                                                  , returnAttributes = [] -- :: [PA.ParameterAttribute],
                                                  , function = Right (AST.ConstantOperand $ C.GlobalReference funcType (mkName "llvm.acosh.f64") ) -- :: CallableOperand,
                                                  , arguments = [(AST.LocalReference elemType $ mkTemp arg, [])] --  :: [(Operand, [PA.ParameterAttribute])],
                                                  , functionAttributes = [ ] -- :: [Either FA.GroupID FA.FunctionAttribute],
                                                  , metadata = [] -- :: InstructionMetadata
                                                  } ]-}
                      --error "for i n [n `at` i <<- "acosh" ++ arg `at` i]"
          Atanh arg -> callFun "atanh" n arg
          {--[mkTemp n AST.:= AST.Call  { tailCallKind = Nothing -- :: Maybe TailCallKind,
                                                   , callingConvention = CC.C -- :: CallingConvention, Found in IR construction Instruction.hs
                                                   , returnAttributes = [] -- :: [PA.ParameterAttribute],
                                                   , function = Right (AST.ConstantOperand $ C.GlobalReference funcType (mkName "llvm.atanh.f64") ) -- :: CallableOperand,
                                                   , arguments = [(AST.LocalReference elemType $ mkTemp arg, [])] --  :: [(Operand, [PA.ParameterAttribute])],
                                                    , functionAttributes = [ ] -- :: [Either FA.GroupID FA.FunctionAttribute],
                                                   , metadata = [] -- :: InstructionMetadata
                                                   } ]-}
                      --error "for i n [n `at` i <<- "atanh" ++ arg `at` i]"
          -- MARK: Complex related. Not related to Scalar
          RealImag arg1 arg2 -> error "RealImag should not be here"
          RealPart arg -> error "RealPart should not be here"
          ImagPart arg -> error "ImagPart should not be here"
          InnerProd _ arg1 arg2 -> error "InnerProd should not be here"
          Piecewise marks condition branches -> error "Piecewise should not be here"
          Rotate [amount] arg -> error "Rotate 1D should not be here"
          Rotate [amount1, amount2] arg -> error "Rotate 2D should not be here"
          Rotate [amount1, amount2, amount3] arg -> error "Rotate 3D should not be here"
  in
    GlobalDefinition functionDefaults
       { name = Name "func"
       , parameters =
           ( [ Parameter elemType (mkName name) [] | (name,_) <- sortedVars ]
           , False )
       , returnType = elemType
       , basicBlocks = [body]
       }

funcType = ptr $ FunctionType elemType [elemType] False

mkModule :: Expression d et -> AST.Module
mkModule exp = 
  let 
    Expression topLevel exprMap  = exp 
    llvmMemMap = makeLLVMMemMap exprMap
  in 
    defaultModule
    { moduleName = "basic"
    , moduleDefinitions =  [ generateEvaluatingCodes llvmMemMap (exprMap, [topLevel])]
        ++ externals
    }

externals :: [AST.Definition]
externals =
  let
    onePar = ([Parameter elemType (mkName "") []], False)
    twoPar = ([Parameter elemType (mkName "") [], Parameter elemType (mkName "") []], False)
    defn (name, attrs, par) = GlobalDefinition $ functionDefaults
      { name        = mkName $ "llvm."++name++".f64"
      , linkage     = External
      , parameters = par
      , returnType = elemType
      , LLVM.AST.Global.functionAttributes = attrs
      }

  in
    map defn
      [ ("sin",[Right FA.NoUnwind, Right FA.ReadNone, Right FA.Speculatable], onePar)
      , ("cos",[Right FA.NoUnwind, Right FA.ReadNone, Right FA.Speculatable], onePar)
      , ("tan",[Right FA.NoUnwind, Right FA.ReadNone], onePar)
      , ("sinh",[Right FA.NoUnwind, Right FA.ReadNone], onePar)
      , ("cosh",[Right FA.NoUnwind, Right FA.ReadNone], onePar)
      , ("tanh",[Right FA.NoUnwind, Right FA.ReadNone], onePar)
      , ("asin",[Right FA.NoUnwind, Right FA.ReadNone], onePar)
      , ("acos",[Right FA.NoUnwind, Right FA.ReadNone], onePar)
      , ("atan",[Right FA.NoUnwind, Right FA.ReadNone], onePar)
      , ("asinh",[Right FA.NoUnwind, Right FA.ReadNone], onePar)
      , ("acosh",[Right FA.NoUnwind, Right FA.ReadNone], onePar)
      , ("atanh",[Right FA.NoUnwind, Right FA.ReadNone], onePar)
      , ("exp",[Right FA.NoUnwind, Right FA.ReadNone, Right FA.Speculatable], onePar)
      , ("log",[Right FA.NoUnwind, Right FA.ReadNone, Right FA.Speculatable], onePar)
      , ("pow",[Right FA.NoUnwind, Right FA.ReadNone, Right FA.Speculatable], twoPar)
      , ("sqrt",[Right FA.NoUnwind, Right FA.ReadNone, Right FA.Speculatable], onePar)
      ]
  -- from IRBuilder.Module