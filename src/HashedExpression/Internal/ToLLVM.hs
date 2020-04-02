{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{-|
Module      : ToLLVM
Description : Module to convert the HashedExpression to LLVM intermediate representation
Copyright   : (c) Padma Pasupathi, 2020
Licence     : BSD Licence 2.0                  
Maintainer  : pasupatp@mcmaster.ca
Stability   : experimental
Portability : POSIX

This module has the following functions,
 __/LLVMMemMap/__ - is used to allocate memory for all nodes

 __/GenerateEvaluatingCodes/__ - is used to generate LLVM Definition for every operation

__/mkModule/__ - is used to bind all generated LLVM Definitions to a single LLVM module

 __/Externals/__ - is used to declare all inbuilt functions in LLVM and other helper functions for the above functions.

 The two main functions for generating the LLVM IR evaluation functions from an Expression are makeMemoryMap and generateEvaluatingCode.

 The makeLLVMMemoryMap function allocates memory for all subexpressions. Expression is a DAG (Directed Acyclic Graph) of subexpressions which is stored in a hash map. Every node contains an operation, references to inputs and its dimensions, and whether it is real or complex. The makeLLVMMemoryMap function gets the list of nodes, calculates its size based on the shape obtained from the Expression map and lays them out linearly. This is implemented with a fold operation. The value accumulated by the fold is a pair of the total size and an IntMap mapping the node id to the offset in the allocated memory.

 The generateEvaluatingCode function is used to generate the evaluation function for the given expression.  The code is generated one subexpression at a time.

 Since the subexpression is a DAG, there is an ordering of the nodes in which inputs occur before subexpressions which use them.  Say for instance, in the expression 2x + 2y, the subexpressions can be calculated in the order 2x,2y,2x + 2y, but 2x + 2y could not calculated before 2*y, because it needs it as an input.  This type of order is called a topological order, and is implemented in topologicalSortManyRoots.  Many roots means that it can be used to evaluate multiple expressions with shared subexpressions.

 Code generation is implemented by taking the sorting keys and mapping an evaluation function over the list of keys.  The evaluation function matches the operation to LLVM IR instructions.

 __NOTE__: The test suite for this module is available at test/TestToLLVM/ directory. Please go through the README.md file in this directory for running the test cases.

-}

module HashedExpression.Internal.ToLLVM
  (     LLVMMemMap(..)
      , LLVMMemMapEntry
      , Code
      , makeLLVMMemMap
      , mkModule
      , generateEvaluatingCodes
      , mmUpdate
      , externals
      , elemType
      , funcType
      , nameDef
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

-- | offset/key into the allocated memory
offsetType :: AST.Type
offsetType = AST.IntegerType 64

-- | all elements used here are of double type
elemType :: AST.Type
elemType = AST.FloatingPointType AST.DoubleFP

-- | Every node entry in the memory has an Index, Type of entry which can be either Real or Complex and its dimension
type LLVMMemMapEntry = (Int,       --  Node index
                        EntryType, --  Type of node (Real or complex),
                        Shape)     --  Dimension of the node (0 -[]: Scalar 1D - [n]: Array, 2D - [n,m]: Matrix . . . )

-- | EntryType Defines the type of node entry in the memory which can either be Real or Complex
data EntryType
    = EntryR -- ^ node of type Real number
    | EntryC -- ^ node of type Complex number
    deriving (Show, Eq, Ord)

-- | LLVM Code is a list of Named instructions like Add, FAdd, Mul, Call etc.
type Code = [Named AST.Instruction] -- ^ List of LLVM instructions forming final code

-- | LLVMMemMap has the every node with an index
data LLVMMemMap =
    LLVMMemMap
        { entryMap :: IntMap LLVMMemMapEntry -- ^ every node entry with its index.
        , totalDoubles :: Int -- ^ Total number of entries
        }
    deriving (Show, Eq, Ord)

-- |  __makeLLVMMemMap__ takes any expression map and converts it to corresponding LLVM memory map
makeLLVMMemMap :: ExpressionMap -- ^ Expression with its dimension/shape
                  -> LLVMMemMap -- ^ node entry in memory
makeLLVMMemMap exprMap = uncurry LLVMMemMap $ foldl' (mmUpdate exprMap) (IM.empty, 0) nkeys
  where
    nkeys = IM.keys exprMap

-- | mmUpdate is a helper function for |makeLLVMMemMap|

mmUpdate ::
       ExpressionMap -- ^ Expression mapped with its dimension
    -> (IntMap LLVMMemMapEntry, Int) -- ^ current memory and its size
    -> Int -- ^ node id
    -> (IntMap LLVMMemMapEntry, Int) -- ^ updated memory and updated size
mmUpdate exprMap (memMapSoFar, sizeSoFar) nId =
    let (shape, node) = retrieveInternal nId exprMap
        (nodeSz, mmShape)
            | nodeElementType node exprMap == R = (product shape, EntryR)
            | nodeElementType node exprMap == C = (2 * product shape, EntryC)
        newMemMap = IM.insert nId (sizeSoFar, mmShape, shape) memMapSoFar
     in (newMemMap, sizeSoFar + nodeSz)

-- | Generate evaluation code (usually an expression and its partial derivatives) given an ExpressionMap and indices of nodes to be computed
--
generateEvaluatingCodes :: String -- ^ Name of the function to be generated in LLVM
                           -> LLVMMemMap -- ^ mapped node from memory
                           -> (ExpressionMap, [Int])  -- ^ expression map and its indices
                           -> AST.Definition -- ^ LLVM definition for the given expression
generateEvaluatingCodes funcName memMap (mp, rootIds) =
  let
    sortedVars = sort $ varNodesWithId mp --  Sorts variable names given in the expression
    parameterMap = zip (map fst sortedVars) [0..] --  Maps all sorted parameters/ variables with a index number

    -- | create a temporary variable out of the node id
    -- | mkTemp :: Int -> AST.Name
    mkTemp :: Int        -- ^ Takes an integer (index of the node) as input
              -> AST.Name -- ^ returns a variable name of "Name" Type in LLVM
    mkTemp n =
      case retrieveInternal n mp of
        ([],Var name) -> case lookup name parameterMap of
                            Just idx -> mkName (name) --  If the node is a variable, its name is used as the LLVM variable
                            _ -> error "variable name missing from parameterMap"
        _ -> mkName ("t" ++ show n) --  if the node is anything other than Var type, temporary variable name is generated.

    -- | mkTemp2 is for generating temporary variable names when there are more than 2 variables in an addition or multiplication operation
    mkTemp2 :: Int  -- ^ Takes the index of node as input
               -> Int -- ^ Takes the index of argument/ parameter as another input
               -> AST.Name -- ^ Builds a temporary variable name in LLVM "Name" Type concatenating the above two inputs
    mkTemp2 n m = mkName ("s"++show n++"_"++show m)

    -- | callFun is used for generating LLVM code for all operations that uses CALL instruction
    callFun :: String -- ^ name of the external function
               -> Int -- ^ node id
               -> Int -- ^ input parameter
               -> [Named AST.Instruction] -- ^ generated LLVM Instruction for the given function
    callFun funName n arg = [mkTemp n AST.:= AST.Call  { tailCallKind = Nothing -- :: Maybe TailCallKind,
                                                           , callingConvention = CC.C -- :: CallingConvention, Found in IR construction Instruction.hs
                                                           , returnAttributes = [] -- :: [PA.ParameterAttribute],
                                                           , function = Right (AST.ConstantOperand $ C.GlobalReference funcType (nameDef funName) ) -- :: CallableOperand,
                                                           , arguments = [(AST.LocalReference elemType $ mkTemp arg, [])] --  :: [(Operand, [PA.ParameterAttribute])],
                                                           , functionAttributes = [ ] -- :: [Either FA.GroupID FA.FunctionAttribute],
                                                           , metadata = [] -- :: InstructionMetadata
                                                           } ]

    -- | arithFun is used for generating LLVM code for arithmetic operations like Add and Mul
    arithFun :: (AST.FastMathFlags --  Flags for arithmetic instructions in LLVM
                 -> AST.Operand --  First operand of the arithmetic instruction
                 -> AST.Operand --  Second operand of the arithmetic instruction
                 -> AST.InstructionMetadata --  Specifies Meta data of the instruction. Eg., it says if it is inline or not and so on.
                 -> AST.Instruction ) -- ^ All the above inputs gives out this instruction
                 -> Int -- ^ node id
                 -> [Int] -- ^ list of input parameters
                 -> [Named AST.Instruction] -- ^ generated LLVM instruction for the given arithmetic operation with the given parameters
    arithFun instr n [] --  for Zero arguments
                        = [ mkTemp n AST.:=  instr AST.noFastMathFlags
                               (AST.ConstantOperand (C.Float $ LLVM.AST.Float.Double 0))
                               (AST.ConstantOperand $ C.Float $ LLVM.AST.Float.Double 0)
                               []]
    arithFun instr n (x:[]) --  For one argument
                        = let argName = mkTemp x
                        in[ mkTemp n AST.:=  instr AST.noFastMathFlags
                                     (AST.ConstantOperand (C.Float $ LLVM.AST.Float.Double 0))
                                     (AST.LocalReference elemType (argName))
                                     []]
    arithFun instr n (x:y:[]) --  For two arguments
                                = let argName1 = mkTemp x
                                      argName2 = mkTemp y
                                in[ mkTemp n AST.:=  instr AST.noFastMathFlags
                                     (AST.LocalReference elemType (argName1))
                                     (AST.LocalReference elemType (argName2))
                                     []]
    arithFun instr n (x:y:xs) = --  For more than two arguments, recursive call is used
      let
        helper (w1:w2:[]) =[ mkTemp2 w1 w2 AST.:=  instr AST.noFastMathFlags
                                      (AST.LocalReference elemType (mkTemp w1))
                                      (AST.LocalReference elemType (mkTemp w2))
                                      []]
        helper (w1:w2:w3:ws) = helper (w2:ws)
                            ++ [ mkTemp2 w1 w2 AST.:=  instr AST.noFastMathFlags
                                                                 (AST.LocalReference elemType (mkTemp w1))
                                                                 (AST.LocalReference elemType (mkTemp2 w2 w3))
                                                                 []]
      in
        helper (y:xs)
        ++ ( [ mkTemp n AST.:= instr AST.noFastMathFlags
                                              (AST.LocalReference elemType (mkTemp2 n y))
                                              (AST.LocalReference elemType (mkTemp x))
                                              []] )

    -- | gets the dimension of the expressionMap
    getShape :: Int -- ^ Node id of the expression map
                -> Shape -- ^ Dimension /shape of the expression map
    getShape nId = retrieveShape nId mp

    -- | generates LLVM Definition based on the node id obtained from the topological sort
    genCode :: Int -- ^ Node id of the expression map
               -> Code -- ^ generates corresponding LLVM instruction
    genCode n =
      let (shape, op) = retrieveInternal n mp
          elementType nId = retrieveElementType nId mp
      in case op of
          Var nam -> let --varName = mkTemp nam
                         parIdx = lookup nam parameterMap
                     in [] 
           --  For scalars we dont allocate space for local variables. We use mkTemp to refer to the variable name directly.
          DVar _ -> error "DVar not available for Scalars"
          Const val -> [ mkTemp n AST.:=   AST.FAdd AST.noFastMathFlags
                                              (AST.ConstantOperand (C.Float $ LLVM.AST.Float.Double 0)) -- Check me - Type of var and const
                                              (AST.ConstantOperand $ C.Float $ LLVM.AST.Float.Double val)
                                              []] --error "for i n [n `at` i <<- show val]"
          Sum _ args -> arithFun AST.FAdd n args
          Mul _ args -> arithFun AST.FMul n args
          Power x arg  -> [mkTemp n AST.:= AST.Call  { tailCallKind = Nothing -- :: Maybe TailCallKind,
                                                      , callingConvention = CC.C -- :: CallingConvention, Found in IR construction Instruction.hs
                                                      , returnAttributes = [] -- :: [PA.ParameterAttribute],
                                                      , function = Right (AST.ConstantOperand $ C.GlobalReference funcType2 (mkName "llvm.pow.f64") ) -- :: CallableOperand,
                                                      , arguments = [(AST.LocalReference elemType $ mkTemp arg, []),(AST.ConstantOperand (C.Float $ LLVM.AST.Float.Double $ fromIntegral(x)),[])]
                                                      , functionAttributes = [] -- :: [Either FA.GroupID FA.FunctionAttribute],
                                                      , metadata = [] -- :: InstructionMetadata
                                                      } ]
          Neg _ arg -> let argName = mkTemp arg
                       in [ mkTemp n AST.:= AST.FSub AST.noFastMathFlags
                                                  (AST.ConstantOperand (C.Float $ LLVM.AST.Float.Double 0))
                                                  (AST.LocalReference elemType (argName))
                                     []]     -- need to generate address, load, negate, address and store (and two temporary) for non scalars
          Scale _ scalar arg -> error "Scale should not be here" -- Not relevant to scalar operations
          -- MARK: only applicable to Real
          Div arg1 arg2  -> [ mkTemp n AST.:= AST.FDiv AST.noFastMathFlags
                                                   (AST.LocalReference elemType (mkTemp arg1))
                                                   (AST.LocalReference elemType (mkTemp arg2))
                                                   []]
                         --error "Div not implemented"
          Sqrt arg -> callFun "sqrt" n arg
                    --error "for i n [n `at` i <<- "sqrt" ++ arg `at` i]"
          Sin arg -> callFun "sin" n arg
                    --error "for i n [n `at` i <<- "sin" ++ arg `at` i]"
          Cos arg -> callFun "cos" n arg
                    --error "for i n [n `at` i <<- "cos" ++ arg `at` i]"
          Tan arg -> callFun "tan" n arg
                    --error "for i n [n `at` i <<- "tan" ++ arg `at` i]"
          Exp arg -> callFun "exp" n arg
                    --error "for i n [n `at` i <<- "exp" ++ arg `at` i]"
          Log arg -> callFun "log" n arg
                    --error "for i n [n `at` i <<- "log" ++ arg `at` i]"
          Sinh arg -> callFun "sinh" n arg
                    --error "for i n [n `at` i <<- "sinh" ++ arg `at` i]"
          Cosh arg -> callFun "cosh" n arg
                      --error "for i n [n `at` i <<- "cosh" ++ arg `at` i]"
          Tanh arg -> callFun "tanh" n arg
                      --error "for i n [n `at` i <<- "tanh" ++ arg `at` i]"
          Asin arg -> callFun "asin" n arg
                      --error "for i n [n `at` i <<- "asin" ++ arg `at` i]"
          Acos arg -> callFun "acos" n arg
                      --error "for i n [n `at` i <<- "acos" ++ arg `at` i]"
          Atan arg -> callFun "atan" n arg
                      --error "for i n [n `at` i <<- "atan" ++ arg `at` i]"
          Asinh arg -> callFun "asinh" n arg
                      --error "for i n [n `at` i <<- "asinh" ++ arg `at` i]"
          Acosh arg -> callFun "acosh" n arg
                      --error "for i n [n `at` i <<- "acosh" ++ arg `at` i]"
          Atanh arg -> callFun "atanh" n arg
                      --error "for i n [n `at` i <<- "atanh" ++ arg `at` i]"
          -- MARK: Complex related. Not related to Scalar
          RealImag arg1 arg2 -> error "RealImag not applicable for scalars"
          RealPart arg -> error "RealPart not applicable for scalars"
          ImagPart arg -> error "ImagPart not applicable for scalars"
          InnerProd _ arg1 arg2 -> error "InnerProd not applicable for scalars"
          Piecewise marks condition branches -> error "Piecewise not applicable for scalars"
          Rotate [amount] arg -> error "Rotate 1D not applicable for scalars"
          Rotate [amount1, amount2] arg -> error "Rotate 2D not applicable for scalars"
          Rotate [amount1, amount2, amount3] arg -> error "Rotate 3D not applicable for scalars"

    -- | defines the body of the LLVM module
    body = BasicBlock --  body of the LLVM module
           (Name "entry") --  defines the entry of the LLVM Module
           (concatMap genCode $ topologicalSortManyRoots (mp, rootIds)) --  gets all expression, sort them, generates LLVM instructions as per the sorted node ids
           (Do $ Ret (Just (LocalReference elemType (head $ map mkTemp rootIds))) []) --  return variable
  in
    GlobalDefinition functionDefaults --  Actual definition of LLVM Module
       { name = mkName funcName -- Name of the function
       , parameters =
           ( [ Parameter elemType (mkName name) [] | (name,_) <- sortedVars ] --  Input parameters which is sorted
           , False )
       , returnType = elemType --  Return type of the module
       , basicBlocks = [body] --  calling Body of LLVM module
       }

-- | defining type of functions
funcType = ptr $ FunctionType elemType [elemType] False
funcType2 = ptr $ FunctionType elemType [elemType,elemType] False

-- | defining names in LLVM "Name" type based on its function call
-- | nameDef -> String -> AST.Name
nameDef :: String -- ^ Name of the external function in terms of string
           -> AST.Name -- ^ Name of the external function in LLVM "Name" Type
nameDef name = case name of
                          "sin" -> mkName $ "llvm."++name++".f64"
                          "cos" -> mkName $ "llvm."++name++".f64"
                          "log" -> mkName $ "llvm."++name++".f64"
                          "exp" -> mkName $ "llvm."++name++".f64"
                          "pow" -> mkName $ "llvm."++name++".f64"
                          "sqrt" -> mkName $ "llvm."++name++".f64"
                          _ -> mkName name


-- | @mkModule@ function for combining all generated LLVM Definitions to a single LLVM module
-- | mkModule :: String -> Expression d et -> AST.Module
mkModule :: String -- ^ name for the LLVM module to be generated
            -> Expression d et  -- ^ Expression map
            -> AST.Module -- ^ LLVM module that has all generated LLVM Definitions
mkModule funcName exp =
  let 
    Expression topLevel exprMap  = exp 
    llvmMemMap = makeLLVMMemMap exprMap
  in 
    defaultModule --  has parameters for the default LLVM module
    { moduleName = "basic" --  describes the module id to be generated in LLVM code
    , moduleSourceFileName = "Main.hs" --  Describes the source file name from which LLVM code is generated
    , moduleDefinitions =  [ generateEvaluatingCodes funcName llvmMemMap (exprMap, [topLevel])] --  Complete LLVM module generation which calls generateEvaluatingCodes function with the expression map
      ++ externals --  External function call declarations in the LLVM program
    }

-- | Declaration of all predefined functions used by LLVM definitions
externals :: [AST.Definition] -- ^ List of LLVM Definitions
externals =
  let
    onePar = ([Parameter elemType (mkName "") []], False) --  Parameter list for functions with one parameter
    twoPar = ([Parameter elemType (mkName "") [], Parameter elemType (mkName "") []], False) --  Parameter list for functions with two parameter
    defn (name, attrs, par) = GlobalDefinition $ functionDefaults --  External Function declaration
      { name       = nameDef name --  Name of the function
      , linkage    = External --  For external symbol references
      , parameters = par --  Parameter list
      , returnType = elemType -- Return type of the function
      , LLVM.AST.Global.functionAttributes = attrs -- Attributes of the function
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
      ]  -- from IRBuilder.Module

-- | Steps to execute
-- 
-- [Go to the folder app in the HashedExpression]
-- 
-- >>> stack ghci Main.hs
--
-- >>> Prelude> main
--
-- [LLVM Generated Code]
--
-- [OR]
--
-- [Go to the project folder]
--
-- >>> stack build
--
-- >>> stack run
-- 
-- [LLVM Generated code]
-- 
-- [LLVM code is also found in the generated file specified in the Main.hs]
-- 
-- [To check the output with C file]
--
-- >>> clang sampleMod.ll testMain.c -O3 -o test.exe
--
-- >>> ./test.exe
--
-- >>> 0.841471 [Some output based on the LLVM code and given input from C file] 
