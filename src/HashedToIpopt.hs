{-# LANGUAGE TypeApplications,DataKinds #-}

module HashedToIpopt where

import HashedExpression
import HashedUtils
import HashedNode
import HashedToC
import HashedSolver
import HashedDerivative
import HashedExpression
import HashedInterp
import HashedNormalize
import HashedOperation hiding (product, sum)
import qualified HashedOperation
import HashedPrettify

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.IntMap (IntMap)
import qualified Data.Map as Map
import qualified Data.IntMap as I
import Data.List as L

--import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import qualified Data.Set as Set

import qualified Data.Array as Array

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

-- TODO remove me
import GHC.Stack
import Debug.Trace
debug = (flip  trace)


-- | Variable and Type Identifiers
--
idNumVars = "n"
idINPUTSIZE = "INPUTSIZE"
idMEMSIZE   = "MEMSIZE"
idCONSTRAINTSIZE = "CONSTRAINTSIZE"
idNumConstraints = "m"
idIndex = "ind"
idLowerBoundsX = "x_L"
idUpperBoundsX = "x_U"
idLowerBoundsG = "g_L"
idUpperBoundsG = "g_U"
idIpoptProblem = "nlp"
idStatus     = "status"
idVarX       = "x"
idMultLowerX = "mult_x_L"
idMultUpperX = "mult_x_U"
idObjective  = "obj"
idCounter    = "i"
idEvalf      = "eval_f"
idEvalg      = "eval_g"
idEvalGradf   = "eval_grad_f"
idEvalJacg    = "eval_jac_g"
idEvalHess    = "eval_h"
idSharedData  = "sdata"
idNELEJAC = "NELE_JAC"
idNELEHESS = "NELE_HESS"
idPtr = "ptr"

typeIdIndex = "Index"
typeIdNumber = "Number"
typeIdSharedData = "SharedData"

indent str = "  " ++ str
padding = map T.pack ["",""]

-- TODO move me to HashedToC???
memcpyCode (destPtr,destIdx) (srcPtr,srcIdx) (typeId,size) =
  "memcpy(&"++destPtr++"["++show destIdx++"],&"++srcPtr++"["++show srcIdx++"],sizeof("++typeId++")*"++show size++");"

-- | Templates
--
mainTemplate :: ValMaps -> Problem -> [T.Text]
mainTemplate valMaps (Problem vars objId exprMap memMap mBoxConstraints mScalarConstraints) =
  let
     -- ptr offset (to be used in c code) for the resulting value of the obj function
     objOffset :: Int
     objOffset = memOffset memMap objId LookupR
     -- code for evaluating objective function
     objEvalCode = generateEvaluatingCodes memMap (exprMap,[objId])
     -- ptr offset (to be used in c code) for each partial derivative and their sizes
     objDiffOffsetsSize = map (\idx -> (memOffset memMap idx LookupR, product $ retrieveShape idx exprMap)) $
                          map partialDerivativeId vars
     -- code for evaluating ALL partial derivatives of objective function
     objDiffEvalCode = generateEvaluatingCodes memMap (exprMap,map partialDerivativeId vars)
     -- variables with their dimensions/sizes
     varsWithSizes :: [(Variable, Int)]
     varsWithSizes = map (\v -> (v, product $ retrieveShape (nodeId v) exprMap)) vars
     -- vairable offsets and sizes
     varsOffsetsSizes :: [(Int,Int)]
     varsOffsetsSizes = map (\(v,sz) -> (memOffset memMap (nodeId v) LookupR,sz)) varsWithSizes
     -- total number of primal vars (size of idVarX)
     totalNumVars = sum $ map snd varsWithSizes
     -- create code to memcpy x to SharedData
     xPtrOffsetPairs = zip varsOffsetsSizes (scanl (+) 0 $ map snd varsOffsetsSizes)
     x2PtrMemCpyCode = ["/* copy x to SharedData */"] ++
                       map (\((oOff,oSz),i) -> memcpyCode (idPtr,oOff) (idVarX,i) (typeIdNumber,oSz))
                       xPtrOffsetPairs
     -- total memory size of SharedData (includes primals and all other nodes)
     memSize = totalDoubles memMap
     -- size of known nonzeros in jacbian/hessian
     nele_jac = totalNumVars * constraintSize -- TODO define nele_jac
     nele_hess = 0  -- TODO is nele_hess correct?
     -- number of constraints
     constraintSize = length scalarConsIDs
     -- collect all scalar constraints
     scalarConsIDs = case mScalarConstraints of
                       Just xs -> map constraintValueId xs
                       Nothing -> []
     scalarConsOffsetSizes = map (\i -> (memOffset memMap i LookupR,
                                             product $ retrieveShape i exprMap)) $ scalarConsIDs
     scalarConsEvalCode = generateEvaluatingCodes memMap (exprMap,scalarConsIDs)
     -- collect partials of scalar constraints
     partialScalarConsIDs = case mScalarConstraints of
                              Just xs -> concatMap constraintPartialDerivatives xs
                              Nothing -> []
     paritalScalarOffsetsSizes = map (\i -> (memOffset memMap i LookupR,
                                             product $ retrieveShape i exprMap)) $ partialScalarConsIDs
     partialScalarConsEvalCode = generateEvaluatingCodes memMap (exprMap,partialScalarConsIDs)
  in concat $ intersperse padding [
     includesTemplate
    ,typedefTemplate
    ,definesTemplate totalNumVars nele_jac nele_hess memSize constraintSize
    ,evalfTemplate varsOffsetsSizes objOffset objEvalCode x2PtrMemCpyCode
    ,evalGradfTemplate objDiffOffsetsSize objDiffEvalCode x2PtrMemCpyCode
    ,evalhTemplate
    ,evalgTemplate scalarConsIDs scalarConsOffsetSizes scalarConsEvalCode x2PtrMemCpyCode
    ,evalJacTemplate paritalScalarOffsetsSizes partialScalarConsEvalCode x2PtrMemCpyCode
    ,map T.pack ["int main()","{"]
    ,varDeclareTemplate
    ,allocateTemplate objOffset
    ,initializeTemplate valMaps memMap exprMap
    ,createIpoptTemplate nele_jac nele_hess
    ,freeBoundsTemplate
    ,addOptionsTemplate
    ,ipoptSolveTemplate
    -- ,printSolutionTemplate TODO include printSolutionTemplate
    ,freeSolutionTemplate
    ,map T.pack ["  return 0;","}"]
    ]

includesTemplate :: [T.Text]
includesTemplate = map T.pack
 ["#include \"IpStdCInterface.h\""
 ,"#include <stdlib.h>"
 ,"#include <assert.h>"
 ,"#include <stdio.h>"
 ,"#include <math.h>"
                   ]

typedefTemplate :: [T.Text]
typedefTemplate = map T.pack
  ["struct _"++typeIdSharedData++" {"++typeIdNumber++"* data; int data_size; };"
  ,"typedef struct _"++typeIdSharedData++"* "++typeIdSharedData++";"]

definesTemplate :: Int -> Int -> Int -> Int -> Int -> [T.Text]
definesTemplate inputSize neleJac neleHess memSize constraintSize = map T.pack
  ["#define "++idINPUTSIZE ++ "  " ++ show inputSize
  ,"#define "++idNELEJAC ++ "  " ++ show neleJac
  ,"#define "++idNELEHESS ++ "  " ++ show neleHess
  ,"#define "++idMEMSIZE++ "  " ++ show memSize
  ,"#define "++idCONSTRAINTSIZE++ "  " ++ show constraintSize
  ]
varDeclareTemplate :: [T.Text]
varDeclareTemplate = map (T.pack . indent)
  [typeIdIndex++" "++idNumVars++"=-1;                          /* number of variables */"
  ,typeIdIndex++" "++idNumConstraints++"=-1;                          /* number of constraints */"
  ,typeIdNumber++"* "++idLowerBoundsX++" = NULL;                  /* lower bounds on x */"
  ,typeIdNumber++"* "++idUpperBoundsX++" = NULL;                  /* upper bounds on x */"
  ,typeIdNumber++"* "++idLowerBoundsG++" = NULL;                  /* lower bounds on g */"
  ,typeIdNumber++"* "++idUpperBoundsG++" = NULL;                  /* upper bounds on g */"
  ,"IpoptProblem "++idIpoptProblem++" = NULL;             /* IpoptProblem */"
  ,"enum ApplicationReturnStatus "++idStatus++"; /* Solve return code */"
  ,typeIdNumber++"* "++idVarX++" = NULL;                    /* starting point and solution vector */"
  ,typeIdNumber++"* "++idMultLowerX++" = NULL;             /* lower bound multipliers at the solution */"
  ,typeIdNumber++"* "++idMultUpperX++" = NULL;             /* upper bound multipliers at the solution */"
  ,typeIdNumber++" "++idObjective++";                          /* objective value */"
  ,typeIdIndex++" "++idCounter++";                             /* generic counter */"
  ,typeIdSharedData++" "++idSharedData++";"
  ]

allocateTemplate :: Int -> [T.Text]
allocateTemplate objOffset = map (T.pack . indent)
  [idSharedData ++ " = ("++typeIdSharedData++")malloc(sizeof(struct _"++typeIdSharedData++"));"
  ,idSharedData ++ "->data = ("++typeIdNumber++"*)malloc(sizeof("++typeIdNumber++")*"++idMEMSIZE++");"
  ,idSharedData++"->data_size = "++idMEMSIZE++";"
  ,idVarX++" = &"++idSharedData++"->data["++show objOffset++"];" -- point x to SharedData instead of allocating
  ,idLowerBoundsX++" = ("++typeIdNumber++"*)malloc(sizeof("++typeIdNumber++")*"++idINPUTSIZE++");"
  ,idUpperBoundsX++" = ("++typeIdNumber++"*)malloc(sizeof("++typeIdNumber++")*"++idINPUTSIZE++");"
  -- TODO allocate constraints g_L,g_U in allocateTemplate
  ]

initializeTemplate :: ValMaps -> MemMap -> ExpressionMap -> [T.Text]
initializeTemplate valMaps memMap exprMap = map (T.pack . indent) $
  ["/* Performing Initializion */"]
  ++
  generateAssignValueCodes valMaps memMap exprMap -- TODO load from file isn't configured?

createIpoptTemplate :: Int -> Int -> [T.Text]
createIpoptTemplate nele_jac nele_hess = map (T.pack . indent)
  [ idIpoptProblem ++ " = CreateIpoptProblem("
  ++ idNumVars ++ ","
  ++ idLowerBoundsX ++ ","
  ++ idUpperBoundsX ++ ","
  ++ idNumConstraints ++ ","
  ++ idLowerBoundsG ++ ","
  ++ idUpperBoundsG ++ ","
  ++ idUpperBoundsG ++ ","
  ++ show nele_jac ++ ","
  ++ show nele_hess ++ ","
  ++ "0," -- index style (start counting row/column indices at 0)
  ++ "&" ++ idEvalf ++ ","
  ++ "&" ++ idEvalg ++ ","
  ++ "&" ++ idEvalGradf ++ ","
  ++ "&" ++ idEvalJacg ++ ","
  ++ "&" ++ idEvalHess
  ++ ");"
  ]

freeBoundsTemplate :: [T.Text]
freeBoundsTemplate = map (T.pack . indent)
  [ "free(" ++ idLowerBoundsX ++ ");"
  , "free(" ++ idUpperBoundsX ++ ");"
  , "free(" ++ idLowerBoundsG ++ ");"
  , "free(" ++ idUpperBoundsG ++ ");"
  ]

-- TODO un-hardcode addOptionsTemplate
addOptionsTemplate :: [T.Text]
addOptionsTemplate = map (T.pack . indent)
  ["AddIpoptNumOption(nlp, \"tol\", 1e-5);"
  ,"AddIpoptStrOption(nlp, \"mu_strategy\", \"adaptive\");"
  ,"AddIpoptNumOption(nlp, \"print_frequency_time\", 10);"
  ,"AddIpoptStrOption(nlp, \"hessian_approximation\",\"limited_memory\");"
  -- NOTE turns on lbfgs hessian approximation (hence eval_h isn't used)
  -- see https://www.coin-or.org/Ipopt/documentation/node31.html
  ]

ipoptSolveTemplate :: [T.Text]
ipoptSolveTemplate = map (T.pack . indent)
  [idMultLowerX ++ " = ("++typeIdNumber++"*)malloc(sizeof("++typeIdNumber++")*"++idINPUTSIZE++");"
  ,idMultUpperX ++ " = ("++typeIdNumber++"*)malloc(sizeof("++typeIdNumber++")*"++idINPUTSIZE++");"
  ,idStatus ++ " = IpoptSolve("
  ++ idIpoptProblem ++ ","
  ++ idVarX ++ ","
  ++ "NULL,"
  ++ "&" ++ idObjective ++ ","
  ++ "NULL,"
  ++ idMultLowerX ++ ","
  ++ idMultUpperX ++ ","
  ++ "(void*)"++idSharedData++");"
  ]

printSolutionTemplate :: [T.Text]
printSolutionTemplate = undefined -- TODO finish printSolutionTemplate

freeSolutionTemplate :: [T.Text]
freeSolutionTemplate = map (T.pack . indent)
  ["FreeIpoptProblem(" ++ idIpoptProblem ++ ");"
  ,"free(" ++ idVarX ++ ");"
  ,"free(" ++ idMultLowerX ++ ");"
  ,"free(" ++ idMultUpperX ++ ");"
  ,"free(" ++ idSharedData ++ ");"
  ]

-- | Template for objective function
--
evalfTemplate :: [(Int,Int)] -> Int -> Code -> Code -> [T.Text]
evalfTemplate varsOffsetsSizes objOffset objEvalCode x2PtrMemCpyCode =
  let
    idNewX = "new_x"
    idObjVal = "obj_value"
    idPtr = "ptr"
  in map (T.pack)
  ["Bool eval_f("++typeIdIndex++" "++idIndex++","
                 ++typeIdNumber++"* "++idVarX++","
                 ++"Bool "++idNewX++","
                 ++typeIdNumber++"* "++idObjVal++","
                 ++"UserDataPtr user_data)"
  ,"{"
  ]
  ++
  map (T.pack . indent)
  [typeIdSharedData ++ " " ++ idSharedData ++ " = (" ++ typeIdSharedData ++ ")user_data;"
  ,typeIdNumber++" *ptr = "++idSharedData++"->data;"
  ]
  ++
  padding
  ++
  -- TODO point x to SharedData and remove me (after fixing MemMap allocation)
  map (T.pack . indent) x2PtrMemCpyCode
  ++
  padding
  ++
  map (T.pack . indent) (["/* evaluate objective function here*/"] ++ objEvalCode)
  ++
  padding
  ++
  map (T.pack . indent) ["/* assign resulting objective function evaluation */"
                        ,"*"++idObjVal++" = ptr["++show objOffset++"];"]
  ++
  [(T.pack . indent) "return TRUE;"
   ,T.pack "}"]

-- | Template for objective function
--
evalGradfTemplate :: [(Int,Int)] -> Code -> Code -> [T.Text]
evalGradfTemplate objDiffOffsetsSizes objDiffEvalCode x2PtrMemCpyCode =
  let
    idGradf = "grad_f"
    idPtr   = "ptr"
    valPtrOffsetPairs = zip objDiffOffsetsSizes (scanl (+) 0 $ map snd objDiffOffsetsSizes)
    gradMemCpyCode = map (\((gOff,gSz),i) -> memcpyCode (idGradf,i) (idPtr,gOff) (typeIdNumber,gSz))
                     valPtrOffsetPairs
  in map (T.pack)
  ["Bool eval_grad_f("++typeIdIndex++" "++idIndex++","
                      ++typeIdNumber++"* "++idVarX++","
                      ++typeIdNumber++"* "++idGradf++","
                      ++"UserDataPtr user_data)"
  ,"{"
  ]
  ++
  map (T.pack . indent)
  [typeIdSharedData ++ " " ++ idSharedData ++ " = (" ++ typeIdSharedData ++ ")user_data;"
  ,typeIdNumber++" *ptr = "++idSharedData++"->data;"
  ]
  ++
  padding
  ++
  -- TODO point x to SharedData and remove me (after fixing MemMap allocation)
  map (T.pack .indent) x2PtrMemCpyCode
  ++
  padding
  ++
  map (T.pack . indent) (["/* evaluate gradient here */"] ++ objDiffEvalCode)
  ++
  padding
  ++
  map (T.pack . indent ) (["/* copy values from SharedData */"] ++ gradMemCpyCode)
  ++
  padding
  ++
  [(T.pack . indent) "return TRUE;"
   ,T.pack "}"]

-- | Template for the hessian
--
evalhTemplate :: [T.Text]
evalhTemplate =
  let
    idNewX = "new'_x"
    idObjFact = "obj_factor"
    idShared = "user_data"
    idIndexM = "index_m"
    idLambda = "lambda"
    idHessN = "nele_hess"
    idIdxRow = "iRow"
    idIdxCol = "jCol"
    idValues = "values"
  in map (T.pack)
  ["Bool eval_h("++typeIdIndex++" "++idIndex++","
                 ++typeIdNumber++"* "++idVarX++","
                 ++"Bool "++idNewX++","
                 ++typeIdNumber++" "++idObjFact++","
                 ++typeIdIndex++" "++idIndexM++","
                 ++typeIdNumber++" *"++idLambda++","
                 ++typeIdIndex++" "++idHessN++","
                 ++typeIdIndex++" *"++idIdxRow++","
                 ++typeIdIndex++" *"++idIdxCol++","
                 ++typeIdNumber++" *"++idValues++","
                 ++"UserDataPtr " ++ idShared++")"
  ,"{"
  ]
  ++
  -- NOTE when hessian_approximation option is on (as is current default) eval_h should be empty and return false
  -- TODO implement option to turn hessian_approximation off and implement evaluation of hessian here
  [(T.pack . indent) "return FALSE; /* AddIpoptStrOption hessian_approximation set */" -- set to true if hessian_appromation is off
  ,T.pack "}"]

-- | Template for constraint function
--
evalgTemplate :: [Int] -> [(Int,Int)] -> Code -> Code -> [T.Text]
evalgTemplate scalarConsIDs scalarConsOffsetSizes scalarConsEvalCode x2PtrMemCpyCode =
  let
    idNewX = "new_x"
    idVarG = "g"
    idNumG = "m"
    idPtr  = "ptr"
    valPtrOffsetPairs = zip scalarConsOffsetSizes (scanl (+) 0 $ map snd scalarConsOffsetSizes)
    -- FIXME ? correct size, should sSz == 1 for testExprAndConstraints?
    constMemCpy = map (\((sOff,sSz),i) -> memcpyCode (idVarG,i) (idPtr,sOff) (typeIdNumber,sSz))
                      valPtrOffsetPairs
  in map (T.pack)
  ["Bool eval_g("++typeIdIndex++" "++idIndex++","
                      ++typeIdNumber++"* "++idVarX++","
                      ++"Bool "++idNewX++","
                      ++typeIdIndex ++ " "++idNumG++","
                      ++typeIdNumber++"* "++idVarG++","
                      ++"UserDataPtr user_data)"
  ,"{"
  ]
  ++
  map (T.pack . indent)
  [typeIdSharedData ++ " " ++ idSharedData ++ " = (" ++ typeIdSharedData ++ ")user_data;"
  ,typeIdNumber++" *ptr = "++idSharedData++"->data;"
  ]
  ++
  padding
  ++
  -- TODO point x to SharedData and remove me (after fixing MemMap allocation)
  map (T.pack . indent) x2PtrMemCpyCode
  ++
  padding
  ++
  map (T.pack . indent) (["/* evaluate constraints */"] ++  scalarConsEvalCode)
  ++
  padding
  ++
  map (T.pack . indent) (["/* copy from SharedData */"] ++ constMemCpy)
  ++
  padding
  ++
  [(T.pack . indent) "return TRUE;"
   ,T.pack "}"]

-- | Template for constraint function
--
evalJacTemplate :: [(Int,Int)] -> Code -> Code -> [T.Text]
evalJacTemplate partialScalarOffsetsSizes partialScalarConsEvalCode x2PtrMemCpyCode =
  let
    idNewX = "new_x"
    idNumG = "m"
    idNeleJac = "nele_jac"
    idRowSkip = "iRow"
    idColSkip = "iCol"
    idValues = "values"
    idPtr = "ptr"
    valPtrOffsetPairs = zip partialScalarOffsetsSizes (scanl (+) 0 $ map snd partialScalarOffsetsSizes)
    jacMemCpyCode = map (\((pOff,pSz),i) -> memcpyCode (idValues,i) (idPtr,pOff) (typeIdNumber,pSz))
                    valPtrOffsetPairs
  in map (T.pack)
  ["Bool eval_jac_g("++typeIdIndex++" "++idIndex++","
                     ++typeIdNumber++"* "++idVarX++","
                     ++"Bool "++idNewX++","
                     ++typeIdIndex ++ " "++idNumG++","
                      ++typeIdIndex++" "++idNeleJac++","
                      ++typeIdIndex++"* "++idRowSkip++","
                      ++typeIdIndex++"* "++idColSkip++","
                      ++typeIdNumber++"* "++idValues++","
                      ++"UserDataPtr user_data)"
  ,"{"
  ]
  ++
  map (T.pack . indent)
  [typeIdSharedData ++ " " ++ idSharedData ++ " = (" ++ typeIdSharedData ++ ")user_data;"
  ,typeIdNumber++" *ptr = "++idSharedData++"->data;"
  ]
  ++
  padding
  ++
  map (T.pack . indent)
  ["if ("++idValues++" == NULL) {"]
  ++
  map (T.pack . indent . indent) ["/* return structure of jacboian */"]
  -- TODO implement structure of jacbian in evalJacTemplate
  ++
  map (T.pack . indent)
  ["}"
  ,"else {"]
  ++
  -- TODO point x to SharedData and remove me (after fixing MemMap allocation)
  map (T.pack . indent) (["/* copy objective vars to SharedData */"] ++ x2PtrMemCpyCode)
  ++
  padding
  ++
  map (T.pack . indent) ( ["/* eval values of the jacbian of constraints */"]
                          ++ partialScalarConsEvalCode)
  ++
  padding
  ++
  map (T.pack . indent) ( ["/* copy values from SharedData */"]
                          ++ jacMemCpyCode)
  ++
  [(T.pack . indent) "}"]
  ++
  [(T.pack . indent) "return TRUE;"
   ,T.pack "}"]


-- | Generate C Code Ipopt File
--
--generateProblemIpopt :: ValMaps -> Problem -> GenResult
generateProblemIpopt :: ValMaps -> Problem -> [T.Text]
generateProblemIpopt valMaps problem@(Problem vars objId exprMap memMap mBoxConstraints mScalarConstraints) =
  let
     objCodes = generateEvaluatingCodes memMap (exprMap,[objId]) -- TODO DELETE ME
     -- ptr offset (to be used in c code) for the resulting value of the obj function
     objOffset :: Int
     objOffset = memOffset memMap objId LookupR
     -- variables with thier dimensions/sizes
     varsWithShapes :: [(String, Shape)]
     varsWithShapes = map (\v -> (varName v, retrieveShape (nodeId v) exprMap)) vars
     varsWithSizes :: [(String, Int)]
     varsWithSizes = map (\(vN,vS) -> (vN,product vS)) varsWithShapes
     -- total number of primal vars (size of idVarX, multiply this by sizeof(NumberType) when allocating)
     totalNumVars = sum $ map snd varsWithSizes

     -- compute main template
     code = mainTemplate valMaps problem
  in code `debug` (show vars)

--expressionToIpopt :: Expression Scalar R -> [String] -> (Code,Problem)
expressionToIpopt valMaps (expr@(Expression exprIdx exprMap),constraints) vars =
  let
    setVars = Set.fromList vars
    problem@(Problem vrs objIdx exprMap memMap mBoxConstraints mScalarConstraints) =
      case constructProblem expr vars constraints of
        ProblemValid prob -> prob
        ProblemInvalid msg -> error $ "constructProblem Invalid: "++msg
  in generateProblemIpopt valMaps problem

testProblem :: Problem
testProblem =
  let
    (testExpr,constraints) = testExprAndConstraints
  in case constructProblem testExpr (["x","y"]) constraints of
       ProblemValid prob -> prob
       ProblemInvalid msg -> error $ "constructProblem Invalid: "++msg

testExprAndConstraints :: (Expression Scalar R,Constraint)
testExprAndConstraints =
  let
    [x, y] = map (variable2D @256 @256) ["x", "y"]
    z = x * x + y
    xCon = x .<= (V2D $ Array.listArray ((0,0),(255,255)) $ repeat 5)
    yCon = y .<= (V2D $ Array.listArray ((0,0),(255,255)) $ repeat 5)
    sCon = (const 2) * (x <.> y) .>= VScalar 1
  in (sumElements z,IPOPTConstraint [xCon,yCon,sCon])

testValMaps = Map.fromList [("x",(V2D $ Array.listArray ((0,0),(255,255)) $ repeat 0))
                           ,("y",(V2D $ Array.listArray ((0,0),(255,255)) $ repeat 0))]

writeExprToIpopt file valMaps expr vars = TIO.writeFile file $ T.unlines $ expressionToIpopt valMaps expr vars
testFile = "testIpopt.c"

runTest = writeExprToIpopt testFile testValMaps testExprAndConstraints ["x","y"]
