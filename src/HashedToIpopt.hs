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
import qualified Data.IntMap as I
import Data.List as L

--import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import qualified Data.Set as Set

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
import Debug.Trace
debug = (flip  trace)

-- | Variable and Type Identifiers
--
idNumVars = "n"
idINPUTSIZE = "INPUTSIZE"
idMEMSIZE   = "MEMSIZE"
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

typeIdIndex = "Index"
typeIdNumber = "Number"
typeIdSharedData = "SharedData"

indent str = "  " ++ str
padding = map T.pack ["",""]

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
     objDiffOffsetsSize = map (\i -> (memOffset memMap i LookupR, product $ retrieveShape i exprMap)) $
                          map partialDerivativeId vars
     -- code for evaluating ALL partial derivatives of objective function
     objDiffEvalCode = generateEvaluatingCodes memMap (exprMap,map partialDerivativeId vars)
     -- variables with thier dimensions/sizes
     varsWithShapes :: [(String, Shape)]
     varsWithShapes = map (\v -> (varName v, retrieveShape (nodeId v) exprMap)) vars
     varsWithSizes :: [(String, Int)]
     varsWithSizes = map (\(vN,vS) -> (vN,product vS)) varsWithShapes
     -- total number of primal vars (size of idVarX)
     totalNumVars = sum $ map snd varsWithSizes
     -- total memory size of SharedData (includes primals and all other nodes)
     memSize = totalDoubles memMap
     -- size of jacbian and hessian
     nele_jac = 0 -- TODO define nele_jac
     nele_hess = 0  -- TODO is nele_hess correct?
  in
  includesTemplate
  ++ padding
  ++ typedefTemplate
  ++ definesTemplate totalNumVars nele_jac nele_hess memSize
  ++ padding
  ++ evalfTemplate objOffset objEvalCode
  ++ padding
  ++ evalGradfTemplate objDiffOffsetsSize objDiffEvalCode
  ++ padding
  ++ evalhTemplate
  ++ padding
  ++ evalgTemplate
  ++ padding
  ++ evalJacTemplate
  ++ padding
  ++ map T.pack
     ["int main()"
     ,"{"]
  ++ varDeclareTemplate
  ++ padding
  ++ allocateTemplate objOffset
  -- ++ initializeTemplate TODO include initializeTemplate in main template
  ++ padding
  ++ createIpoptTemplate nele_jac nele_hess
  ++ padding
  ++ freeBoundsTemplate
  ++ padding
  ++ addOptionsTemplate
  ++ padding
  ++ ipoptSolveTemplate
  ++ padding
  -- ++ printSolutionTemplate TODO include printSolutionTemplate
  ++ padding
  ++ freeSolutionTemplate
  ++ map T.pack
     ["  return 0;"
     ,"}"]

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

definesTemplate :: Int -> Int -> Int -> Int -> [T.Text]
definesTemplate inputSize neleJac neleHess memSize = map T.pack
  ["#define "++idINPUTSIZE ++ "  " ++ show inputSize
  ,"#define "++idNELEJAC ++ "  " ++ show neleJac
  ,"#define "++idNELEHESS ++ "  " ++ show neleHess
  ,"#define "++idMEMSIZE++ "  " ++ show memSize
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

initializeTemplate :: [T.Text]
initializeTemplate = undefined -- TODO finish initializeTemplate (break up into before/after createIpopt)
                               -- initialize bounds and constraints before, initial point after

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
evalfTemplate :: Int -> [String] -> [T.Text]
evalfTemplate objOffset objEvalCode =
  let
    idNewX = "new_x"
    idObjVal = "obj_value"
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
  map (T.pack . indent) objEvalCode
  ++
  padding
  ++
  map (T.pack . indent)
  ["*"++idObjVal++" = ptr["++show objOffset++"];"]
  ++
  [(T.pack . indent) "return TRUE;"
   ,T.pack "}"]

-- | Template for objective function
--
evalGradfTemplate :: [(Int,Int)] -> [String] -> [T.Text]
evalGradfTemplate objDiffOffsetsSizes objDiffEvalCode =
  let
    idGradf = "grad_f"
    idPtr   = "ptr"
    gradAssignsLoops = scoped $ ["int tmp = 0;"] ++ concatMap gradAssignLoop objDiffOffsetsSizes
    gradAssignLoop (offset,sz) = forRange "i" sz [gradAssignCode "tmp" ("i + "++show offset)
                                                 ,"tmp++;"]
    gradAssignCode gradfIdx ptrIdx = idGradf++"["++gradfIdx++"]" <<- idPtr++"["++ptrIdx++"]"
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
  map (T.pack . indent) objDiffEvalCode
  ++
  padding
  ++
  map (T.pack . indent )["/* populize grad_f here */"]-- TODO finish evalGradfTemplate'
  ++
  map (T.pack . indent) gradAssignsLoops
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
  -- TODO finish evalhTemplate
  -- (NOTE only necessary when turning off hessian_approximation option)
  -- ++
  [(T.pack . indent) "return FALSE; /* AddIpoptStrOption hessian_approximation set */" -- set to true if hessian_appromation is off
  ,T.pack "}"]

-- | Template for constraint function
--
evalgTemplate :: [T.Text]
evalgTemplate  =
  let
    idNewX = "new_x"
    idVarG = "g"
    idNumG = "m"
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
  -- TODO finish evalgTemplate
  ++
  [(T.pack . indent) "return TRUE;"
   ,T.pack "}"]

-- | Template for constraint function
--
evalJacTemplate :: [T.Text]
evalJacTemplate  =
  let
    idNewX = "new_x"
    idNumG = "m"
    idNeleJac = "nele_jac"
    idRowSkip = "iRow"
    idColSkip = "iCol"
    idValues = "values"
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
  map (T.pack . indent . indent) ["/* return the values of the jacbian of constraints */"]
  --TODO implement jacbian evaluation of constraints in evalJacTemplate
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
expressionToIpopt (expr@(Expression exprIdx exprMap),constraints) vars =
  let
    setVars = Set.fromList vars
    problem@(Problem vrs objIdx exprMap memMap mBoxConstraints mScalarConstraints) =
      case constructProblem expr vars undefined of
        ProblemValid prob -> prob
        ProblemInvalid msg -> error $ "constructProblem Invalid: "++msg
  in generateProblemIpopt undefined problem

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
    z = x * y
    xCon = x .<= VScalar 5
    yCon = y .<= VScalar 5
  in (sumElements z,IPOPTConstraint [xCon,yCon])

writeExprToIpopt file expr vars = TIO.writeFile file $ T.unlines $ expressionToIpopt expr vars
testFile = "testIpopt.c"

runTest = writeExprToIpopt testFile testExprAndConstraints ["x","y"]
