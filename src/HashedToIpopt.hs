{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}

module HashedToIpopt where


import HashedToC
import HashedExpression

import qualified Data.Text as T
import Data.IntMap (IntMap)
import qualified Data.IntMap as I
import Data.List as L

--import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C

-- | Variable and Type Identifiers
--
identNumVars = "n"
identNumConstraints = "m"
identLowerBoundsX = "x_L"
identUpperBoundsX = "x_U"
identLowerBoundsG = "g_L"
identUpperBoundsG = "g_U"
identIpoptProblem = "nlp"
identStatus = "status"
identVarX   = "x"
identMultLowerX = "mult_x_L"
identMultUpperX = "mult_x_U"
identObjective  = "obj"
identCounter    = "i"
identEvalf      = "eval_f"
identEvalg      = "eval_g"
identEvalGradf   = "eval_grad_f"
identEvalJacg    = "eval_jac_g"
identEvalHess    = "eval_h"
indexType = "Index"
numberType = "Number"

indent str = "  " ++ str

-- | Templates
--

mainTemplate :: [T.Text]
mainTemplate =
  let
    nele_jac = undefined -- TODO define nele_jac from MemMap
    nele_hess = undefined  -- TODO define nele_hess from MemMap
  in
  includesTemplate
  ++ map T.pack
     ["int main()"
     ,"{"]
  ++ allocateTemplate
  ++ varInitTemplate
  ++ createIpoptTemplate nele_jac nele_hess
  ++ freeBoundsTemplate
  ++ addOptionsTemplate
  ++ ipoptSolveTemplate
  ++ printSolutionTemplate
  ++ freeSolutionTemplate
  ++ map T.pack
     ["  return 0;"
     ,"}"]

includesTemplate :: [T.Text]
includesTemplate = map T.pack
  ["#include \"IpStdCInterface.h\""
 ,"#include <stdlib.h>"
 ,"#incldue <assert.h>"
 ,"#include <stdio.h>"
 ,"#include <math.h>"
                   ]

varInitTemplate :: [T.Text]
varInitTemplate = map (T.pack . indent)
  [indexType++" "++identNumVars++"=-1;                          /* number of variables */"
  ,indexType++" "++identNumConstraints++"=-1;                          /* number of constraints */"
  ,numberType++"* "++identLowerBoundsX++" = NULL;                  /* lower bounds on x */"
  ,numberType++"* "++identUpperBoundsX++" = NULL;                  /* upper bounds on x */"
  ,numberType++"* "++identLowerBoundsG++" = NULL;                  /* lower bounds on g */"
  ,numberType++"* "++identUpperBoundsG++" = NULL;                  /* upper bounds on g */"
  ,"IpoptProblem "++identIpoptProblem++" = NULL;             /* IpoptProblem */"
  ,"enum ApplicationReturnStatus "++identStatus++"; /* Solve return code */"
  ,numberType++"* "++identVarX++" = NULL;                    /* starting point and solution vector */"
  ,numberType++"* "++identMultLowerX++" = NULL;             /* lower bound multipliers at the solution */"
  ,numberType++"* "++identMultUpperX++" = NULL;             /* upper bound multipliers at the solution */"
  ,numberType++" "++identObjective++";                          /* objective value */"
  ,indexType++" "++identCounter++";                             /* generic counter */"
  ]

allocateTemplate :: [T.Text]
allocateTemplate = undefined -- TODO finish allocateTemplate (break up into before/after createIpopt)
                             -- allocate bounds/constraints before / x and bound multipliers after

initializeTemplate :: [T.Text]
initializeTemplate = undefined -- TODO finish initializeTemplate (break up into before/after createIpopt)
                               -- initialize bounds and constraints before, initial point after

createIpoptTemplate :: Int -> Int -> [T.Text]
createIpoptTemplate nele_jac nele_hess = map (T.pack . indent)
  [ identIpoptProblem ++ " = CreateIpoptProblem("
  ++ identNumVars ++ ","
  ++ identLowerBoundsX ++ ","
  ++ identUpperBoundsX ++ ","
  ++ identNumConstraints ++ ","
  ++ identLowerBoundsG ++ ","
  ++ identUpperBoundsG ++ ","
  ++ identUpperBoundsG ++ ","
  ++ show nele_jac ++ ","
  ++ show nele_hess ++ ","
  ++ "0," -- index style (start counting row/column indices at 0)
  ++ "&" ++ identEvalf ++ ","
  ++ "&" ++ identEvalg ++ ","
  ++ "&" ++ identEvalGradf ++ ","
  ++ "&" ++ identEvalJacg ++ ","
  ++ "&" ++ identEvalHess
  ++ ");"
  ]

freeBoundsTemplate :: [T.Text]
freeBoundsTemplate = map (T.pack . indent)
  [ "free(" ++ identLowerBoundsX ++ ");"
  , "free(" ++ identUpperBoundsX ++ ");"
  , "free(" ++ identLowerBoundsG ++ ");"
  , "free(" ++ identUpperBoundsG ++ ");"
  ]

-- TODO un-hardcode addOptionsTemplate
addOptionsTemplate :: [T.Text]
addOptionsTemplate = map (T.pack . indent)
  ["AddIpoptNumOption(nlp, \"tol\", 1e-5);"
  ,"AddIpoptStrOption(nlp, \"mu_strategy\", \"adaptive\");"
  ,"AddIpoptNumOption(nlp, \"print_frequency_time\", 10);"
  ]

ipoptSolveTemplate :: [T.Text]
ipoptSolveTemplate = map (T.pack . indent)
  [identStatus ++ " = IpoptSolve("
  ++ identIpoptProblem ++ ","
  ++ identVarX ++ ","
  ++ "NULL,"
  ++ "&" ++ identObjective ++ ","
  ++ "NULL,"
  ++ identMultLowerX ++ ","
  ++ identMultUpperX ++ ","
  ++ "NULL);"
  ]

printSolutionTemplate :: [T.Text]
printSolutionTemplate = undefined -- TODO finish printSolutionTemplate

freeSolutionTemplate :: [T.Text]
freeSolutionTemplate = map (T.pack . indent)
  ["FreeIpoptProblem(" ++ identIpoptProblem ++ ");"
  ,"free(" ++ identVarX ++ ");"
  ,"free(" ++ identMultLowerX ++ ");"
  ,"free(" ++ identMultUpperX ++ ");"
  ]

--   /* set the number of variables and allocate space for the bounds */
--   n=4;
--   x_L = (Number*)malloc(sizeof(Number)*n);
--   x_U = (Number*)malloc(sizeof(Number)*n);
--   /* set the values for the variable bounds */
--   for (i=0; i<n; i++) {
--     x_L[i] = 1.0;
--     x_U[i] = 5.0;
--   }

--   /* set the number of constraints and allocate space for the bounds */
--   m=2;
--   g_L = (Number*)malloc(sizeof(Number)*m);
--   g_U = (Number*)malloc(sizeof(Number)*m);
--   /* set the values of the constraint bounds */
--   g_L[0] = 25; g_U[0] = 2e19;
--   g_L[1] = 40; g_U[1] = 40;

--   /* create the IpoptProblem */
--   nlp = CreateIpoptProblem(n, x_L, x_U, m, g_L, g_U, 8, 10, 0, 
-- 			   &eval_f, &eval_g, &eval_grad_f, 
-- 			   &eval_jac_g, &eval_h);
  
--   /* We can free the memory now - the values for the bounds have been
--      copied internally in CreateIpoptProblem */
--   free(x_L);
--   free(x_U);
--   free(g_L);
--   free(g_U);

--   /* set some options */
--   AddIpoptNumOption(nlp, "tol", 1e-9);
--   AddIpoptStrOption(nlp, "mu_strategy", "adaptive");

--   /* allocate space for the initial point and set the values */
--   x = (Number*)malloc(sizeof(Number)*n);
--   x[0] = 1.0;
--   x[1] = 5.0;
--   x[2] = 5.0;
--   x[3] = 1.0;

--   /* allocate space to store the bound multipliers at the solution */
--   mult_x_L = (Number*)malloc(sizeof(Number)*n);
--   mult_x_U = (Number*)malloc(sizeof(Number)*n);

--   /* solve the problem */
--   status = IpoptSolve(nlp, x, NULL, &obj, NULL, mult_x_L, mult_x_U, NULL);

--   if (status == Solve_Succeeded) {
--     printf("\n\nSolution of the primal variables, x\n");
--     for (i=0; i<n; i++)
--       printf("x[%d] = %e\n", i, x[i]); 

--     printf("\n\nSolution of the bound multipliers, z_L and z_U\n");
--     for (i=0; i<n; i++)
--       printf("z_L[%d] = %e\n", i, mult_x_L[i]); 
--     for (i=0; i<n; i++)
--       printf("z_U[%d] = %e\n", i, mult_x_U[i]); 

--     printf("\n\nObjective value\nf(x*) = %e\n", obj); 
--   }
 
--   /* free allocated memory */
--   FreeIpoptProblem(nlp);
--   free(x);
--   free(mult_x_L);
--   free(mult_x_U);

--   return 0;
-- }]
