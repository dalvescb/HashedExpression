{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HashedHash where

import qualified Data.IntMap.Strict as IM
import HashedExpression hiding ((*), (+))

-- | HasHash is a class which has something as input and has just operation
-- input : a which is something
class HasHash a where
    -- hash is the operation of HasHash which gets "a" as input and return an Integer Number
    hash :: a -> Int

-- | Helper hash functions, copy from HashedExpression
--
moveBase :: Char -> Int -> Int
moveBase c hash = hash * 40591 + fromEnum c

-- Function for generating Hashed codes using the list of indices of arguments in the ExpressionMap
-- Input : The list of indices of arguments in the ExpressionMap
-- Output : An integer Hashed number based on the algorithm  below

-- | Shape type:
-- []        --> Return 0
-- [n]       --> Returning the Head of the list which is an integer number
argHash :: [Int] -> Int
-- This is a recursive function which gets the head(arg) and tail(args) of the list separately,
-- And then add up 31 to the head (which is an integer number) and multiply the output with the final result of
-- recursively calling the function again.
argHash (arg:args) = arg + 31 * argHash args
-- Very last step of our recursively defined argHash function which return ZERO if the list if empty.
argHash [] = 0

rehash :: Int -> [Int]
rehash x = x : [x + (241 + x * 251) * i | i <- [1 ..]]

-- | HasHash instances

-- | HasHash Internal
-- This instance works when the input of the HasHaash class is in the form of  Internal, which is a type defined in
-- Hashed Expression
instance HasHash Internal where
    -- Input : Shape of the Expression and the hashed Node Index
    -- Output : Ineger number which is the result of mathematical operation below
    hash (shape, node) = hash node * (1 + argHash shape)

-- | HasHash ET
-- This instance works when the input of the HasHaash class is in the form of  ET, which is a type defined in
-- Hashed Expression
instance HasHash ET where
    -- If the expression type is Real then return 423
    hash R = 423
    -- If the expression type is Complex then return 451
    hash C = 451
    -- If the expression type is Covector then return 269
    hash Covector = 269

-- | HasHash Node
-- This instance works when the input of the HasHaash class is in the form of  Node, which is a type defined in
-- Hashed Expression
instance HasHash Node where
    hash node =
        case node of
            Var name -> foldr moveBase 0 name
            DVar name -> foldr moveBase 1123 name
            Const num -> 919393 + foldr moveBase 0 (show num)
            -- MARK: Basics
            Sum et args -> (1 + argHash (hash et : args)) * 2131
            Mul et args -> (1 + argHash (hash et : args)) * 3343
            Power x arg -> (1 + argHash [arg, x]) * 2527
            Neg et arg -> (1 + argHash [hash et, arg]) * 2293
            Scale et arg1 arg2 -> (1 + argHash [hash et, arg1, arg2]) * 3343
            -- MARK: only apply to R
            Div arg1 arg2 -> (1 + argHash [arg1, arg2]) * 2621
            Sqrt arg -> (1 + argHash [arg]) * 3083
            Sin arg -> (1 + argHash [arg]) * 1009
            Cos arg -> (1 + argHash [arg]) * 1013
            Tan arg -> (1 + argHash [arg]) * 1019
            Exp arg -> (1 + argHash [arg]) * 1031
            Log arg -> (1 + argHash [arg]) * 1033
            Sinh arg -> (1 + argHash [arg]) * 1039
            Cosh arg -> (1 + argHash [arg]) * 1049
            Tanh arg -> (1 + argHash [arg]) * 1051
            Asin arg -> (1 + argHash [arg]) * 1061
            Acos arg -> (1 + argHash [arg]) * 1063
            Atan arg -> (1 + argHash [arg]) * 1069
            Asinh arg -> (1 + argHash [arg]) * 1087
            Acosh arg -> (1 + argHash [arg]) * 1091
            Atanh arg -> (1 + argHash [arg]) * 1093
            -- MARK: Complex related
            RealPart arg -> (1 + argHash [arg]) * 223
            ImagPart arg -> (1 + argHash [arg]) * 227
            RealImag arg1 arg2 -> (1 + argHash [arg1, arg2]) * 229
            InnerProd et arg1 arg2 -> (1 + argHash [hash et, arg1, arg2]) * 3187
            -- MARK: Piecewise
            Piecewise marks arg branches ->
                (1 + argHash (foldr moveBase 0 (show marks) : arg : branches)) *
                269
            -- MARK: If the node is Rotate then get amount of the rotation, as well as arg should be rotated then
            -- generate the hash code based on the mathematical operation below
            -- Note : Read the comment related to argHash function above this page
            Rotate amount arg -> (1 + argHash (arg : amount)) * 593

-- |
--
data HashOutcome
    = IsClash
    | IsDuplicate Int
    | IsNew Int
    deriving (Eq, Show, Ord)

hashOutcome :: ExpressionMap -> Internal -> Int -> HashOutcome
hashOutcome mp new newHash =
    case IM.lookup newHash mp of
        Nothing -> IsNew newHash
        Just old ->
            if old == new
                then IsDuplicate newHash
                else IsClash

-- |
--
addEntry :: ExpressionMap -> Internal -> (ExpressionMap, Int)
addEntry mp e =
    case dropWhile (== IsClash) . map (hashOutcome mp e) . rehash . hash $ e of
        (IsDuplicate h:_) -> (mp, h)
        (IsNew h:_) -> (IM.insert h e mp, h)
        _ -> error "addEntry everything clashed!"

fromNode :: Internal -> (ExpressionMap, Int)
fromNode e = (mp, h)
  where
    h = hash e
    mp = IM.insert h e IM.empty
