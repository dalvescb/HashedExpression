-------------------------------------------------------------------------------
-- | Inner Hashed functions, there are functions as
-- how to combine expression with options
--
--
-------------------------------------------------------------------------------
module HashedInner where

import qualified Data.IntMap.Strict as IM
import Data.List (sort, sortBy, sortOn)
import HashedExpression
import HashedHash
import HashedNode
import HashedUtils

-- |
--
data ElementOutcome
    = ElementSpecific ET
    | ElementDefault -- Highest element (R < C < Covector)

-- | Node Outcome (Represent the different kinds of outcomes)
data NodeOutcome
    = OpOne (Arg -> Node)
    | OpOneElement (ET -> Arg -> Node) ElementOutcome
    | OpTwo (Arg -> Arg -> Node)
    | OpTwoElement (ET -> Arg -> Arg -> Node) ElementOutcome -- ?
    | OpMany (Args -> Node)
    | OpManyElement (ET -> Args -> Node) ElementOutcome

-- | ShapeOutcome (Represent different kinds of shapes that a Node can have)
data ShapeOutcome
    = ShapeSpecific Shape
    | ShapeDefault -- Highest shape (shape with longest length)
    | ShapeBranches -- Same as branches' shape

-- | Different kinds of Operation Options
data OperationOption
    =
    -- Normal Operation Options (Which is a kind of data which has NodeOutcome and ShapeOutcome)
    Normal NodeOutcome ShapeOutcome
    | Condition (ConditionArg -> [BranchArg] -> Node)

-- |
--
unwrap :: Expression d et -> (ExpressionMap, Int)
unwrap (Expression n mp) = (mp, n)

-- ?
wrap :: (ExpressionMap, Int) -> Expression d et
wrap = uncurry $ flip Expression

-- uncurry ($) (show, 1)
-- |
--
highestShape :: [(ExpressionMap, Int)] -> Shape
highestShape = last . sortOn length . map (uncurry $ flip retrieveShape)

-- | R < C < Covector
--
highestElementType :: [(ExpressionMap, Int)] -> ET
highestElementType = maximum . map (uncurry $ flip retrieveElementType)

-- | The apply function that is used everywhere
-- Input : Operation Option, A list of tuples which is a collection of Expression Map (Hash map of all subexpressions)
-- and and
apply :: OperationOption -> [(ExpressionMap, Int)] -> (ExpressionMap, Int)
-- | In case we have Normal Operation Option, and (exps) as our list expression
apply (Normal nodeOutcome shapeOutcome) exps =
    let mergedMap =
            -- Check if the Expression List is Empty
            if null exps
                -- Return Error
                then error "List empty here????"
                -- If it is not empty, then run foldl1 on the union of frist parts of expressions in the list
                else foldl1 IM.union . map fst $ exps
        shape =
            case shapeOutcome of
                ShapeSpecific s -> s
                _ -> highestShape exps
        elementType elementOutcome =
            case elementOutcome of
                ElementSpecific et -> et
                _ -> highestElementType exps
        node =
            case (nodeOutcome, map snd exps) of
                (OpOne op, [arg]) -> op arg
                (OpOneElement op elm, [arg]) -> op (elementType elm) arg
                (OpTwo op, [arg1, arg2]) -> op arg1 arg2
                (OpTwoElement op elm, [arg1, arg2]) ->
                    op (elementType elm) arg1 arg2
                (OpMany op, args) -> op args
                (OpManyElement op elm, args) -> op (elementType elm) args
                _ -> error "HashedInner.apply"
     in addEntry mergedMap (shape, node)
apply (Condition op) exps@(conditionArg:branchArgs) =
    let mergedMap = foldl1 IM.union . map fst $ exps
        (headBranchMp, headBranchN) = head branchArgs
        shape = retrieveShape headBranchN headBranchMp
        node = op (snd conditionArg) $ map snd branchArgs
     in addEntry mergedMap (shape, node)

-- | General multiplication and sum
--
mulMany :: [(ExpressionMap, Int)] -> (ExpressionMap, Int)
mulMany = apply $ naryET Mul ElementDefault

sumMany :: [(ExpressionMap, Int)] -> (ExpressionMap, Int)
sumMany = apply $ naryET Sum ElementDefault

-- | Hasshape gets a Operation Option and shape and return an Operation Option as output
-- Inputs: Operation Option , Shape of the Operation
-- Output: Operation Option
hasShape :: OperationOption -> Shape -> OperationOption
-- This definition just care about the first part of the Operation Option which is the nodeOutcome (See NodeOutcome
-- comments) and then take the specific shape and merge this to data together.
hasShape (Normal nodeOutcome _) specificShape =
    Normal nodeOutcome (ShapeSpecific specificShape)
-- This definition return the option itself, without caring about the kind of shape
hasShape option _ = option

-- | applyBinary function get an OperationOption and two expression and merge them together to generate a new
-- expression
applyBinary ::
       OperationOption
    -> Expression d1 et1
    -> Expression d2 et2
    -> Expression d3 et3
-- Inputs : option=OperationOption, e1= Expression One, e2= Expression Two
-- What it do is : Uwrap the e1 and e2 and apply option on them and then wrap everything together and create a new
-- Expression
applyBinary option e1 e2 = wrap . apply option $ [unwrap e1, unwrap e2]

-- | Apply Unary is a function that unwrap over expression, run operation on it and then wrap it again.
-- This same as the applyBinary with difference that this just unwarping an expression rather than two, and wrap it
-- with an operation
applyUnary ::
       OperationOption
       -> Expression d1 et1
       -> Expression d2 et2
-- Inputs : option=OperationOption, e1= Expression One
-- What it do is : Unwrap the e1 and apply option on it and then wrap everything together and create a new
-- expression
applyUnary option e1 = wrap . apply option $ [unwrap e1]



-- | applyNary is a function that unwrap over expression, run operation on it and then wrap it again.
-- This same as the applyBinary with difference that first it is working on a list of expressions rather that an individual
-- one. and second, this function just used the option='OperationOption' part and try
-- to prepare something predefined, so later on this 'ready-to-use' function can have a list of expressions and generate
-- a new expression as output
applyNary :: OperationOption -> [Expression d1 et1] -> Expression d2 et2
-- Input : option as "OperationOption"
-- Output : "Ready-to-use" function which needs just a list of expressions
applyNary option = wrap . apply option . map unwrap

applyConditionAry ::
       OperationOption
    -> Expression d et1
    -> [Expression d et2]
    -> Expression d et2
applyConditionAry option e branches =
    wrap . apply option $ unwrap e : map unwrap branches

-- | binary operations
--
binary :: (Arg -> Arg -> Node) -> OperationOption
binary op = Normal (OpTwo op) ShapeDefault

-- | The 'binaryET' function, generates an OperationOption based on its input
binaryET ::
  (ET -> Arg -> Arg -> Node) -- ^ Input : (Element type -> Argument -> Argument-> Node) structure as the first input
  -> ElementOutcome -- ^ Input : ElementOutcome as the second input
  -> OperationOption -- ^ Output : OperationOption as the output
{-- op is (ET -> Arg -> Arg -> Node) which is a Node. elm is the ElementOutcome. Since the first op is in the form of
 (ET -> Arg -> Arg -> Node), then in should have the OpTwoElement of Node Outcome. This function just create an
 operationOption which is Normal, has OpTwoElement as Node Outcome and defaultShape as ShapeOutcome, and finally presents
 it as output operationOption
 -}
binaryET op elm = Normal (OpTwoElement op elm) ShapeDefault

-- | unary operations
-- Generate a operation option based on the Node we defined and used
unary :: (Arg -> Node) -> OperationOption
unary op = Normal (OpOne op) ShapeDefault


-- | n-ary operations
--
nary :: (Args -> Node) -> OperationOption
nary op = Normal (OpMany op) ShapeDefault

-- | unaryET gets a combination of (Element Type -> Arguments -> Node) which is basically a node, and ElementOutcome
-- look at the ElementOutcome data, and return OperationOption has output
unaryET :: (ET -> Arg -> Node) -> ElementOutcome -> OperationOption
-- This definition implies that unaryET get the operation and element and merge it whit ShapeDefualt (have a look at
-- ShapeDefault comments) and merge them in order to make a Operation Option
unaryET op elm = Normal (OpOneElement op elm) ShapeDefault


-- | naryET gets a combination of (Element Type -> Arguments -> Node) which is basically a node, and ElementOutcome
-- look at the ElementOutcome data, and return OperationOption has output
naryET :: (ET -> Args -> Node) -> ElementOutcome -> OperationOption
-- This definition implies that naryET get the operation and element and merge it whit ShapeDefualt (have a look at
-- ShapeDefault comments) and merge them in order to make a Operation Option. The only difference between this function
-- and the unaryET is that, this function use OpManyElement for ElemntOutCome
naryET op elm = Normal (OpManyElement op elm) ShapeDefault

-- | branch operation
--
conditionAry :: (ConditionArg -> [BranchArg] -> Node) -> OperationOption
conditionAry = Condition
