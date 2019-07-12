module HashedNode where

import Data.IntMap.Strict as IM
import HashedExpression

-- | Helpers functions for Expression nodes
--
nodeElementType ::
  Node  -- ^ Input : Node (See Hashed Expression)
  -> ExpressionMap -- ^ Input : Expression Map
  -> ET -- ^ Output : Element Type of the Node
nodeElementType node mp =
    case node of
        Var _ -> R
        DVar _ -> Covector
        Const _ -> R
        Sum et _ -> et
        Mul et _ -> et
        Power _ arg -> retrieveElementType arg mp
        Neg et _ -> et
        Scale et _ _ -> et
        Div _ _ -> R
        Sqrt _ -> R
        Sin _ -> R
        Cos _ -> R
        Tan _ -> R
        Exp _ -> R
        Log _ -> R
        Sinh _ -> R
        Cosh _ -> R
        Tanh _ -> R
        Asin _ -> R
        Acos _ -> R
        Atan _ -> R
        Asinh _ -> R
        Acosh _ -> R
        Atanh _ -> R
        RealImag _ _ -> C -- from real and imagine
        RealPart _ -> R -- extract real part
        ImagPart _ -> R -- extract imaginary part
        InnerProd et _ _ -> et
        Piecewise _ _ branches -> retrieveElementType (head branches) mp
        Rotate _ arg -> retrieveElementType arg mp

nodeTypeWeight :: Node -> Int
nodeTypeWeight node =
    case node of
        Var {} -> 1
        DVar {} -> 800
        Const {} -> -9999
        Sum {} -> 9999
        Mul {} -> 3
        Power {} -> 28
        Neg {} -> 4
        Scale {} -> 5
        Div {} -> 6
        Sqrt {} -> 7
        Sin {} -> 8
        Cos {} -> 9
        Tan {} -> 10
        Exp {} -> 11
        Log {} -> 12
        Sinh {} -> 13
        Cosh {} -> 14
        Tanh {} -> 15
        Asin {} -> 16
        Acos {} -> 17
        Atan {} -> 18
        Asinh {} -> 19
        Acosh {} -> 20
        Atanh {} -> 21
        RealImag {} -> 22
        RealPart {} -> 23
        ImagPart {} -> 24
        InnerProd {} -> 25
        Piecewise {} -> 26
        Rotate {} -> 27

-- |
--
sameNodeType :: Node -> Node -> Bool
sameNodeType node1 node2 = nodeTypeWeight node1 == nodeTypeWeight node2

-- | Get list of arguments of this node
--
nodeArgs :: Node -> Args
nodeArgs node =
    case node of
        Var _ -> []
        DVar _ -> []
        Const _ -> []
        Sum _ args -> args
        Mul _ args -> args
        Power _ arg -> [arg]
        Neg _ arg -> [arg]
        Scale _ arg1 arg2 -> [arg1, arg2]
        Div arg1 arg2 -> [arg1, arg2]
        Sqrt arg -> [arg]
        Sin arg -> [arg]
        Cos arg -> [arg]
        Tan arg -> [arg]
        Exp arg -> [arg]
        Log arg -> [arg]
        Sinh arg -> [arg]
        Cosh arg -> [arg]
        Tanh arg -> [arg]
        Asin arg -> [arg]
        Acos arg -> [arg]
        Atan arg -> [arg]
        Asinh arg -> [arg]
        Acosh arg -> [arg]
        Atanh arg -> [arg]
        RealImag arg1 arg2 -> [arg1, arg2]
        RealPart arg -> [arg]
        ImagPart arg -> [arg]
        InnerProd _ arg1 arg2 -> [arg1, arg2]
        Piecewise _ conditionArg branches -> conditionArg : branches
        Rotate _ arg -> [arg]

-- | Auxiliary functions for operations
--
retrieveNode :: Int -> ExpressionMap -> Node
retrieveNode n mp =
    case IM.lookup n mp of
        Just (_, node) -> node
        _ -> error "node not in map"

retrieveInternal :: Int -> ExpressionMap -> Internal
retrieveInternal n mp =
    case IM.lookup n mp of
        Just internal -> internal
        _ -> error "node not in map"

retrieveElementType :: Int -> ExpressionMap -> ET
retrieveElementType n mp =
    case IM.lookup n mp of
        Just (_, node) -> nodeElementType node mp
        _ -> error "expression not in map"

retrieveShape :: Int -> ExpressionMap -> Shape
retrieveShape n mp =
    case IM.lookup n mp of
        Just (dim, _) -> dim
        _ -> error "expression not in map"

-- | The 'expressionElementType' gets and Expression as input. and return the element type of it
expressionElementType ::
  Expression d et -- ^ Input Expression which has d as dimension and et as element type
  -> ET           -- ^ Output element type
expressionElementType (Expression n mp) =
    -- Look up in our Expression map to find the Internal tuple of this expression
    case IM.lookup n mp of
        {-- If look up process has a result, it would be in the form of tuple. We ignore the frist part of tuple,
        since we just need the Node type. The function return the Node type. Which later on, it used as an input for
        'nodeElementType' and it returns our Element Type for us.
        -}
        Just (_, node) -> nodeElementType node mp
        {--
        If lookup does not have any result, then just show an error regarding that such an expression does not exist.
        in our expression map.
        -}
        _ -> error "expression not in map"

-- | Expression Shape Function, get at expression and return its Shape
-- Input : Expression with (the index of expression, expressionMap ) as Input
-- Output: Shape of the function (Read the comments on the Shape in HashedExpression File).
expressionShape :: Expression d et -> Shape
expressionShape (Expression n mp) =
    -- Basically, this function look for finding the index of the expression in the Expression Map (Look the Expression
    -- map (Look at the comments of Expression and Internal in the HashedExpression File). Then check if it can find
    case IM.lookup n mp of
        Just (shape, _) -> shape
        _ -> error "expression not in map"

expressionInternal :: Expression d et -> Internal
expressionInternal (Expression n mp) =
    case IM.lookup n mp of
        Just internal -> internal
        _ -> error "expression not in map"

expressionNode :: Expression d et -> Node
expressionNode (Expression n mp) =
    case IM.lookup n mp of
        Just (_, node) -> node
        _ -> error "expression not in map"
