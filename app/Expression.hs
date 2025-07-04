{- |
This module is in charge basically of all the logic. Mainly the `shuntingYard` algorithm
and the `solveExpression` function.
-}
module Expression (
    Result,
    BinaryOperator (..),
    precedence,
    Function (..),
    numOfArgs,
    -- ExpressionToken (..),
    Token (..),
    Expression (..),
    shuntingYard,
    solveExpression,
) where

import qualified Data.List as List
import qualified Data.Tree as Tree
import Data.Tree.Pretty (drawVerticalTree)

type Result a = Either String a

data Token
    = NumToken Float
    | BinOpToken BinaryOperator
    | FuncToken Function
    | OpenParenthesisToken
    | CloseParenthesisToken
    deriving (Show)

data BinaryOperator
    = Plus
    | Minus
    | Multiply
    | Divide
    | Power
    deriving (Show)

precedence :: BinaryOperator -> Int
precedence Plus = 1
precedence Minus = 1
precedence Multiply = 2
precedence Divide = 2
precedence Power = 3

data Function
    = -- = UniMinus
      Sqrt
    | Abs
    | Max
    | Min
    | Pi
    | Sin
    | Cos
    | Tan
    | E
    | LogBase
    | Log
    | Ln
    deriving (Show)

numOfArgs :: Function -> Int
-- numOfArgs UniMinus = 1
numOfArgs Sqrt = 1
numOfArgs Abs = 1
numOfArgs Max = 2
numOfArgs Min = 2
numOfArgs Pi = 0
numOfArgs Sin = 1
numOfArgs Cos = 1
numOfArgs Tan = 1
numOfArgs E = 0
numOfArgs LogBase = 2
numOfArgs Log = 1
numOfArgs Ln = 1

data Expression
    = Value Float
    | FunctionCall Function [Expression]
    | BinaryCalculation Expression BinaryOperator Expression

astToDataTree :: Expression -> Tree.Tree String
astToDataTree (Value n) =
    Tree.Node (show n) []
astToDataTree (FunctionCall f args) =
    Tree.Node (show f) (map astToDataTree args)
astToDataTree (BinaryCalculation l o r) =
    Tree.Node (show o) [astToDataTree l, astToDataTree r]

instance Show Expression where
    show ast =
        drawVerticalTree $ astToDataTree ast

solveExpression :: Expression -> Float
solveExpression (Value n) = n
solveExpression (FunctionCall f args) =
    -- When creating the AST, we confirmed that the number of args is sufficient
    -- so we can safely use `head` and `!!`
    case f of
        -- UniMinus ->
        --     negate $ solveExpression $ head args
        Sqrt ->
            sqrt $ solveExpression $ head args
        Abs ->
            abs $ solveExpression $ head args
        Min ->
            min (solveExpression $ head args) (solveExpression $ args !! 1)
        Max ->
            max (solveExpression $ head args) (solveExpression $ args !! 1)
        Pi ->
            pi
        Sin ->
            sin $ solveExpression $ head args
        Cos ->
            cos $ solveExpression $ head args
        Tan ->
            tan $ solveExpression $ head args
        E ->
            exp 1
        LogBase ->
            logBase (solveExpression $ head args) (solveExpression $ args !! 1)
        Log ->
            logBase 10 $ solveExpression $ head args
        Ln ->
            log $ solveExpression $ head args
solveExpression (BinaryCalculation l o r) =
    let
        lSolved = solveExpression l
        rSolved = solveExpression r
     in
        case o of
            Plus ->
                lSolved + rSolved
            Minus ->
                lSolved - rSolved
            Multiply ->
                lSolved * rSolved
            Divide ->
                lSolved / rSolved
            Power ->
                lSolved ** rSolved

{- | OperationStackItem is either an operator or a parenthesis or function,
used for shunting yard operator stack
so any token that is not a number
-}
data OperationStackItem
    = OpenParOnStack
    | CloseParOnStack
    | OpOnStack BinaryOperator
    | FuOnStack Function
    deriving (Show)

-- | shuttingYard :: oppStack outputStack inputTokens -> AST
shuntingYard :: [OperationStackItem] -> [Expression] -> [Token] -> Result Expression
shuntingYard [] [expr] [] =
    Right expr -- finished, done.
shuntingYard [] _outStack [] =
    -- this means somehow we finished,
    -- but the outStack doesn't have a single element (maybe 0 or maybe >1)
    Left $ "Failed to parse!\nCurrent outStack: " ++ show _outStack
shuntingYard oppStack outStack (t : rest) =
    -- "regular case"
    -- for each token
    case t of
        NumToken n ->
            -- if its number add to `outStack`
            shuntingYard oppStack (Value n : outStack) rest
        OpenParenthesisToken ->
            -- if it's an OpenParenthesis, just add it to the oppStack
            shuntingYard (OpenParOnStack : oppStack) outStack rest
        CloseParenthesisToken ->
            -- if it's a CloseParenthesis, then `dealWithParenthesis` (keep creating nodes until OpenParenthesis)
            case dealWithParenthesis oppStack outStack of
                Right (newOppStack, newOutStack) ->
                    shuntingYard newOppStack newOutStack rest
                Left err ->
                    Left err
        BinOpToken o ->
            -- if it's an operator then `dealWithOperator` (check precedence and if needed, create nodes
            case dealWithOperator o oppStack outStack of
                Right (newOppStack, newOutStack) ->
                    shuntingYard newOppStack newOutStack rest
                Left err ->
                    Left err
        FuncToken f ->
            --     shuntingYard (Fu f : oppStack) outStack rest
            case dealWithFunction f oppStack outStack of
                Right (newOppStack, newOutStack) ->
                    shuntingYard newOppStack newOutStack rest
                Left err ->
                    Left err
-- done with all tokens -> clean up stacks
shuntingYard (opf : oppRest) outStack [] =
    case opf of
        OpenParOnStack ->
            Left "Missing close parenthesis for open parenthesis!"
        CloseParOnStack ->
            Left "Unexpected closing parenthesis!" -- should never get here
        OpOnStack o ->
            case createOperatorAST o outStack of
                Right newOutStack ->
                    shuntingYard oppRest newOutStack []
                Left err ->
                    Left err
        FuOnStack f ->
            case createFunctionAST f outStack of
                Right newOutStack ->
                    shuntingYard oppRest newOutStack []
                Left err ->
                    Left err

-- | create an operator node and add it to the output stack
createOperatorAST :: BinaryOperator -> [Expression] -> Result [Expression]
createOperatorAST o outStack =
    case outStack of
        (r : l : restOut) ->
            Right $ BinaryCalculation l o r : restOut
        _ -> Left $ "Not enough operands for operator " ++ show o

-- | create a function node and add it to the output stack
createFunctionAST :: Function -> [Expression] -> Result [Expression]
createFunctionAST f outStack =
    let nOArgs = numOfArgs f
     in case List.splitAt nOArgs outStack of
            (args, restOut) ->
                if length args /= nOArgs
                    then
                        Left $
                            "Not enough arguments for function `"
                                ++ show f
                                ++ "`, only "
                                ++ show (length args)
                                ++ " of "
                                ++ show nOArgs
                    else
                        -- we have enough args, so create the node
                        -- and add it to the output stack
                        -- because we wer're using a stack,
                        -- args are in reverse order, so we need to reverse them
                        Right $ FunctionCall f (List.reverse args) : restOut

-- | go through the operator stack and create nodes until hit an OpenParenthesis
dealWithParenthesis :: [OperationStackItem] -> [Expression] -> Result ([OperationStackItem], [Expression])
dealWithParenthesis (OpenParOnStack : rest) outStack =
    -- base-case: we hit an OpenParenthesis -> we are done
    Right (rest, outStack)
dealWithParenthesis [] _outStack =
    -- finished the stack, but no OpenParenthesis!
    Left "Unmatched closing operator!"
dealWithParenthesis (CloseParOnStack : _) _outStack =
    -- should never happen, as we should have dealt with this in `shuntingYard`
    Left "Unexpected closing parenthesis!"
dealWithParenthesis (FuOnStack f : restOpp) outStack =
    case createFunctionAST f outStack of
        Right newOutStack ->
            dealWithParenthesis restOpp newOutStack
        Left err ->
            Left err
dealWithParenthesis (OpOnStack o : restOpp) outStack =
    case createOperatorAST o outStack of
        Right newOutStack ->
            dealWithParenthesis restOpp newOutStack
        Left err ->
            Left err

-- | keep creating nodes while precedence of the opStack is higher
dealWithOperator :: BinaryOperator -> [OperationStackItem] -> [Expression] -> Result ([OperationStackItem], [Expression])
dealWithOperator oppProcessing [] outStack =
    Right ([OpOnStack oppProcessing], outStack)
dealWithOperator oppProcessing oppStack@(OpOnStack o : restOpp) outStack =
    if precedence o >= precedence oppProcessing
        then -- use the operator on top of that stack and loop again
            case createOperatorAST o outStack of
                Right newOutStack ->
                    dealWithOperator oppProcessing restOpp newOutStack
                Left err ->
                    Left err
        else -- if precedence o < precedence oppProcessing
        -- add oppProcessing to oppStack
            Right (OpOnStack oppProcessing : oppStack, outStack)
dealWithOperator oppProcessing (FuOnStack f : restOpp) outStack =
    -- precedence of a function is higher than any operator, so deal with it
    case createFunctionAST f outStack of
        Right newOutStack ->
            dealWithOperator oppProcessing restOpp newOutStack
        Left err ->
            Left err
dealWithOperator oppProcessing oppStack@(OpenParOnStack : _) outStack =
    {-
    ? whats the meaning of this?
    I think means looking threw to check precedence and found a parenthesis -
        so add to stack-}
    Right (OpOnStack oppProcessing : oppStack, outStack)
dealWithOperator oppProcessing oppStack@(CloseParOnStack : _) outStack =
    {-
    ? whats the meaning of this?
    I think means looking threw to check precedence and found a parenthesis -
        so add to stack-}
    Right (OpOnStack oppProcessing : oppStack, outStack)

{- | "keep creating nodes while precedence of the opStack is higher",
for functions that means all future functions that *need* arguments
-}
dealWithFunction :: Function -> [OperationStackItem] -> [Expression] -> Result ([OperationStackItem], [Expression])
dealWithFunction funcProcessing [] outStack =
    Right ([FuOnStack funcProcessing], outStack)
dealWithFunction funcProcessing oppStack@(OpOnStack _ : _) outStack =
    -- function has higher "precedence" than all operator, so just add it to the stack
    Right (FuOnStack funcProcessing : oppStack, outStack)
dealWithFunction funcProcessing oppStack@(OpenParOnStack : _) outStack =
    Right (FuOnStack funcProcessing : oppStack, outStack)
dealWithFunction funcProcessing oppStack@(CloseParOnStack : _) outStack =
    Right (FuOnStack funcProcessing : oppStack, outStack)
dealWithFunction funcProcessing oppStack@(FuOnStack f : restOpp) outStack =
    if numOfArgs funcProcessing == 0
        -- if the function does not need arguments, then it should be considered "done"
        -- so this for example works without parenthesis:
        -- `sin pi` instead of `sin(pi)`
        then
            Right (FuOnStack funcProcessing : oppStack, outStack)
        else case createFunctionAST f outStack of
            Right newOutStack ->
                dealWithFunction funcProcessing restOpp newOutStack
            Left err ->
                Left err
