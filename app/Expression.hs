{- |
Module      : Expression

This module contains is in charge of parsing tokens into expressions.
-}
module Expression (
    Result,
    BinaryOperator (..),
    precedence,
    Function (..),
    numOfArgs,
    ExpressionToken (..),
    Expression (..),
    expressionToDataTree,
    shuntingYard,
) where

import qualified Data.List as List
import qualified Data.Tree as Tree
import Data.Tree.Pretty (drawVerticalTree)

type Result a = Either String a

data ExpressionToken
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
    = Sin
    | Cos
    | Tan
    | Pi
    | E
    | Max
    deriving (Show)

numOfArgs :: Function -> Int
numOfArgs Sin = 1
numOfArgs Cos = 1
numOfArgs Tan = 1
numOfArgs Pi = 0
numOfArgs E = 0
numOfArgs Max = 2

data Expression
    = Value Float
    | FunctionCall Function [Expression]
    | BinaryCalculation Expression BinaryOperator Expression

expressionToDataTree :: Expression -> Tree.Tree String
expressionToDataTree (Value n) =
    Tree.Node (show n) []
expressionToDataTree (FunctionCall f args) =
    Tree.Node (show f) (map expressionToDataTree args)
expressionToDataTree (BinaryCalculation l o r) =
    Tree.Node (show o) [expressionToDataTree l, expressionToDataTree r]

instance Show Expression where
    show ast =
        drawVerticalTree $ expressionToDataTree ast

-- {- | OperationStackItem is either an operator or a parenthesis or function,
-- used for shunting yard operator stack
-- -}
data OperationStackItem
    = OpenParOnStack
    | CloseParOnStack
    | OpOnStack BinaryOperator
    | FuOnStack Function
    deriving (Show)

-- | shuttingYard :: oppStack outputStack inputTokens -> AST
shuntingYard :: [OperationStackItem] -> [Expression] -> [ExpressionToken] -> Result Expression
shuntingYard [] [expr] [] = Right expr -- finished, done.
shuntingYard [] _outStack [] =
    -- this means somehow we finished,
    -- but the outStack doesn't have a single element (maybe 0 or maybe >2)
    Left "Failed to parse"
shuntingYard oppStack outStack (t : rest) =
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
            Left "Unmatched open parenthesis!"
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
                        Right $ FunctionCall f args : restOut

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
