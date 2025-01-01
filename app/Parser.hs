module Parser (AST (..), parse, solve) where

import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Tree as Tree
import Data.Tree.Pretty (drawVerticalTree)
import Debug.Trace
import Tokenizer

debugLog :: (Show b) => b -> b
debugLog a =
    traceShow a a

data AST
    = Value Float
    | BinaryOp {left :: AST, binaryO :: BinaryOperator, right :: AST}
    | Func {function :: Function, arguments :: [AST]}

astToDataTree :: AST -> Tree.Tree String
astToDataTree (Value n) =
    Tree.Node (show n) []
astToDataTree Func{function = f, arguments = args} =
    Tree.Node (show f) (map astToDataTree args)
astToDataTree BinaryOp{left = l, binaryO = o, right = r} =
    Tree.Node (show o) [astToDataTree l, astToDataTree r]

instance Show AST where
    show ast =
        drawVerticalTree $ astToDataTree ast

{- | OpOrPar is either an operator or a parenthesis or function,
used for shunting yard operator stack
-}
data OpOrParOrFunc = Op BinaryOperator | Par Parenthesis | Fu Function

-- | create an operator node and add it to the output stack
createOperatorAST :: BinaryOperator -> [AST] -> Result [AST]
createOperatorAST o outStack =
    case outStack of
        (r : l : restOut) ->
            Right $ BinaryOp{left = l, binaryO = o, right = r} : restOut
        _ -> Left $ "Not enough operands for operator " ++ show o

-- | create a function node and add it to the output stack
createFunctionAST :: Function -> [AST] -> Result [AST]
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
                        Right $
                            Func
                                { function = f
                                , -- for the same reason r is before l in `createOperatorAST` need to reverse
                                  arguments = List.reverse args
                                }
                                : restOut

-- | keep creating nodes while precedence of the opStack is higher
dealWithOperator :: BinaryOperator -> [OpOrParOrFunc] -> [AST] -> Result ([OpOrParOrFunc], [AST])
dealWithOperator oppProcessing [] outStack = Right ([Op oppProcessing], outStack)
dealWithOperator oppProcessing oppStack@(Op o : restOpp) outStack =
    if precedence o >= precedence oppProcessing
        then -- use the operator on top of that stack and loop again
            case createOperatorAST o outStack of
                Right newOutStack ->
                    dealWithOperator oppProcessing restOpp newOutStack
                Left err ->
                    Left err
        else -- if precedence o < precedence oppProcessing
        -- add oppProcessing to oppStack
            Right (Op oppProcessing : oppStack, outStack)
dealWithOperator oppProcessing (Fu f : restOpp) outStack =
    -- precedence of a function is higher than any operator, so deal with it
    case createFunctionAST f outStack of
        Right newOutStack ->
            dealWithOperator oppProcessing restOpp newOutStack
        Left err ->
            Left err
dealWithOperator oppProcessing oppStack@(Par _ : _) outStack =
    {-
    ? whats the meaning of this?
    I think means looking threw to check precedence and found a parenthesis -
        so add to stack-}
    Right (Op oppProcessing : oppStack, outStack)

-- | go through the operator stack and create nodes until hit an OpenParenthesis
dealWithParenthesis :: [OpOrParOrFunc] -> [AST] -> Result ([OpOrParOrFunc], [AST])
dealWithParenthesis (Par OpenP : rest) outStack =
    -- base-case: we hit an OpenParenthesis -> we are done
    Right (rest, outStack)
dealWithParenthesis [] _outStack =
    -- finished the stack, but no OpenParenthesis!
    Left "Unmatched closing operator!"
dealWithParenthesis (Par CloseP : _) _outStack =
    -- should never happen, as we should have dealt with this in `shuntingYard`
    Left "Unexpected closing parenthesis!"
dealWithParenthesis (Fu f : restOpp) outStack =
    case createFunctionAST f outStack of
        Right newOutStack ->
            dealWithParenthesis restOpp newOutStack
        Left err ->
            Left err
dealWithParenthesis (Op o : restOpp) outStack =
    case createOperatorAST o outStack of
        Right newOutStack ->
            dealWithParenthesis restOpp newOutStack
        Left err ->
            Left err

{- | "keep creating nodes while precedence of the opStack is higher",
for functions that means all future functions that *need* arguments
-}
dealWithFunction :: Function -> [OpOrParOrFunc] -> [AST] -> Result ([OpOrParOrFunc], [AST])
dealWithFunction funcProcessing [] outStack = Right ([Fu funcProcessing], outStack)
dealWithFunction funcProcessing oppStack@(Op _ : _) outStack =
    -- function has higher "precedence" than all operator, so just add it to the stack
    Right (Fu funcProcessing : oppStack, outStack)
dealWithFunction funcProcessing oppStack@(Par _ : _) outStack =
    Right (Fu funcProcessing : oppStack, outStack)
dealWithFunction funcProcessing oppStack@(Fu f : restOpp) outStack =
    if numOfArgs funcProcessing == 0
        -- if the function does not need arguments, then it should be considered "done"
        -- so this for example works without parenthesis:
        -- `sin pi` instead of `sin(pi)`
        then
            Right (Fu funcProcessing : oppStack, outStack)
        else case createFunctionAST f outStack of
            Right newOutStack ->
                dealWithFunction funcProcessing restOpp newOutStack
            Left err ->
                Left err

-- | shuttingYard :: oppStack outputStack inputTokens -> AST
shuntingYard :: [OpOrParOrFunc] -> [AST] -> [Token] -> Result [AST]
shuntingYard [] outStack [] = Right outStack -- finished, done.
-- shuntingYard [] _outStack [] =
--     -- this means somehow we finished,
--     -- but the outStack doesn't have a single element (maybe 0 or maybe >2)
--     Left "Failed to parse"
shuntingYard oppStack outStack (t : rest) =
    -- for each token
    case t of
        SemicolonToken ->
            -- if we hit a semicolon, then we are done with this expression
            -- so we need to reset the oppStack - and build the expression,
            case shuntingYard oppStack outStack [] of
                Right newOutStack ->
                    -- and then start a new expression
                    shuntingYard [] newOutStack rest
                Left err ->
                    Left err
        -- shuntingYard [] (outStack ++ ()) rest
        -- shuntingYard oppStack outStack rest
        -- if its number add to `outStack`
        NumToken n ->
            shuntingYard oppStack (Value n : outStack) rest
        -- if it's an OpenParenthesis, just add it to the oppStack
        ParToken OpenP ->
            shuntingYard (Par OpenP : oppStack) outStack rest
        -- if it's a CloseParenthesis, then `dealWithParenthesis` (keep creating nodes until OpenParenthesis)
        ParToken CloseP ->
            case dealWithParenthesis oppStack outStack of
                Right (newOppStack, newOutStack) ->
                    shuntingYard newOppStack newOutStack rest
                Left err ->
                    Left err
        -- if it's an operator then `dealWithOperator` (check precedence and if needed, create nodes
        BinOpToken o ->
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
        Op o ->
            case createOperatorAST o outStack of
                Right newOutStack ->
                    shuntingYard oppRest newOutStack []
                Left err ->
                    Left err
        Fu f ->
            case createFunctionAST f outStack of
                Right newOutStack ->
                    shuntingYard oppRest newOutStack []
                Left err ->
                    Left err
        Par OpenP ->
            Left "Unmatched open parenthesis!"
        Par CloseP ->
            Left "Unexpected closing parenthesis!" -- should never get here

-- | parse a string and return the AST
parse :: String -> [AST]
parse s =
    tokenize "" [] s
        & either error (shuntingYard [] [] . debugLog)
        & either error (List.reverse)

-- | solve the AST and return the result
solve :: AST -> Float
solve (Value n) = n
solve Func{function = f, arguments = args} =
    case f of
        Sin ->
            sin $ solve (head args)
        Cos ->
            cos $ solve (head args)
        Tan ->
            tan $ solve (head args)
        Pi ->
            pi
        E ->
            exp 1
        Max ->
            let [a, b] = map solve args
             in max a b
        Min ->
            let [a, b] = map solve args
             in min a b
solve BinaryOp{left = l, binaryO = o, right = r} =
    case o of
        Addition ->
            solve l + solve r
        Subtraction ->
            solve l - solve r
        Multiplication ->
            solve l * solve r
        Division ->
            solve l / solve r
        Power ->
            solve l ** solve r
