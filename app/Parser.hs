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
astToDataTree (Value n) = Tree.Node (show n) []
astToDataTree Func{function = f, arguments = args} =
    Tree.Node (show f) (map astToDataTree args)
astToDataTree BinaryOp{left = l, binaryO = o, right = r} =
    Tree.Node (show o) [astToDataTree l, astToDataTree r]

instance Show AST where
    show ast =
        drawVerticalTree $ astToDataTree ast

{- | OpOrPar is either an operator or a parenthesis,
used for shunting yard operator stack
-}
data OpOrParOrFunc = Op BinaryOperator | Par Parenthesis | Fu Function

-- | keep creating nodes while precedence of the opStack is higher
dealWithOperator :: BinaryOperator -> [OpOrParOrFunc] -> [AST] -> Result ([OpOrParOrFunc], [AST])
dealWithOperator oppProcessing [] outStack = Right ([Op oppProcessing], outStack)
dealWithOperator oppProcessing oppStack outStack =
    case oppStack of
        (Op o : restOpp) ->
            if precedence o >= precedence oppProcessing
                then -- use the operator on top of that stack and loop again (threw the operator stack)
                    case outStack of
                        (r : l : restOut) ->
                            dealWithOperator
                                oppProcessing
                                restOpp
                                (BinaryOp{left = l, binaryO = o, right = r} : restOut)
                        _ -> Left "Not enough operands! (Dealing with operators)"
                else -- if precedence o < precedence oppProcessing
                -- add oppProcessing to oppStack
                    Right (Op oppProcessing : oppStack, outStack)
        (Par _ : _) -> Right (Op oppProcessing : oppStack, outStack)
        (Fu f : restOpp) ->
            let nOArgs = numOfArgs f
             in case List.splitAt nOArgs outStack of
                    (args, restOut) ->
                        dealWithOperator oppProcessing restOpp (Func{function = f, arguments = args} : restOut)

-- Right (restOpp, Func{function = f, arguments = args} : restOut)

-- | go through the operator stack and create nodes until hit an OpenParenthesis
dealWithParenthesis :: [OpOrParOrFunc] -> [AST] -> Result ([OpOrParOrFunc], [AST])
dealWithParenthesis (Par OpenP : rest) outStack = Right (rest, outStack)
dealWithParenthesis [] _outStack = Left "Unmatched closing operator!"
dealWithParenthesis (Par CloseP : _) _outStack =
    -- should never happen, as we should have dealt with this in `shuntingYard`
    Left "Dealing with parenthesis, by building ast but hit parenthesis!"
dealWithParenthesis (Fu f : restOpp) outStack =
    let nOArgs = numOfArgs f
     in if length outStack < nOArgs
            then Left $ "Not enough arguments for function " ++ show f
            else case List.splitAt nOArgs outStack of
                (args, restOut) ->
                    dealWithParenthesis restOpp (Func{function = f, arguments = args} : restOut)
dealWithParenthesis (Op o : restOpp) outStack =
    case outStack of
        (r : l : restOut) ->
            dealWithParenthesis
                restOpp
                (BinaryOp{left = l, binaryO = o, right = r} : restOut)
        _ -> Left "Not enough operands! (Dealing with parenthesis)"

dealWithFunction :: Function -> [OpOrParOrFunc] -> [AST] -> Result ([OpOrParOrFunc], [AST])
dealWithFunction funcProcessing [] outStack = Right ([Fu funcProcessing], outStack)
dealWithFunction funcProcessing oppStack outStack =
    case oppStack of
        (Op _ : _) ->
            -- function has higher "precedence" than all operator, so just add it to the stack
            Right (Fu funcProcessing : oppStack, outStack)
        (Par _ : _) -> Right (Fu funcProcessing : oppStack, outStack)
        (Fu f : restOpp) ->
            -- if a function is all ready on the stack, deal with it first!
            let nOArgs = numOfArgs f
             in case List.splitAt nOArgs outStack of
                    (args, restOut) ->
                        dealWithFunction funcProcessing restOpp (Func{function = f, arguments = args} : restOut)

-- | shuttingYard :: oppStack outputStack inputTokens -> AST
shuntingYard :: [OpOrParOrFunc] -> [AST] -> [Token] -> Result AST
shuntingYard [] [ast] [] = Right ast -- finished, done.
shuntingYard oppStack outStack (t : rest) =
    -- for each token
    case t of
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
                Left err -> Left err
        -- if it's an operator then `dealWithOperator` (check precedence and if needed, create nodes
        BinOpToken o ->
            case dealWithOperator o oppStack outStack of
                Right (newOppStack, newOutStack) ->
                    shuntingYard newOppStack newOutStack rest
                Left err -> Left err
        FuncToken f ->
            case dealWithFunction f oppStack outStack of
                Right (newOppStack, newOutStack) ->
                    shuntingYard newOppStack newOutStack rest
                Left err -> Left err
-- shuntingYard (Fu f : oppStack) outStack rest
shuntingYard (Fu f : oppRest) operandStack [] =
    let nOArgs = numOfArgs f
     in if length operandStack < nOArgs
            then Left $ "Not enough arguments for function " ++ show f
            else case List.splitAt nOArgs operandStack of
                (args, restOut) ->
                    shuntingYard oppRest (Func{function = f, arguments = args} : restOut) []
shuntingYard (Op o : oppRest) (r : l : outRest) [] =
    shuntingYard oppRest (BinaryOp{left = l, binaryO = o, right = r} : outRest) []
shuntingYard _ _ _ = Left "Failed to parse"

parse :: String -> AST
parse s =
    tokenize "" [] s
        & either error (shuntingYard [] [] . debugLog)
        & either error id

solve :: AST -> Float
solve (Value n) = n
solve Func{function = f, arguments = args} =
    case f of
        Sin ->
            sin $ solve (head args)
        Max ->
            let [a, b] = map solve args
             in max a b
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
