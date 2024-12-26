module Parser (AST (..), parse, solve) where

import Data.Function ((&))
import qualified Data.Tree as Tree
import Data.Tree.Pretty (drawVerticalTree)
import Debug.Trace
import Tokenizer

debugLog :: (Show b) => b -> b
debugLog a =
    traceShow a a

data AST = AST {left :: AST, operator :: Operator, right :: AST} | Value Float

astToDataTree :: AST -> Tree.Tree String
astToDataTree (Value n) = Tree.Node (show n) []
astToDataTree AST{left = l, operator = o, right = r} =
    Tree.Node (show o) [astToDataTree l, astToDataTree r]

instance Show AST where
    show ast =
        drawVerticalTree $ astToDataTree ast

{- | OpOrPar is either an operator or a parenthesis,
used for shunting yard operator stack
-}
data OpOrPar = Op Operator | Par Parenthesis

dealWithOperator :: Operator -> [OpOrPar] -> [AST] -> Result ([OpOrPar], [AST])
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
                                (AST{left = l, operator = o, right = r} : restOut)
                        _ -> Left "Not enough operands! (Dealing with operators)"
                else -- if precedence o < precedence oppProcessing
                -- add oppProcessing to oppStack
                    Right (Op oppProcessing : oppStack, outStack)
        (Par _ : _) -> Right (Op oppProcessing : oppStack, outStack)

dealWithParenthesis :: [OpOrPar] -> [AST] -> Result ([OpOrPar], [AST])
dealWithParenthesis (Par OpenP : rest) outStack = Right (rest, outStack)
dealWithParenthesis [] _outStack = Left "Unmatched closing operator!"
dealWithParenthesis (Par CloseP : _) _outStack = Left "Dealing with parenthesis, by building ast but hit parenthesis!"
dealWithParenthesis (Op o : restOpp) outStack =
    case outStack of
        (r : l : restOut) ->
            dealWithParenthesis
                restOpp
                (AST{left = l, operator = o, right = r} : restOut)
        _ -> Left "Not enough operands! (Dealing with parenthesis)"

-- | shuttingYard :: oppStack outputStack inputTokens -> AST
shuntingYard :: [OpOrPar] -> [AST] -> [Token] -> Result AST
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
        OpToken o ->
            case dealWithOperator o oppStack outStack of
                Right (newOppStack, newOutStack) ->
                    shuntingYard newOppStack newOutStack rest
                Left err -> Left err
-- if no tokens left, cleanup.
shuntingYard (Op o : oppRest) (r : l : outRest) [] =
    shuntingYard oppRest (AST{left = l, operator = o, right = r} : outRest) []
shuntingYard _ _ _ = Left "Failed to parse"

parse :: String -> AST
parse s =
    tokenize "" [] s
        & either error (shuntingYard [] [] . debugLog)
        & either error id

solve :: AST -> Float
solve (Value n) = n
solve AST{left = l, operator = o, right = r} =
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
