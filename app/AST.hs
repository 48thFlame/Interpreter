module AST (
    Operator (..),
    AST (..),
    solve,
    parse,
) where

import Data.Function ((&))
import qualified Data.Tree as Tree
import Data.Tree.Pretty
import Debug.Trace

debugLog :: (Show b) => b -> b
debugLog a =
    traceShow a a

data Operator = Addition | Subtraction

instance Show Operator where
    show Addition = "+"
    show Subtraction = "-"

-- Precedence and associativity functions (example)
precedence :: Operator -> Int
precedence Addition = 1
precedence Subtraction = 1

data AST = AST {left :: AST, operator :: Operator, right :: AST} | Value Float

data Token = OpToken Operator | NumToken Float deriving (Show)

astToDataTree :: AST -> Tree.Tree String
astToDataTree (Value n) = Tree.Node (show n) []
astToDataTree AST{left = l, operator = o, right = r} =
    Tree.Node (show o) [astToDataTree l, astToDataTree r]

instance Show AST where
    show ast =
        drawVerticalTree $ astToDataTree ast

stringToToken :: String -> Token
stringToToken s =
    case s of
        "+" -> OpToken Addition
        "-" -> OpToken Subtraction
        _ ->
            NumToken (read s)

dealWithOperator :: Operator -> [Operator] -> [AST] -> ([Operator], [AST])
dealWithOperator oppProcessing [] outStack = ([oppProcessing], outStack)
dealWithOperator oppProcessing oppStack outStack =
    let (o : restOpp) = oppStack
     in if precedence o >= precedence oppProcessing
            then -- use the operator on top of that stack and loop again (threw the operator stack)
                case outStack of
                    (r : l : restOut) ->
                        dealWithOperator
                            oppProcessing
                            restOpp
                            (AST{left = l, operator = o, right = r} : restOut)
                    _ -> error "Not enough operands!"
            else -- if precedence o < precedence oppProcessing
            -- add oppProcessing to oppStack
                (oppProcessing : oppStack, outStack)

-- | oppStack outputStack input -> AST
shuttingYard :: [Operator] -> [AST] -> [Token] -> AST
shuttingYard [] [ast] [] = ast -- finished, done.
shuttingYard oppStack outStack (t : rest) =
    -- for each token
    case t of
        -- if its number add to `outStack`
        NumToken n ->
            shuttingYard oppStack (Value n : outStack) rest
        -- if it's an operator then
        OpToken o ->
            let (newOppStack, newOutStack) = dealWithOperator o oppStack outStack
             in shuttingYard newOppStack newOutStack rest
shuttingYard (o : oppRest) (r : l : outRest) [] =
    shuttingYard oppRest (AST{left = l, operator = o, right = r} : outRest) []
shuttingYard _ _ _ = error "Invalid input"

parse :: String -> AST
parse s =
    words s
        & debugLog
        & map stringToToken
        & debugLog
        & shuttingYard [] []

solve :: AST -> Float
solve (Value n) = n
solve AST{left = l, operator = o, right = r} =
    case o of
        Addition ->
            solve l + solve r
        Subtraction ->
            solve l - solve r
