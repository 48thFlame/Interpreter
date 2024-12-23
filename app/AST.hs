module AST (
    Operator (..),
    AST (..),
    solve,
    parse,
    isNum,
) where

import Data.Char (isDigit)
import Data.Function ((&))
import qualified Data.Tree as Tree
import Data.Tree.Pretty (drawVerticalTree)
import Debug.Trace

debugLog :: (Show b) => b -> b
debugLog a =
    traceShow a a

data Operator = Addition | Subtraction | Multiplication | Division | Power

instance Show Operator where
    show Addition = "+"
    show Subtraction = "-"
    show Multiplication = "*"
    show Division = "/"
    show Power = "^"

precedence :: Operator -> Int
precedence Addition = 1
precedence Subtraction = 1
precedence Multiplication = 2
precedence Division = 2
precedence Power = 3

data AST = AST {left :: AST, operator :: Operator, right :: AST} | Value Float

data Token = OpToken Operator | NumToken Float deriving (Show)

astToDataTree :: AST -> Tree.Tree String
astToDataTree (Value n) = Tree.Node (show n) []
astToDataTree AST{left = l, operator = o, right = r} =
    Tree.Node (show o) [astToDataTree l, astToDataTree r]

instance Show AST where
    show ast =
        drawVerticalTree $ astToDataTree ast

stringedTokenToToken :: String -> Token
stringedTokenToToken s =
    case s of
        "+" -> OpToken Addition
        "-" -> OpToken Subtraction
        "*" -> OpToken Multiplication
        "/" -> OpToken Division
        "^" -> OpToken Power
        _ ->
            NumToken (read (debugLog s))

isNum :: Char -> Bool
isNum c = isDigit c || c == '.'

isSpace :: Char -> Bool
isSpace c = c == ' '

-- | parse a string and return the tokens
tokenize :: String -> [Token] -> [Char] -> [Token]
-- 3 case where (acc == ""): done, char is space, start of new token
tokenize "" parsedTokens [] = parsedTokens
tokenize "" parsedTokens (' ' : rest) = tokenize "" parsedTokens rest
tokenize "" parsedTokens (char : rest)
    | isNum char = tokenize [char] parsedTokens rest
    | otherwise = tokenize "" (parsedTokens ++ [stringedTokenToToken [char]]) rest
-- base case: done going threw the string
tokenize acc parsedTokens [] =
    parsedTokens ++ [stringedTokenToToken acc]
-- otherwise: loop
tokenize acc parsedTokens (char : rest)
    | isSpace char = tokenize "" (parsedTokens ++ [stringedTokenToToken acc]) rest
    -- if isDigit need to assume might be big number: so add to `acc`
    | isNum char = tokenize (acc ++ [char]) parsedTokens rest
    -- otherwise: must be operator tokenize immediately
    | otherwise =
        tokenize
            ""
            ( parsedTokens
                ++ [stringedTokenToToken acc, stringedTokenToToken [debugLog char]]
            )
            rest

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

-- | shuttingYard :: oppStack outputStack inputTokens -> AST
shuntingYard :: [Operator] -> [AST] -> [Token] -> AST
shuntingYard [] [ast] [] = ast -- finished, done.
shuntingYard oppStack outStack (t : rest) =
    -- for each token
    case t of
        -- if its number add to `outStack`
        NumToken n ->
            shuntingYard oppStack (Value n : outStack) rest
        -- if it's an operator then `dealWithOperator`
        OpToken o ->
            let (newOppStack, newOutStack) = dealWithOperator o oppStack outStack
             in shuntingYard newOppStack newOutStack rest
shuntingYard (o : oppRest) (r : l : outRest) [] =
    shuntingYard oppRest (AST{left = l, operator = o, right = r} : outRest) []
shuntingYard _ _ _ = error "Invalid input"

parse :: String -> AST
parse s =
    tokenize "" [] s
        & debugLog
        & shuntingYard [] []

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
