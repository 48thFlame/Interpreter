module AST (
    Operator (..),
    AST (..),
    solve,
    parse,
) where

import Data.Char (isDigit)
import Data.Function ((&))
import qualified Data.Tree as Tree
import Data.Tree.Pretty (drawVerticalTree)
import Debug.Trace

debugLog :: (Show b) => b -> b
debugLog a =
    traceShow a a

data Operator
    = Addition
    | Subtraction
    | Multiplication
    | Division
    | Power
    deriving (Eq)

instance Show Operator where
    show Addition = "+"
    show Subtraction = "-"
    show Multiplication = "*"
    show Division = "/"
    show Power = "^"

data Parenthesis
    = OpenP
    | CloseP
    deriving (Show, Eq)

precedence :: Operator -> Int
precedence Addition = 1
precedence Subtraction = 1
precedence Multiplication = 2
precedence Division = 2
precedence Power = 3

data AST = AST {left :: AST, operator :: Operator, right :: AST} | Value Float

data Token
    = OpToken Operator
    | ParToken Parenthesis
    | NumToken Float
    deriving (Show, Eq)

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
        "(" -> ParToken OpenP
        ")" -> ParToken CloseP
        _ ->
            NumToken (read s)

isNum :: Char -> Bool
isNum c = isDigit c || c == '.'

isSpace :: Char -> Bool
isSpace c = c == ' '

-- | parse a string and return the tokens
tokenize :: String -> [Token] -> [Char] -> [Token]
-- 3 case where (acc == ""):
-- done, char is space, start of new token - either number or just operator.
tokenize "" parsedTokens [] = parsedTokens
tokenize "" parsedTokens (char : rest)
    | isSpace char = tokenize "" parsedTokens rest
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
                ++ [stringedTokenToToken acc, stringedTokenToToken [char]]
            )
            rest

type OpPArStack = [Either Parenthesis Operator]

dealWithOperator :: Operator -> OpPArStack -> [AST] -> (OpPArStack, [AST])
dealWithOperator oppProcessing [] outStack = ([Right oppProcessing], outStack)
dealWithOperator oppProcessing oppStack outStack =
    case oppStack of
        (Right o : restOpp) ->
            if precedence o >= precedence oppProcessing
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
                    (Right oppProcessing : oppStack, outStack)
        (Left _ : _) -> (Right oppProcessing : oppStack, outStack)

dealWithParenthesis :: OpPArStack -> [AST] -> (OpPArStack, [AST])
dealWithParenthesis (Left OpenP : rest) outStack = (rest, outStack)
dealWithParenthesis [] _outStack = error "Unmatched closing operator!"
dealWithParenthesis (Left CloseP : _) _outStack = error "Dealing with parenthesis, by building ast but hit parenthesis!"
dealWithParenthesis (Right o : restOpp) outStack =
    case outStack of
        (r : l : restOut) ->
            dealWithParenthesis
                restOpp
                (AST{left = l, operator = o, right = r} : restOut)
        _ -> error "Not enough operands!"

-- | shuttingYard :: oppStack outputStack inputTokens -> AST
shuntingYard :: OpPArStack -> [AST] -> [Token] -> AST
shuntingYard [] [ast] [] = ast -- finished, done.
shuntingYard oppStack outStack (t : rest) =
    -- for each token
    case t of
        -- if its number add to `outStack`
        NumToken n ->
            shuntingYard oppStack (Value n : outStack) rest
        -- if it's an OpenParenthesis, just add it to the oppStack
        ParToken OpenP ->
            shuntingYard (Left OpenP : oppStack) outStack rest
        -- if it's a CloseParenthesis, then `dealWithParenthesis` (keep creating nodes until OpenParenthesis)
        ParToken CloseP ->
            let (newOppStack, newOutStack) = dealWithParenthesis oppStack outStack
             in shuntingYard newOppStack newOutStack rest
        -- if it's an operator then `dealWithOperator` (check precedence and if needed, create nodes
        OpToken o ->
            let (newOppStack, newOutStack) = dealWithOperator o oppStack outStack
             in shuntingYard newOppStack newOutStack rest
-- if no tokens left, cleanup.
shuntingYard (Right o : oppRest) (r : l : outRest) [] =
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
