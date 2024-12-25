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
import Text.Read (readMaybe)

debugLog :: (Show b) => b -> b
debugLog a =
    traceShow a a

type Result a = Either String a

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

stringedTokenToToken :: String -> Result Token
stringedTokenToToken s =
    case s of
        "+" -> Right $ OpToken Addition
        "-" -> Right $ OpToken Subtraction
        "*" -> Right $ OpToken Multiplication
        "/" -> Right $ OpToken Division
        "^" -> Right $ OpToken Power
        "(" -> Right $ ParToken OpenP
        ")" -> Right $ ParToken CloseP
        _ ->
            case readMaybe s of
                Just n -> Right $ NumToken n
                Nothing -> Left $ "Invalid token NAN: " ++ s

isNum :: Char -> Bool
isNum c = isDigit c || c == '.'

isSpace :: Char -> Bool
isSpace c = c == ' '

-- | parse a string and return the tokens
tokenize :: String -> [Token] -> [Char] -> Result [Token]
-- 3 case where (acc == ""):
-- done, char is space, start of new token - either number or just operator.
tokenize "" parsedTokens [] = Right parsedTokens
tokenize "" parsedTokens (char : rest)
    | isSpace char = tokenize "" parsedTokens rest
    | isNum char = tokenize [char] parsedTokens rest
    | otherwise = case stringedTokenToToken [char] of
        Right token -> tokenize "" (parsedTokens ++ [token]) rest
        Left err -> Left err
-- base case: done going threw the string
tokenize acc parsedTokens [] =
    case stringedTokenToToken acc of
        Right token -> Right $ parsedTokens ++ [token]
        Left err -> Left err
-- otherwise: loop
tokenize acc parsedTokens (char : rest)
    | isSpace char = case stringedTokenToToken acc of
        Right token -> tokenize "" (parsedTokens ++ [token]) rest
        Left err -> Left err
    -- if isDigit need to assume might be big number: so add to `acc`
    | isNum char = tokenize (acc ++ [char]) parsedTokens rest
    -- otherwise: must be operator tokenize immediately both `acc` and `char`
    | otherwise =
        case (stringedTokenToToken acc, stringedTokenToToken [char]) of
            (Right token1, Right token2) ->
                tokenize "" (parsedTokens ++ [token1, token2]) rest
            (Left err, _) -> Left err
            (_, Left err) -> Left err

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
