{- |
This module is in charge og taking a string and parsing it into tokens.
And to build an AST from those tokens.
-}
module Tokenizer (
    Result,
    Token (..),
    BinaryOperator (..),
    precedence,
    Function (..),
    numOfArgs,
    tokenize,
    buildAST,
) where

import Data.Char (isDigit)

import Text.Read (readMaybe)

import Expression (
    BinaryOperator (..),
    Expression,
    Function (..),
    Result,
    Token (..),
    numOfArgs,
    precedence,
    shuntingYard,
 )

import Data.Function ((&))

-- import Debug.Trace (traceShow)
-- debugLog :: (Show b) => b -> b
-- debugLog a =
--     traceShow a a

{- |
    buildAST takes a string, tokenizes it,
    and parses it into an AST using `shuntingYard`.
-}
buildAST :: String -> Expression
buildAST s =
    tokenize "" [] s
        & either error (shuntingYard [] [])
        & either error id

{- | parse a string and return the tokens
`tokenize acc tokens str`
-}
tokenize :: String -> [Token] -> [Char] -> Result [Token]
-- base cases: done going threw the string
tokenize "" tokens [] = Right tokens
tokenize acc tokens [] =
    case readToken acc of
        Right token -> Right $ tokens ++ [token]
        Left err ->
            Left err
-- cases where (acc == "")
tokenize "" tokens (char : rest)
    | isWhiteSpace char = tokenize "" tokens rest -- acc is empty so just continue
    | isNameChar char = tokenize [char] tokens rest -- acc
    | isOperatorChar char =
        -- in the future also add to acc (now all operators are 1 char)
        case readToken [char] of
            Right token -> tokenize "" (tokens ++ [token]) rest
            Left err ->
                Left err -- should not get here ever, its an operatorChar!
    | otherwise = Left $ "Invalid character: " ++ [char]
-- cases where (acc /= "")
tokenize acc tokens (char : rest)
    | isWhiteSpace char =
        -- if whitespace, then new "word" -> make the previous a token
        case readToken acc of
            Right token -> tokenize "" (tokens ++ [token]) rest
            Left err ->
                Left err
    | isNameChar char = tokenize (acc ++ [char]) tokens rest -- in future check for kind of acc
    | isOperatorChar char =
        case (readToken acc, readToken [char]) of
            (Right token1, Right token2) ->
                tokenize "" (tokens ++ [token1, token2]) rest
            (Left err, _) -> Left err
            (_, Left err) -> Left err
    | otherwise = Left $ "Invalid character: " ++ [char]

readToken :: String -> Result Token
readToken s =
    case s of
        "+" -> Right $ BinOpToken Plus
        "-" -> Right $ BinOpToken Minus
        "*" -> Right $ BinOpToken Multiply
        "/" -> Right $ BinOpToken Divide
        "^" -> Right $ BinOpToken Power
        "(" -> Right OpenParenthesisToken
        ")" -> Right CloseParenthesisToken
        "sin" -> Right $ FuncToken Sin
        "cos" -> Right $ FuncToken Cos
        "tan" -> Right $ FuncToken Tan
        "pi" -> Right $ FuncToken Pi
        "e" -> Right $ FuncToken E
        "max" -> Right $ FuncToken Max
        _ ->
            case readMaybe s of
                Just n -> Right $ NumToken n
                Nothing -> Left $ "Undefined token, NAN: " ++ s

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` [' ', '\t', '\n']

-- -- isNum char ||
-- isNum :: Char -> Bool
-- isNum c = isDigit c || c == '.'

isNameChar :: Char -> Bool
isNameChar c =
    c `elem` ['a' .. 'z']
        || c `elem` ['A' .. 'Z']
        || isDigit c
        -- \|| c == '_'
        || c `elem` ['_', '.']

isOperatorChar :: Char -> Bool
isOperatorChar c = c `elem` ['+', '-', '*', '/', '^', '(', ')', ';']
