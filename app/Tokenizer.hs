module Tokenizer (
    Result,
    Token (..),
    Operator (..),
    precedence,
    Parenthesis (..),
    tokenize,
) where

import Data.Char (isDigit)

import Text.Read (readMaybe)

type Result a = Either String a

data Token
    = OpToken Operator
    | ParToken Parenthesis
    | NumToken Float
    deriving (Show, Eq)

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

precedence :: Operator -> Int
precedence Addition = 1
precedence Subtraction = 1
precedence Multiplication = 2
precedence Division = 2
precedence Power = 3

data Parenthesis
    = OpenP
    | CloseP
    deriving (Show, Eq)

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
