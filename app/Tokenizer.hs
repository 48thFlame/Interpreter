module Tokenizer (
    Result,
    Token (..),
    ExpressionToken (..),
    BinaryOperator (..),
    precedence,
    Function (..),
    numOfArgs,
    tokenize,
) where

import Data.Char (isDigit)

import Text.Read (readMaybe)

import Expression

data Token
    = SemicolonToken
    | DoToken
    | EndToken
    | ExpressionToken ExpressionToken
    deriving (Show)

{- | parse a string and return the tokens
tokenize acc tokens str
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
    | isNum char || isNameChar char = tokenize [char] tokens rest -- acc
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
    | isNum char || isNameChar char = tokenize (acc ++ [char]) tokens rest -- in future check for kind of acc
    | isOperatorChar char =
        case (readToken acc, readToken [char]) of
            (Right token1, Right token2) ->
                tokenize "" (tokens ++ [token1, token2]) rest
            (Left err, _) -> Left err
            (_, Left err) -> Left err
    | otherwise = Left $ "Invalid character: " ++ [char]

-- | TODO: should have words do 1 thing, and operators do another, like read from some big map
readToken :: String -> Result Token
readToken s =
    case s of
        "+" -> Right $ ExpressionToken $ BinOpToken Plus
        "-" -> Right $ ExpressionToken $ BinOpToken Minus
        "*" -> Right $ ExpressionToken $ BinOpToken Multiply
        "/" -> Right $ ExpressionToken $ BinOpToken Divide
        "^" -> Right $ ExpressionToken $ BinOpToken Power
        "(" -> Right $ ExpressionToken OpenParenthesisToken
        ")" -> Right $ ExpressionToken CloseParenthesisToken
        ";" -> Right SemicolonToken
        "sin" -> Right $ ExpressionToken $ FuncToken Sin
        "cos" -> Right $ ExpressionToken $ FuncToken Cos
        "tan" -> Right $ ExpressionToken $ FuncToken Tan
        "pi" -> Right $ ExpressionToken $ FuncToken Pi
        "e" -> Right $ ExpressionToken $ FuncToken E
        "max" -> Right $ ExpressionToken $ FuncToken Max
        "do" -> Right DoToken
        "end" -> Right EndToken
        _ ->
            case readMaybe s of
                Just n -> Right $ ExpressionToken $ NumToken n
                Nothing -> Left $ "Invalid token NAN: " ++ s

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` [' ', '\t', '\n']

isNum :: Char -> Bool
isNum c = isDigit c || c == '.'

isNameChar :: Char -> Bool
isNameChar c =
    c `elem` ['a' .. 'z']
        || c `elem` ['A' .. 'Z']
        || isDigit c
        || c == '_'

isOperatorChar :: Char -> Bool
isOperatorChar c = c `elem` ['+', '-', '*', '/', '^', '(', ')', ';']
