module Tokenizer (
    Result,
    Token (..),
    BinaryOperator (..),
    precedence,
    Function (..),
    numOfArgs,
    Parenthesis (..),
    tokenize,
    isNameChar,
) where

import Data.Char (isDigit)

import Text.Read (readMaybe)

type Result a = Either String a

data Token
    = BinOpToken BinaryOperator
    | FuncToken Function
    | ParToken Parenthesis
    | NumToken Float
    | SemicolonToken
    deriving (Show, Eq)

data BinaryOperator
    = Addition
    | Subtraction
    | Multiplication
    | Division
    | Power
    deriving (Eq)

instance Show BinaryOperator where
    show Addition = "+"
    show Subtraction = "-"
    show Multiplication = "*"
    show Division = "/"
    show Power = "^"

precedence :: BinaryOperator -> Int
precedence Addition = 1
precedence Subtraction = 1
precedence Multiplication = 2
precedence Division = 2
precedence Power = 3

data Function
    = Sin
    | Cos
    | Tan
    | Pi
    | E
    | Max
    | Min
    deriving (Eq)

numOfArgs :: Function -> Int
numOfArgs Sin = 1
numOfArgs Cos = 1
numOfArgs Tan = 1
numOfArgs Pi = 0
numOfArgs E = 0
numOfArgs Max = 2
numOfArgs Min = 2

instance Show Function where
    show Sin = "sin"
    show Cos = "cos"
    show Tan = "tan"
    show Pi = "pi"
    show E = "e"
    show Max = "max"
    show Min = "min"

data Parenthesis
    = OpenP
    | CloseP
    deriving (Show, Eq)

stringedTokenToToken :: String -> Result Token
stringedTokenToToken s =
    case s of
        "+" -> Right $ BinOpToken Addition
        "-" -> Right $ BinOpToken Subtraction
        "*" -> Right $ BinOpToken Multiplication
        "/" -> Right $ BinOpToken Division
        "^" -> Right $ BinOpToken Power
        "(" -> Right $ ParToken OpenP
        ")" -> Right $ ParToken CloseP
        ";" -> Right SemicolonToken
        "sin" -> Right $ FuncToken Sin -- TODO: make this more general
        "cos" -> Right $ FuncToken Cos -- TODO: make this more general
        "tan" -> Right $ FuncToken Tan -- TODO: make this more general
        "pi" -> Right $ FuncToken Pi -- TODO: make this more general
        "e" -> Right $ FuncToken E -- TODO: make this more general
        "max" -> Right $ FuncToken Max -- TODO: make this more general
        "min" -> Right $ FuncToken Min -- TODO: make this more general
        _ ->
            case readMaybe s of
                Just n -> Right $ NumToken n
                Nothing -> Left $ "Invalid token NAN: " ++ s

isNum :: Char -> Bool
isNum c = isDigit c || c == '.'

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` [' ', '\t', '\n']

isNameChar :: Char -> Bool
isNameChar c =
    c `elem` ['a' .. 'z']
        || c `elem` ['A' .. 'Z']
        || isDigit c
        || c == '_'

isOperatorChar :: Char -> Bool
isOperatorChar c = c `elem` ['+', '-', '*', '/', '^', '(', ')', ';']

-- * when operators are more then 1 char, then define this: `data AccType`

-- | parse a string and return the tokens
tokenize :: String -> [Token] -> [Char] -> Result [Token]
-- base cases: done going threw the string
tokenize "" tokens [] = Right tokens
tokenize acc tokens [] =
    case stringedTokenToToken acc of
        Right token -> Right $ tokens ++ [token]
        Left err ->
            Left err
-- cases where (acc == "")
tokenize "" tokens (char : rest)
    | isWhiteSpace char = tokenize "" tokens rest -- acc is empty so just continue
    | isNum char || isNameChar char = tokenize [char] tokens rest -- acc
    | isOperatorChar char =
        -- in the future also add to acc (now all operators are 1 char)
        case stringedTokenToToken [char] of
            Right token -> tokenize "" (tokens ++ [token]) rest
            Left err ->
                Left err -- should not get here ever, its an operatorChar!
    | otherwise = Left $ "Invalid character: " ++ [char]
-- cases where (acc /= "")
tokenize acc tokens (char : rest)
    | isWhiteSpace char =
        -- if whitespace, then new "word" -> make the previous a token
        case stringedTokenToToken acc of
            Right token -> tokenize "" (tokens ++ [token]) rest
            Left err ->
                Left err
    | isNum char || isNameChar char = tokenize (acc ++ [char]) tokens rest -- in future check for kind of acc
    | isOperatorChar char =
        case (stringedTokenToToken acc, stringedTokenToToken [char]) of
            (Right token1, Right token2) ->
                tokenize "" (tokens ++ [token1, token2]) rest
            (Left err, _) -> Left err
            (_, Left err) -> Left err
    | otherwise = Left $ "Invalid character: " ++ [char]
