module Parser (buildAST) where

import Debug.Trace

import Data.Function ((&))
import Expression
import Tokenizer

debugLog :: (Show b) => b -> b
debugLog a =
    traceShow a a

buildAST :: String -> [Expression]
buildAST s =
    tokenize "" [] s
        & either error (parse [] [] . debugLog)

parse :: [ExpressionToken] -> [Expression] -> [Token] -> [Expression]
parse [] acc [] = acc
parse [] _ (SemicolonToken : _) =
    error "Semicolon on empty expression!"
parse _ _ [] =
    error "Missing semicolon!"
parse currentExpr acc (ExpressionToken et : rest) =
    parse (currentExpr ++ [et]) acc rest
parse currentExpr acc (SemicolonToken : rest) =
    case shuntingYard [] [] currentExpr of
        Right e -> parse [] (acc ++ [e]) rest
        Left err -> error err
