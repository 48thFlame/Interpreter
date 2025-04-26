module AST (
    AST (..),
    buildAST,
) where

import Debug.Trace

import Data.Function ((&))

-- import qualified Data.List as List

-- import qualified Data.Map as Map
-- import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Tree as Tree
import Data.Tree.Pretty (drawVerticalTree)
import Expression
import Tokenizer

debugLog :: (Show b) => b -> b
debugLog a =
    traceShow a a

data AST
    = Expression Expression
    | Block [AST]
    | Assignment String Expression

-- \| Loop Expression AST

astToDataTree :: AST -> Tree.Tree String
astToDataTree (Expression e) =
    Tree.Node "Expression" [expressionToDataTree e]
astToDataTree (Block asts) =
    Tree.Node "Block" (map astToDataTree asts)
astToDataTree (Assignment s e) =
    Tree.Node "Assign" [Tree.Node s [], expressionToDataTree e]

-- astToDataTree (Loop e ast) =
--     Tree.Node "Loop" [Tree.Node "Times" [expressionToDataTree e], astToDataTree ast]

instance Show AST where
    show ast =
        drawVerticalTree $ astToDataTree ast

-- runAST (Loop e ast) =
--     let times = round $ solveExpression e
--      in List.replicate times (runAST ast)
--             & take times
--             & sum

buildAST :: String -> AST
buildAST s =
    debugLog s
        & tokenize "" []
        & either error (parse [] [] . debugLog)
        & either error fst

{- |
There is an error when there is an end and no do,
it just create a block as if there is a do,
because the end branch doesn't know whether there was a do or not,
so fix when add a method to keep track of what is parsing a block? loop? if? etc
-}

-- | takes a list of tokens and returns an AST, the other args are accumulators
parse :: [AST] -> [ExpressionToken] -> [Token] -> Result (AST, [Token])
parse _ _ [] =
    -- finished going through all the tokens, but there's no end token!
    Left "No end token!"
parse _ (_ : _) (DoToken : _) =
    -- got to DoToken,
    -- but there should be an expression to parse (the expAcc isn't empty)
    -- so that means we never got a semicolon
    Left "No semicolon!"
parse _ (_ : _) (EndToken : _) =
    -- got to EndToken, but there should be an expression to parse
    -- the expAcc isn't empty! meaning we never got a semicolon
    Left "Missing semicolon!"
parse astAcc [] (DoToken : rest) =
    case parse [] [] rest of
        Right (blockAst, []) ->
            Right (blockAst, [])
        Right (blockAst, remaining) ->
            parse (astAcc ++ [blockAst]) [] remaining
        Left err ->
            Left err
parse astAcc [] (EndToken : rest) =
    Right (Block astAcc, rest)
parse astAcc expAcc (ExpressionToken et : rest) =
    parse astAcc (expAcc ++ [et]) rest
parse astAcc expAcc (SemicolonToken : rest) =
    case shuntingYard [] [] expAcc of
        Right expr ->
            parse (astAcc ++ [Expression expr]) [] rest
        Left err ->
            Left err
