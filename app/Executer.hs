module Executer (runAST) where

import AST
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe)

import Expression

data Runtime = Runtime
    { variables :: Map.Map String Float
    }

solveExpression :: Expression -> Float
solveExpression (Value n) = n
solveExpression (FunctionCall f args) =
    case f of
        Sin ->
            sin $ solveExpression $ head args
        Cos ->
            cos $ solveExpression $ head args
        Tan ->
            tan $ solveExpression $ head args
        Pi ->
            pi
        E ->
            exp 1
        Max ->
            max (solveExpression $ head args) (solveExpression $ args !! 1)
solveExpression (BinaryCalculation l o r) =
    case o of
        Plus ->
            solveExpression l + solveExpression r
        Minus ->
            solveExpression l - solveExpression r
        Multiply ->
            solveExpression l * solveExpression r
        Divide ->
            solveExpression l / solveExpression r
        Power ->
            solveExpression l ** solveExpression r

runAST :: AST -> Float
runAST (Expression e) = solveExpression e
runAST (Block asts) =
    map runAST asts
        & reverse
        & listToMaybe
        & fromMaybe 0