module Main (main) where

import AST

main :: IO ()
main = do
    putStrLn "\n"

    let t = AST{left = AST{left = Value 1, opp = Addition, right = Value 1}, opp = Addition, right = AST{left = AST{left = Value 1, opp = Addition, right = Value 1}, opp = Addition, right = AST{left = AST{left = Value 1, opp = Addition, right = Value 1}, opp = Addition, right = AST{left = AST{left = Value 1, opp = Addition, right = Value 1}, opp = Addition, right = Value 1}}}}

    print t
