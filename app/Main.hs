module Main (main) where

-- import Expression (solveExpression)
import AST

import Executer

main :: IO ()
main = do
    putStrLn "---------"

    -- let toParse = "2 * (3-4);"

    -- let toParse = "12 *23+  6 ^ 7;"
    -- let toParse = "do 2+2; 3*6; do 2+3; do 4*8;end end end"
    let toParse = "do 2+2;3*6;  end "
    -- let toParse = "max 2 "
    -- let toParse = "sin;"
    -- let toParse = "e - 2.71828166893 ; sin (2*pi) / cos(2*pi) - tan(2*pi);"
    -- let toParse = "min (sin (tan 1) + 2) (max 3 4)"
    -- let toParse = "sin(sin(1 + 1))"
    -- let toParse = "((1+(sin (3 + (5 - 4)) - 2+2))     * max (2+(6-3)) 2 ) "
    -- let toParse = "((5 - - 6))/7"
    -- let toParse = "(1+1) * 3;\n\n\n 2+3; 3^2"
    -- let toParse = "1 1 * + 2           \n\n\t;(3+1)"
    -- let toParse = "fst 2 3"
    -- toParse <- readFile "input.txt"
    -- code <- readFile "code.fire"
    let ast = buildAST toParse
    print ast
    print $ runAST ast
