module Main (main) where

import Parser

main :: IO ()
main = do
    putStrLn "---------"

    -- let toParse = "12 *23+  6 ^ 7"
    -- let toParse = "2 * (3-4)"
    -- let toParse = "12 / 0"
    -- let toParse = "max 2 "
    -- let toParse = "sin pi"
    -- let toParse = "sin(sin(1 + 1))"
    let toParse = "((1+(sin (3 + (5 - 4)) - 2+2))     * max (2+(6-3)) (fst 3 6) ) "
    -- let toParse = "((5 - - 6))/7"
    -- let toParse = "((1+1)"
    -- let toParse = "fst 2 3"
    -- toParse <- readFile "input.txt"
    print toParse

    let ast = parse toParse
    print ast

    let solution = solve ast
    print solution
