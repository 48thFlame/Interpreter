module Main (main) where

import Parser

main :: IO ()
main = do
    putStrLn "---------"

    -- let toParse = "12 *23+  6 ^ 7"
    -- let toParse = "2 * (3-4)"
    -- let toParse = "12 / 0"
    let toParse = "((1+((5 - 4) - 2+2))*(2+(6-3)))"
    -- let toParse = "((5 - - 6))/7"
    -- let toParse = "((1+1))"
    print toParse

    let ast = parse toParse
    print ast

    let solution = solve ast
    print solution
