module Main (main) where

import AST

main :: IO ()
main = do
    putStrLn "---------"

    -- let toParse = "12 *23+  6 ^ 7"
    -- let toParse = "2 * (3-4)"
    -- let toParse = "12 / 0"
    -- let toParse = "((1+((5 - 4) - 2+2))*(2+(6-3)))"
    -- let toParse = "((5 - - 6))/7"
    let toParse = "((1)"
    print toParse

    let ps = parse toParse
    print ps

    let solution = solve ps
    print solution
