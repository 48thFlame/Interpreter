module Main (main) where

import AST

main :: IO ()
main = do
    putStrLn "---------"

    let toParse = "12 *23+  6 ^ 7"
    print toParse

    let ps = parse toParse
    print ps

    let solution = solve ps
    print solution
