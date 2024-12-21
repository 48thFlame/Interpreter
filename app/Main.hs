module Main (main) where

import AST

main :: IO ()
main = do
    putStrLn "---------"

    let toParse = "  1 + 2324435546*4234.5454 / 58.87    ^ 0.2  "
    print toParse

    let ps = parse toParse
    print ps

    let solution = solve ps
    print solution
