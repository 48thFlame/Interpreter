module Main (main) where

import Expression (solveExpression)
import Tokenizer (buildAST)

-- TODO: add unary minus `-5` instead of `0-5`

main :: IO ()
main = do
    putStrLn "---------"
    -- let toParse = "2 * (3-4)"
    -- let toParse = "12 *23+  6 ^ 7"
    -- let toParse = "32/           0 - 5/0"
    -- let toParse = "max 2 "
    -- let toParse = "max 2 "
    -- let toParse = "max 2 "
    -- let toParse = "e - 2.71828166893"
    -- let toParse = "sin (2*pi) / cos(2*pi) - tan(2*pi)"
    -- let toParse = "max (sin (tan 1) + 2) (max 3 4)"
    -- let toParse = "sin(sin(1 + 1))"
    -- let toParse = "((1+(sin (3 + (5 - 4)) - 2+2))     * max (2+(6-3)) 2 ) "
    let toParse = "log 1000 + logBase 10 1000 + ln 1000 + abs (0-5) + sqrt 16 + pi + e"
    -- let toParse = "((5 - - 6))/7"
    -- toParse <- readFile "input.txt"
    -- code <- readFile "code.fire"
    let ast = buildAST toParse
    print ast
    print $ solveExpression ast
