module Main (main) where

import AST

main :: IO ()
main = do
    putStrLn ""

    let t =
            AST
                { left =
                    AST
                        { left = Value 1
                        , operator = Addition
                        , right = Value 1
                        }
                , operator = Addition
                , right =
                    AST
                        { left =
                            AST
                                { left =
                                    Value 1
                                , operator = Addition
                                , right = Value 1
                                }
                        , operator = Addition
                        , right =
                            AST
                                { left =
                                    AST{left = Value 1, operator = Addition, right = Value 1}
                                , operator = Addition
                                , right =
                                    AST
                                        { left =
                                            AST{left = Value 1, operator = Addition, right = Value 1}
                                        , operator = Addition
                                        , right = Value 1
                                        }
                                }
                        }
                }

    print t

    let s = solve t

    print s

    putStrLn ""

    let ps = parse "8 ^ 5 / 8 - 8 ^ 4"

    print ps

    let s2 = solve ps
    print s2