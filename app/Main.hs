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

    let ps = parse "123 - 24 + 35 + 6"

    print ps

    let s2 = solve ps
    print s2