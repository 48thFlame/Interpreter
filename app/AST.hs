module AST (
    Operator (..),
    AST (..),
) where

import Data.Tree
import Data.Tree.Pretty

data Operator = Addition

instance Show Operator where
    show Addition = "+"

data AST = AST {left :: AST, opp :: Operator, right :: AST} | Value Float

astToDataTree :: AST -> Tree String
astToDataTree (Value n) = Node (show n) []
astToDataTree AST{left = l, opp = o, right = r} =
    Node (show o) [astToDataTree l, astToDataTree r]

instance Show AST where
    show ast =
        drawVerticalTree $ astToDataTree ast
