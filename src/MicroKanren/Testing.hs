module MicroKanren.Testing where

import MicroKanren.Types

data Symbol = Pea | Pod |
                Olive | Oil |
                Plum |
                Tea | Cup |
                Fail
                deriving (Eq, Show, Ord, Enum)

data Variable = U | V | W |
                X | Y | Z
                deriving (Eq, Show, Ord, Enum)

testVars :: [v] -> [Expr a v]
testVars = (Variable <$>) . indexed

indexed :: [a] -> [(Int, a)]
indexed = zip [0..]
