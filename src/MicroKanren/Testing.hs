module MicroKanren.Testing where

import MicroKanren.Types

data Symbol = Pea | Pod |
                Olive | Oil |
                Plum | Pear |
                Grape | Raisin |
                Tea | Cup |
                Fail |
                A | B | C
                deriving (Eq, Show, Ord, Enum)

data Variable = Q | R | S | T |
                U | V | W |
                X | Y | Z
                deriving (Eq, Show, Ord, Enum)

testVars :: Integer -> [Term f]
testVars n = Variable <$> [1..n]

-- TODO remove
indexed :: [a] -> [(Int, a)]
indexed = zip [0..]
