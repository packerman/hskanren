module Wires where

import MicroKanren

disj :: [Goal a v] -> Goal a v
disj = foldr disj2 failure

conj :: [Goal a v] -> Goal a v
conj = foldr conj2 success

-- TODO - what instead of defrel?
-- TODO - some monad?
-- function returning Goal - applicative?

-- |
-- >>> let q = 'q'
-- >> 
-- >>> runAll q failure
-- []
-- >>> runAll q 
runAll = undefined
