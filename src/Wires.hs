module Wires where

import Data.Functor

import MicroKanren

disj :: [Goal a v] -> Goal a v
disj = foldr disj2 failure

conj :: [Goal a v] -> Goal a v
conj = foldr conj2 success

-- TODO - what instead of defrel?
-- TODO - some monad?
-- function returning Goal - applicative?

-- |
-- >>> let [pea, pod] = Value <$> ["pea", "pod"]
-- >>> let q = 'q'
-- >>> run q (\q -> failure)
-- []
-- >>> run q (\q -> pea === pod)
-- []
-- >>> run q (\q -> q === pea)
-- [Value "pea"]
-- >>> run q (\q -> pea === q)
-- [Value "pea"]
-- >>> run q (\q -> success)
-- [Reified 0]
-- >>> run q (\q -> q === q)
-- [Reified 0]
run :: Ord v => v -> (Expr a v -> Goal a v) -> [Expr a v]
run q f = eval $ do
                    q' <- var q
                    pure $ map (reify q') $ (f q') emptySubst
