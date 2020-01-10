{-# LANGUAGE FlexibleContexts #-}

module MicroKanren(
    module MicroKanren.Types,
    module MicroKanren
    ) 
where

import Control.Monad
import Control.Monad.State
import Data.Functor

import MicroKanren.Types
import MicroKanren.Functions
import MicroKanren.Internal
import MicroKanren.Testing
import MicroKanren.Pair

type LogicM = State Counter

type Relation f a v = Term f a v -> Goal f a v

type RelationM f a v = Term f a v -> LogicM (Goal f a v)

disj :: [Goal f a v] -> Goal f a v
disj = foldr disj2 failure

conj :: [Goal f a v] -> Goal f a v
conj = foldr conj2 success

disjM :: Monad m => [m (Goal f a v)] -> m (Goal f a v)
disjM = fmap disj . sequence

conjM :: Monad m => [m (Goal f a v)] -> m (Goal f a v)
conjM = fmap conj . sequence

run :: (Ord v, Foldable f, Functor f) => Term f a v -> Goal f a v -> [Term f a v]
run e g = map (reify e) $ g emptySubst

runWith :: (Foldable f, Functor f) => (Term f a Integer -> LogicM (Goal f a Integer)) -> [Term f a Integer]
runWith f = eval $ fresh >>= (\q -> run q <$> f q)

runWith2 :: (Expr a -> Expr a -> LogicM (Goal Pair a Integer)) -> [Expr a]
runWith2 f = eval $ fresh >>= (\q ->
                        fresh >>= (\r ->
                            run (list [q, r]) <$> f q r))

conde :: [[Goal f a v]] -> Goal f a v
conde = disj . map conj

eval :: LogicM a -> a
eval = flip evalState defaultCounter

fresh :: (MonadState Counter m) => m (Term f a Integer)
fresh = Variable <$> state getAndInc

fresh2 :: (MonadState Counter m) => m (Term f a Integer, Term f a Integer)
fresh2 = liftM2 (,) fresh fresh

fresh3 :: (MonadState Counter m) => m (Term f a Integer, Term f a Integer, Term f a Integer)
fresh3 = liftM3 (,,) fresh fresh fresh
