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

-- |
-- >>> :set -XScopedTypeVariables
-- >>> let [pea, pod] = (Value <$> ["pea", "pod"] :: [Term Pair String Integer])
-- >>> eval $ fresh <&> (flip run (failure :: Goal Pair () Integer))
-- []
-- >>> eval $ fresh <&> (flip run $ pea === pod)
-- []
-- >>> eval $ fresh <&> (\q -> run q $ q === pea)
-- ["pea"]
-- >>> eval $ fresh <&> (\q -> run q $ pea === q)
-- ["pea"]
-- >>> eval $ fresh <&> (flip run (success :: Goal Pair () Integer))
-- [_0]
-- >>> eval $ fresh <&> (\(q :: Term Pair () Integer) -> run q $ q === q)
-- [_0]
run :: (Ord v, Foldable f, Functor f) => Term f a v -> Goal f a v -> [Term f a v]
run e g = map (reify e) $ g emptySubst

-- |
-- >>> :set -XScopedTypeVariables
-- >>> let [pea, pod] = Value <$> ["pea", "pod"]
-- >>> let [olive, oil] = Value <$> ["olive", "oil"]
-- >>> runWith $ \(q :: Term Pair String Integer) -> pure $ q === pea
-- ["pea"]
-- >>> runWith $ \(q :: Term Pair String Integer) -> fresh <&> (\x -> pea === q)
-- ["pea"]
-- >>> runWith $ \(q :: Term Pair String Integer) -> fresh <&> (\x -> pea === x)
-- [_0]
-- >>> runWith $ \(q :: Term Pair String Integer) -> fresh <&> (\x -> list [x] === q)
-- [(_0)]
-- >>> runWith $ \(q :: Term Pair String Integer) -> fresh <&> (\x -> x === q )
-- [_0]
-- >>> runWith $ \q -> pure $ list [pea, pod] === list[pea, q]
-- ["pod"]
-- >>> runWith $ \q -> pure $ list [pea, pod] === list[q, pod]
-- ["pea"]
-- >>> runWith $ \q -> fresh <&> (\x -> list [q, x] === list [x, pod])
-- ["pod"]
-- >>> runWith $ \q -> fresh <&> (\x -> list [x, x] === q)
-- [(_0 _0)]
-- >>> runWith $ \q -> do { x <- fresh; y <- fresh; pure $ list [q, y] === list [list [x, y], x] }
-- [(_0 _0)]
-- >>> runWith $ \q -> do { x <- fresh; y <- fresh; pure $ list [x, y] === q }
-- [(_0 _1)]
-- >>> runWith $ \q -> do { x <- fresh; y <- fresh; pure $ list [x, y, x] === q }
-- [(_0 _1 _0)]
-- >>> runWith $ \(q :: Term Pair String Integer) -> pure $ disj2 (olive === q) (oil === q)
-- ["olive","oil"]
runWith :: (Foldable f, Functor f) => (Term f a Integer -> LogicM (Goal f a Integer)) -> [Term f a Integer]
runWith f = eval $ fresh >>= (\q -> run q <$> f q)

runWith2 :: (Expr a -> Expr a -> LogicM (Goal Pair a Integer)) -> [Expr a]
runWith2 f = eval $ fresh >>= (\q ->
                        fresh >>= (\r ->
                            run (list [q, r]) <$> f q r))

-- |
-- >>> :set -XScopedTypeVariables
-- >>> :{
--  runWith $ \(x :: Term Pair Symbol Integer) -> pure $ conde [
--                                                          [Value Olive === x, failure],
--                                                          [Value Oil === x]]
-- :}
-- [Oil]
conde :: [[Goal f a v]] -> Goal f a v
conde = disj . map conj

eval :: LogicM a -> a
eval = flip evalState defaultCounter

-- |
-- >>> :set -XFlexibleContexts
-- >>> :set -XScopedTypeVariables
-- >>> :{
--      let go = do x1 :: (Term Pair () Integer) <- fresh
--                  x2 <- fresh
--                  y <- fresh
--                  return (x1 == x2, x1 == y, y == x2)
--      in eval go
-- :}
-- (False,False,False)
fresh :: (MonadState Counter m) => m (Term f a Integer)
fresh = Variable <$> state getAndInc

-- |
-- >>> eval $ fresh2 :: (Term Pair () Integer, Term Pair () Integer)
-- (var0,var1)
fresh2 :: (MonadState Counter m) => m (Term f a Integer, Term f a Integer)
fresh2 = liftM2 (,) fresh fresh

-- |
-- >>> eval $ fresh3 :: (Term Pair () Integer, Term Pair () Integer, Term Pair () Integer)
-- (var0,var1,var2)
fresh3 :: (MonadState Counter m) => m (Term f a Integer, Term f a Integer, Term f a Integer)
fresh3 = liftM3 (,,) fresh fresh fresh
