module MicroKanren(
    module MicroKanren.Types,
    module MicroKanren
    ) 
where

import Control.Monad.State
import Data.Functor

import MicroKanren.Types
import MicroKanren.Functions
import MicroKanren.Internal
import MicroKanren.Testing

type LogicM = State Counter

type Relation a = Expr a -> (Goal a)

type RelationM a = Expr a -> LogicM (Goal a)

disj :: [Goal a] -> Goal a
disj = foldr disj2 failure

conj :: [Goal a] -> Goal a
conj = foldr conj2 success

disjM :: Monad m => [m (Goal a)] -> m (Goal a)
disjM = fmap disj . sequence

conjM :: Monad m => [m (Goal a)] -> m (Goal a)
conjM = fmap conj . sequence

-- |
-- >>> let [pea, pod] = Value <$> ["pea", "pod"]
-- >>> run $ fresh <&> (\q -> (q, pure failure))
-- []
-- >>> run $ fresh <&> (\q -> (q, pure $ pea === pod))
-- []
-- >>> run $ fresh <&> (\q -> (q, pure $ q === pea))
-- [Value "pea"]
-- >>> run $ fresh <&> (\q -> (q, pure $ pea === q))
-- [Value "pea"]
-- >>> run $ fresh <&> (\q -> (q, pure success))
-- [Reified 0]
-- >>> run $ fresh <&> (\q -> (q, pure $ q === q))
-- [Reified 0]
run :: LogicM (Expr a, LogicM (Goal a)) -> [Expr a]
run m = eval $ m >>= (\(e, g) -> run' e g)

run' :: Expr a -> LogicM (Goal a) -> LogicM [Expr a]
run' e g = map (reify e) <$> (g <*> pure emptySubst)

-- |
-- >>> let [pea, pod] = Value <$> ["pea", "pod"]
-- >>> let [olive, oil] = Value <$> ["olive", "oil"]
-- >>> runWith $ \q -> pure $ q === pea
-- [Value "pea"]
-- >>> runWith $ \q -> fresh <&> (\x -> pea === q)
-- [Value "pea"]
-- >>> runWith $ \q -> fresh <&> (\x -> pea === x)
-- [Reified 0]
-- >>> runWith $ \q -> fresh <&> (\x -> list [x] === q)
-- [Cons (Reified 0) Nil]
-- >>> runWith $ \q -> fresh <&> (\x -> x === q )
-- [Reified 0]
-- >>> runWith $ \q -> pure $ list [pea, pod] === list[pea, q]
-- [Value "pod"]
-- >>> runWith $ \q -> pure $ list [pea, pod] === list[q, pod]
-- [Value "pea"]
-- >>> runWith $ \q -> fresh <&> (\x -> list [q, x] === list [x, pod])
-- [Value "pod"]
-- >>> runWith $ \q -> fresh <&> (\x -> list [x, x] === q)
-- [Cons (Reified 0) (Cons (Reified 0) Nil)]
-- >>> runWith $ \q -> do { x <- fresh; y <- fresh; pure $ list [q, y] === list [list [x, y], x] }
-- [Cons (Reified 0) (Cons (Reified 0) Nil)]
-- >>> runWith $ \q -> do { x <- fresh; y <- fresh; pure $ list [x, y] === q }
-- [Cons (Reified 0) (Cons (Reified 1) Nil)]
-- >>> runWith $ \q -> do { x <- fresh; y <- fresh; pure $ list [x, y, x] === q }
-- [Cons (Reified 0) (Cons (Reified 1) (Cons (Reified 0) Nil))]
-- >>> runWith $ \q -> pure $ disj2 (olive === q) (oil === q)
-- [Value "olive",Value "oil"]
runWith :: (Expr a -> LogicM (Goal a)) -> [Expr a]
runWith f = eval $ fresh >>= (\q -> run' q (f q))

-- |
-- >>> :{
--  runWith $ \x -> pure $ conde [
--                                  [Value Olive === x, failure],
--                                  [Value Oil === x]]
-- :}
-- [Value Oil]
conde :: [[Goal a]] -> Goal a
conde = disj . map conj

eval :: LogicM a -> a
eval m = evalState m defaultCounter

-- |
-- >>> :{
--      let go = do x1 <- fresh
--                  x2 <- fresh
--                  y <- fresh
--                  return (x1 == x2, x1 == y, y == x2)
--      in eval go
-- :}
-- (False,False,False)
fresh :: LogicM (Expr a)
fresh = Variable <$> state getAndInc
