module MicroKanren(
    module MicroKanren.Types,
    module MicroKanren
    ) 
where

import Control.Monad.RWS
import Data.Functor

import MicroKanren.Types
import MicroKanren.Functions
import MicroKanren.Internal
import MicroKanren.Testing

type LogicM a = RWS () [Goal a] Counter

type Relation a = Expr a -> LogicM a (Goal a)

disj :: [Goal a] -> Goal a
disj = foldr disj2 failure

conj :: [Goal a] -> Goal a
conj = foldr conj2 success

-- |
-- >>> let [pea, pod] = Value <$> ["pea", "pod"]
-- >>> run (goal failure >> fresh)
-- []
-- >>> run (goal (pea === pod) >> fresh)
-- []
-- >>> run $ do { q <- fresh; goal $ q === pea; pure q }
-- [Value "pea"]
-- >>> run $ do { q <- fresh; goal $ pea === q; pure q }
-- [Value "pea"]
-- >>> run (goal success >> fresh)
-- [Reified 0]
-- >>> run $ do { q <- fresh; goal $ q === q; pure q }
-- [Reified 0]
run :: LogicM a (Expr a) -> [Expr a]
run m = let (e, goals) = eval m
        in map (reify e) $ (conj goals) emptySubst

-- |
-- >>> let [pea, pod] = Value <$> ["pea", "pod"]
-- >>> let [olive, oil] = Value <$> ["olive", "oil"]
-- >>> runWith $ \q -> goal $ q === pea
-- [Value "pea"]
-- >>> runWith $ \q -> do { x <- fresh; goal $ pea === q }
-- [Value "pea"]
-- >>> runWith $ \q -> do { x <- fresh; goal $ pea === x }
-- [Reified 0]
-- >>> runWith $ \q -> do { x <- fresh; goal $ list [x] === q }
-- [Cons (Reified 0) Nil]
-- >>> runWith $ \q -> do { x <- fresh; goal $ x === q }
-- [Reified 0]
-- >>> runWith $ \q -> goal $ list [pea, pod] === list[pea, q]
-- [Value "pod"]
-- >>> runWith $ \q -> goal $ list [pea, pod] === list[q, pod]
-- [Value "pea"]
-- >>> runWith $ \q -> do { x <- fresh; goal $ list [q, x] === list [x, pod] }
-- [Value "pod"]
-- >>> runWith $ \q -> do { x <- fresh; goal $ list [x, x] === q }
-- [Cons (Reified 0) (Cons (Reified 0) Nil)]
-- >>> runWith $ \q -> do { x <- fresh; y <- fresh; goal $ list [q, y] === list [list [x, y], x] }
-- [Cons (Reified 0) (Cons (Reified 0) Nil)]
-- >>> runWith $ \q -> do { x <- fresh; y <- fresh; goal $ list [x, y] === q }
-- [Cons (Reified 0) (Cons (Reified 1) Nil)]
-- >>> runWith $ \q -> do { x <- fresh; y <- fresh; goal $ list [x, y, x] === q }
-- [Cons (Reified 0) (Cons (Reified 1) (Cons (Reified 0) Nil))]
-- >>> runWith $ \q -> goal $ disj2 (olive === q) (oil === q)
-- [Value "olive",Value "oil"]
runWith :: (Expr a -> LogicM a ()) -> [Expr a]
runWith f = run $ satisfying f

-- |
-- >>> :{
--  runWith $ \x -> do
--                      goal $ conde [
--                              [Value Olive === x, failure],
--                              [Value Oil === x]]
-- :}
-- [Value Oil]
conde :: [[Goal a]] -> Goal a
conde = disj . map conj

satisfying :: (Expr a -> LogicM a ()) -> LogicM a (Expr a)
satisfying f = fresh >>= (\q -> f q >> pure q)

eval :: LogicM a b -> (b, [Goal a])
eval m = evalRWS m () defaultCounter

-- |
-- >>> :{
--      let go = do x1 <- fresh
--                  x2 <- fresh
--                  y <- fresh
--                  return (x1 == x2, x1 == y, y == x2)
--      in fst $ eval go
-- :}
-- (False,False,False)
fresh :: LogicM a (Expr a)
fresh = Variable <$> state getAndInc

goal :: Goal a -> LogicM a ()
goal g = goals [g]

goals :: [Goal a] -> LogicM a ()
goals = tell
