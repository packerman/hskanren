{-# LANGUAGE TupleSections #-}
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

type LogicM a v = RWS () [Goal a v] Counter

type Relation a v = Expr a v -> Goal a v

disj :: [Goal a v] -> Goal a v
disj = foldr disj2 failure

conj :: [Goal a v] -> Goal a v
conj = foldr conj2 success

-- |
-- >>> let [pea, pod] = Value <$> ["pea", "pod"]
-- >>> let q = 'q'
-- >>> run (goal failure >> var q)
-- []
-- >>> run (goal (pea === pod) >> var q)
-- []
-- >>> run $ do { q <- var q; goal $ q === pea; pure q }
-- [Value "pea"]
-- >>> run $ do { q <- var q; goal $ pea === q; pure q }
-- [Value "pea"]
-- >>> run (goal success >> var q)
-- [Reified 0]
-- >>> run $ do { q <- var q; goal $ q === q; pure q }
-- [Reified 0]
run :: Ord v => LogicM a v (Expr a v) -> [Expr a v]
run m = let (e, gs) = eval m
        in map (reify e) $ (conj gs) emptySubst

-- |
-- >>> let [pea, pod] = Value <$> ["pea", "pod"]
-- >>> let [olive, oil] = Value <$> ["olive", "oil"]
-- >>> runWith 'q' $ \q -> goal $ q === pea
-- [Value "pea"]
-- >>> runWith 'q' $ \q -> do { x <- var 'x'; goal $ pea === q }
-- [Value "pea"]
-- >>> runWith 'q' $ \q -> do { x <- var 'x'; goal $ pea === x }
-- [Reified 0]
-- >>> runWith 'q' $ \q -> do { x <- var 'x'; goal $ list [x] === q }
-- [Cons (Reified 0) Nil]
-- >>> runWith 'q' $ \q -> do { x <- var 'x'; goal $ x === q }
-- [Reified 0]
-- >>> runWith 'q' $ \q -> goal $ list [pea, pod] === list[pea, q]
-- [Value "pod"]
-- >>> runWith 'q' $ \q -> goal $ list [pea, pod] === list[q, pod]
-- [Value "pea"]
-- >>> runWith 'q' $ \q -> do { x <- var 'x'; goal $ list [q, x] === list [x, pod] }
-- [Value "pod"]
-- >>> runWith 'q' $ \q -> do { x <- var 'x'; goal $ list [x, x] === q }
-- [Cons (Reified 0) (Cons (Reified 0) Nil)]
-- >>> runWith 'q' $ \q -> do { x <- var 'x'; y <- var 'y'; goal $ list [q, y] === list [list [x, y], x] }
-- [Cons (Reified 0) (Cons (Reified 0) Nil)]
-- >>> runWith 'q' $ \q -> do { x <- var 'x'; y <- var 'y'; goal $ list [x, y] === q }
-- [Cons (Reified 0) (Cons (Reified 1) Nil)]
-- >>> runWith 'q' $ \q -> do { x <- var 'x'; y <- var 'y'; goal $ list [x, y, x] === q }
-- [Cons (Reified 0) (Cons (Reified 1) (Cons (Reified 0) Nil))]
-- >>> runWith 'q' $ \q -> goal $ disj2 (olive === q) (oil === q)
-- [Value "olive",Value "oil"]
runWith :: Ord v => v -> (Expr a v -> LogicM a v ()) -> [Expr a v]
runWith q f = run $ satisfying q f

fresh :: v -> (Expr a v -> b) -> LogicM a v b
fresh name f = f <$> var name 

satisfying :: Ord v => v -> (Expr a v -> LogicM a v ()) -> LogicM a v (Expr a v)
satisfying q f = var q >>= (\q' -> f q' >> pure q')

eval :: LogicM a v b -> (b, [Goal a v])
eval m = evalRWS m () defaultCounter

-- |
-- >>> :{
--      let go = do x1 <- var "x"
--                  x2 <- var "x"
--                  y <- var "y"
--                  return (x1 == x2, x1 == y, y == x2)
--      in fst $ eval go
-- :}
-- (False,False,False)
var :: v -> LogicM a v (Expr a v)
var name = Variable <$> (,name) <$> state getAndInc

vars :: [v] -> LogicM a v [Expr a v]
vars = traverse var

goal :: Goal a v -> LogicM a v ()
goal g = goals [g]

goals :: [Goal a v] -> LogicM a v ()
goals = tell
