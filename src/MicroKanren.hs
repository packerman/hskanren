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

type LogicM a = RWS () (Conj a) Counter

type Relation a = Expr a -> LogicM a (Goal a)

disj :: [Goal a] -> Goal a
disj = foldr disj2 failure

conj :: [Goal a] -> Goal a
conj = foldr conj2 success

newtype Disj a = Disj { getDisj :: Goal a }

instance Semigroup (Disj a) where
    (Disj g1) <> (Disj g2) = Disj $ g1 `disj2` g2

instance Monoid (Disj a) where
    mempty = Disj failure

newtype Conj a = Conj { getConj :: Goal a }

instance Semigroup (Conj a) where
    (Conj g1) <> (Conj g2) = Conj $ g1 `conj2` g2

instance Monoid (Conj a) where
    mempty = Conj success

-- |
-- >>> let [pea, pod] = Value <$> ["pea", "pod"]
-- >>> run (goal failure >> newVar)
-- []
-- >>> run (goal (pea === pod) >> newVar)
-- []
-- >>> run $ do { q <- newVar; goal $ q === pea; pure q }
-- [Value "pea"]
-- >>> run $ do { q <- newVar; goal $ pea === q; pure q }
-- [Value "pea"]
-- >>> run (goal success >> newVar)
-- [Reified 0]
-- >>> run $ do { q <- newVar; goal $ q === q; pure q }
-- [Reified 0]
run :: LogicM a (Expr a) -> [Expr a]
run m = let (e, Conj g) = eval m
        in map (reify e) $ g emptySubst

-- |
-- >>> let [pea, pod] = Value <$> ["pea", "pod"]
-- >>> let [olive, oil] = Value <$> ["olive", "oil"]
-- >>> runWith $ \q -> goal $ q === pea
-- [Value "pea"]
-- >>> runWith $ \q -> do { x <- newVar; goal $ pea === q }
-- [Value "pea"]
-- >>> runWith $ \q -> do { x <- newVar; goal $ pea === x }
-- [Reified 0]
-- >>> runWith $ \q -> do { x <- newVar; goal $ list [x] === q }
-- [Cons (Reified 0) Nil]
-- >>> runWith $ \q -> do { x <- newVar; goal $ x === q }
-- [Reified 0]
-- >>> runWith $ \q -> goal $ list [pea, pod] === list[pea, q]
-- [Value "pod"]
-- >>> runWith $ \q -> goal $ list [pea, pod] === list[q, pod]
-- [Value "pea"]
-- >>> runWith $ \q -> do { x <- newVar; goal $ list [q, x] === list [x, pod] }
-- [Value "pod"]
-- >>> runWith $ \q -> do { x <- newVar; goal $ list [x, x] === q }
-- [Cons (Reified 0) (Cons (Reified 0) Nil)]
-- >>> runWith $ \q -> do { x <- newVar; y <- newVar; goal $ list [q, y] === list [list [x, y], x] }
-- [Cons (Reified 0) (Cons (Reified 0) Nil)]
-- >>> runWith $ \q -> do { x <- newVar; y <- newVar; goal $ list [x, y] === q }
-- [Cons (Reified 0) (Cons (Reified 1) Nil)]
-- >>> runWith $ \q -> do { x <- newVar; y <- newVar; goal $ list [x, y, x] === q }
-- [Cons (Reified 0) (Cons (Reified 1) (Cons (Reified 0) Nil))]
-- >>> runWith $ \q -> goal $ disj2 (olive === q) (oil === q)
-- [Value "olive",Value "oil"]
runWith :: (Expr a -> LogicM a ()) -> [Expr a]
runWith f = run $ satisfying f

fresh :: (Expr a -> b) -> LogicM a b
fresh f = f <$> newVar

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
satisfying f = newVar >>= (\q -> f q >> pure q)

eval :: LogicM a b -> (b, Conj a)
eval m = evalRWS m () defaultCounter

-- |
-- >>> :{
--      let go = do x1 <- newVar
--                  x2 <- newVar
--                  y <- newVar
--                  return (x1 == x2, x1 == y, y == x2)
--      in fst $ eval go
-- :}
-- (False,False,False)
newVar :: LogicM a (Expr a)
newVar = Variable <$> state getAndInc

goal :: Goal a -> LogicM a ()
goal g = goals [g]

goals :: [Goal a] -> LogicM a ()
goals = tell . mconcat . map Conj
