{-# LANGUAGE TupleSections #-}
module MicroKanren where

import Control.Monad.State
import qualified Data.Map as M

data Expr a v = Value a |
                Variable v |
                Nil |
                Cons (Expr a v) (Expr a v)
                deriving (Eq, Show)

type EvalM = State Counter

type Substitution a v = M.Map v (Expr a v)

emptyS = M.empty

-- |
-- >>> let [x, y] = Variable <$> ['x', 'y']
-- >>> let [a, e] = Value <$> ['a', 'e']
-- >>> unify x a emptyS
-- Just (fromList [('x',Value 'a')])
-- >>> unify a y emptyS
-- Just (fromList [('y',Value 'a')])
-- >>> unify (Cons x a) (Cons e y) emptyS
-- Just (fromList [('x',Value 'e'),('y',Value 'a')])
-- >>> unify (Cons a x) (Cons e y) emptyS
-- Nothing
unify :: (Eq a, Ord v) => Expr a v -> Expr a v -> Substitution a v -> Maybe (Substitution a v)
unify u v s = 
    let u' = walk u s
        v' = walk v s in
            if u' == v' then Just s
            else case (u', v') of
                    (Variable x, _) -> extS x v' s
                    (_, Variable y) -> extS y u' s
                    (Cons ua ud, Cons va vd) -> (unify ua va s) >>= (unify ud vd)
                    _ -> Nothing

-- |
-- >>> let [v, w, x, y, z] = ['v'..'z']
-- >>> walk (Variable z) $ M.fromList [(z, Value 'a'), (x, Variable w), (y, Variable z)]
-- Value 'a'
-- >>> walk (Variable y) $ M.fromList [(z, Value 'a'), (x, Variable w), (y, Variable z)]
-- Value 'a'
-- >>> walk (Variable x) $ M.fromList [(z, Value 'a'), (x, Variable w), (y, Variable z)]
-- Variable 'w'
-- >>> walk (Variable x) $ M.fromList [(x, Variable y), (v, Variable x), (w, Variable x)]
-- Variable 'y'
-- >>> walk (Variable v) $ M.fromList [(x, Variable y), (v, Variable x), (w, Variable x)]
-- Variable 'y'
-- >>> walk (Variable w) $ M.fromList [(x, Variable y), (v, Variable x), (w, Variable x)]
-- Variable 'y'
-- >>> walk (Variable w) $ M.fromList [(x, Value 'b'), (z, Variable y), (w, list [Variable x, Value 'e', Variable z])]
-- Cons (Variable 'x') (Cons (Value 'e') (Cons (Variable 'z') Nil))
walk :: Ord v => Expr a v -> Substitution a v -> Expr a v
walk v@(Variable x) s = case M.lookup x s of
                            Just e -> walk e s
                            _ -> v
walk e _ = e

-- |
-- >>> let [x, y, z] = ['x'..'z']
-- >>> extS x (list [Variable x]) emptyS
-- Nothing
-- >>> extS x (list [Variable y]) (M.fromList [(y, Variable x)])
-- Nothing
-- >>> :{ 
--    let s = M.fromList [(z, Variable x), (y, Variable z)]
--        in walk (Variable y) <$> (extS x (Value 'e') s)
-- :}
-- Just (Value 'e')
extS :: Ord v => v -> Expr a v -> Substitution a v -> Maybe (Substitution a v)
extS x v s = if occurs x v s 
                then Nothing
                else Just $ M.insert x v s

-- |
-- >>> let [x, y] = ['x', 'y']
-- >>> occurs x (Variable x) emptyS
-- True 
-- >>> occurs x (list [Variable y]) (M.fromList [(y, Variable x)])
-- True
occurs :: Ord v => v -> (Expr a v) -> Substitution a v -> Bool
occurs x v s = case walk v s of
                (Variable y) -> y == x
                (Cons a d) -> occurs x a s || occurs x d s
                _ -> False

-- |
-- >>> list [Value 1, Value 2, Value 3, Value 4]
-- Cons (Value 1) (Cons (Value 2) (Cons (Value 3) (Cons (Value 4) Nil)))
list :: [Expr a v] -> Expr a v
list = foldr Cons Nil

-- |
-- >>> :{
--      let go = do x1 <- var "x"
--                  x2 <- var "x"
--                  y <- var "y"
--                  return (x1 == x2, x1 == y, y == x2)
--      in evalState go defaultCounter
-- :}
-- (False,False,False)
var :: v -> EvalM (Expr a (Int, v))
var name = Variable <$> (,name) <$> state getAndInc

vars :: [v] -> EvalM [Expr a (Int, v)]
vars = traverse var

data Counter = Counter Int deriving (Show)

-- | Return the current state of counter and increments
-- 
-- >>> getAndInc (Counter 5)
-- (5,Counter 6)
getAndInc :: Counter -> (Int, Counter)
getAndInc (Counter n) = (n, Counter $ n + 1)

defaultCounter = Counter 0
