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
-- >>> let [u, v, w, x, y, z] = ['u'..'z']
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
walk vx@(Variable x) s = case M.lookup x s of
                            Just e -> walk e s
                            _ -> vx
walk e _ = e

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
