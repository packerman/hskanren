{-# LANGUAGE TupleSections #-}
module MicroKanren where

import Control.Monad.State

data Expr a v = Value a |
                Variable v |
                Nil |
                Cons (Expr a v) (Expr a v)
                deriving (Eq)

data Counter = Counter Int deriving (Show)

-- | Return the current state of counter and increments
-- 
-- >>> getAndInc (Counter 5)
-- (5,Counter 6)
getAndInc :: Counter -> (Int, Counter)
getAndInc (Counter n) = (n, Counter $ n + 1)

defaultCounter = Counter 0

type EvalM = State Counter

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