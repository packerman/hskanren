{-# LANGUAGE TupleSections #-}
module MicroKanren where

import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe

data Expr a v = Value a |
                Variable v |
                Nil |
                Cons (Expr a v) (Expr a v)
                deriving (Eq, Show)

type EvalM = State Counter

type Substitution a v = M.Map v (Expr a v)

emptyS = M.empty

type Goal a v = Substitution a v -> [Substitution a v]

type Var v = (Int, v)

-- |
-- >>> eval $ take 1 <$> ((callFresh "kiwi" (\fruit -> (Value "plum" === fruit))) <*> pure emptyS)
-- [fromList [((0,"kiwi"),Value "plum")]]
callFresh :: v -> (Expr a (Var v) -> Goal a (Var v)) -> EvalM (Goal a (Var v))
callFresh name f = f <$> var name 

eval :: EvalM a -> a
eval m = evalState m defaultCounter 

-- |
-- >>> (Value True === Value False) emptyS
-- []
-- >>> (Value False === Value False) emptyS
-- [fromList []]
-- >>> let [x, y] = Variable <$> ['x', 'y']
-- >>> (x === y) emptyS
-- [fromList [('x',Variable 'y')]]
-- >>> (y === x) emptyS
-- [fromList [('y',Variable 'x')]]
(===) :: (Eq a, Ord v) => Expr a v -> Expr a v -> Goal a v
u === v =
    \s -> maybeToList $ unify u v s

-- |
-- >>> success emptyS
-- [fromList []]
success :: Goal a v
success = pure

-- |
-- >>> failure emptyS
-- []
failure :: Goal a v
failure = \s -> []

-- |
-- >>> let [x, y] = Variable <$> ['x', 'y']
-- >>> (disj2 (Value "olive" === x) (Value "oil" === x)) emptyS
-- [fromList [('x',Value "olive")],fromList [('x',Value "oil")]]
disj2 :: Goal a v -> Goal a v -> Goal a v
disj2 g1 g2 =
    \s -> interleave (g1 s) (g2 s)
    where
        interleave :: [a] -> [a] -> [a]
        interleave (x:xs) y = x : interleave y xs
        interleave _ y = y

-- |
-- >>> nevero emptyS
-- []
-- >>> head $ (disj2 (Value "olive" === Variable 'x') nevero) emptyS
-- fromList [('x',Value "olive")]
-- >>> head $ (disj2 nevero (Value "olive" === Variable 'x')) emptyS
-- fromList [('x',Value "olive")]
nevero :: Goal a v
nevero = \s -> []

-- |
-- >>> head $ alwayso emptyS
-- fromList []
-- >>> take 3 $ alwayso emptyS
-- [fromList [],fromList [],fromList []]
alwayso :: Goal a v
alwayso = disj2 success alwayso

-- |
conj2 :: Goal a v -> Goal a v -> Goal a v
conj2 g1 g2 = concatMap g2 . g1

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
