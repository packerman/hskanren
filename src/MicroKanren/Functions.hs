module MicroKanren.Functions where

import qualified Data.Map as M
import Data.Maybe
import Data.Function
import Data.Foldable
import Safe

import MicroKanren.Types
import MicroKanren.Pair
import MicroKanren.Testing

emptySubst = M.empty

-- |
-- >>> let [x, y] = testVars 2
-- >>> (ifte success (Value False === y) (Value True === y)) emptySubst
-- [fromList [(2,False)]]
-- >>> (ifte failure (Value False === y) (Value True === y)) emptySubst
-- [fromList [(2,True)]]
-- >>> (ifte (Value True === x) (Value False === y) (Value True === y)) emptySubst
-- [fromList [(1,True),(2,False)]]
-- >>> (ifte (disj2 (Value True === x) (Value False === x)) (Value False === y) (Value True === y)) emptySubst
-- [fromList [(1,True),(2,False)],fromList [(1,False),(2,False)]]
ifte :: Goal f a v -> Goal f a v -> Goal f a v -> Goal f a v
ifte g1 g2 g3 = \s -> case g1 s of
                        [] -> g3 s
                        s' -> concatMap g2 s'

-- |
-- >>> let [x, y] = testVars 2
-- >>> (ifte (once (disj2 (Value True === x) (Value False === x))) (Value False === y) (Value True === y)) emptySubst
-- [fromList [(1,True),(2,False)]]
once :: Goal f a v -> Goal f a v
once g = maybeToList . headMay . g

-- |
-- >>> let [x] = testVars 1
-- >>> map (reify x) (runGoal 5 (disj2 (Value "olive" === x) (Value "oil" === x)))
-- ["olive","oil"]
runGoal :: Int -> Goal f a v -> [Substitution f a v]
runGoal n g = take n $ g emptySubst

-- |
-- >>> let variables = testVars 6
-- >>> let [vu, vv, vw, vx, vy, vz] = variables
-- >>> let [Variable u, Variable v, Variable w, Variable x, Variable y, Variable z] = variables
-- >>> let [ice, corn] = Value <$> ["ice", "corn"]
-- >>> let s = M.fromList [(x, list [vu, vw, vy, vz, list [ice, vz]]), (y, corn), (w, list [vv, vu])]
-- >>> (reify vx) s
-- (_0 (_1 _0) "corn" _2 ("ice" _2))
reify :: (Foldable f, Functor f, Ord v) => Term f a v -> Substitution f a v -> Term f a v
reify v =
    \s -> let v' = walkMany v s
              r = reifySubst v' emptySubst
            in walkMany v' r

reifySubst :: (Foldable f, Ord v) => Term f a v -> Substitution f a v -> Substitution f a v
reifySubst v r = case walk v r of
                        Variable x -> M.insert x (Reified $ M.size r) r
                        Term ts -> foldl (\r' t -> reifySubst t r') r ts
                        _ -> r

-- |
-- >>> let [w, x, y, z] = [1..4]
-- >>> walkMany (Variable w) $ M.fromList [(x, Value 'b'), (z, Variable y), (w, list [Variable x, Value 'e', Variable z])]
-- ('b' 'e' var3)
walkMany :: (Functor f, Ord v) => Term f a v -> Substitution f a v -> Term f a v
walkMany v s = case walk v s of
                Term ts -> Term $ fmap (\t -> walkMany t s) ts
                x -> x


-- |
-- >>> (Value True === Value False) (emptySubst :: Substitution Pair Bool Integer)
-- []
-- >>> (Value False === Value False) (emptySubst :: Substitution Pair Bool Integer)
-- [fromList []]
-- >>> let [x, y] = testVars 2
-- >>> (x === y) emptySubst
-- [fromList [(1,var2)]]
-- >>> (y === x) emptySubst
-- [fromList [(2,var1)]]
(===) :: (Foldable f, Eq a, Ord v, Eq (f (Term f a v))) => Term f a v -> Term f a v -> Goal f a v
u === v =
    \s -> maybeToList $ unify u v s

-- |
-- >>> success (emptySubst :: Substitution Pair () Integer)
-- [fromList []]
success :: Goal f a v
success = pure

-- |
-- >>> failure (emptySubst :: Substitution Pair () Integer)
-- []
failure :: Goal f a v
failure = const []

-- |
-- >>> let [x, y] = testVars 2
-- >>> (disj2 (Value "olive" === x) (Value "oil" === x)) emptySubst
-- [fromList [(1,"olive")],fromList [(1,"oil")]]
disj2 :: Goal f a v -> Goal f a v -> Goal f a v
disj2 g1 g2 =
    \s -> interleave (g1 s) (g2 s)
    where
        interleave :: [a] -> [a] -> [a]
        interleave (x:xs) y = x : interleave y xs
        interleave _ y = y

-- |
conj2 :: Goal f a v -> Goal f a v -> Goal f a v
conj2 g1 g2 = concatMap g2 . g1

-- |
-- >>> let [x, y] = testVars 2
-- >>> let [a, e] = Value <$> ['a', 'e']
-- >>> unify x a emptySubst
-- Just (fromList [(1,'a')])
-- >>> unify a y emptySubst
-- Just (fromList [(2,'a')])
-- >>> unify (Term $ Cons x a) (Term $ Cons e y) emptySubst
-- Just (fromList [(1,'e'),(2,'a')])
-- >>> unify (Term $ Cons a x) (Term $ Cons e y) emptySubst
-- Nothing
unify :: (Eq a, Ord v, Foldable f, Eq (f (Term f a v))) => Term f a v -> Term f a v -> Substitution f a v -> Maybe (Substitution f a v)
unify u v s = 
    let u' = walk u s
        v' = walk v s in
            if u' == v' then Just s
            else case (u', v') of
                    (Variable x, _) -> extend x v' s
                    (_, Variable y) -> extend y u' s
                    (Term us, Term vs) -> unifySubTerms (toList us) (toList vs) s
                    _ -> Nothing
    where
        unifySubTerms us vs s = if not $ equating length us vs
                                then Nothing
                                else foldlM (\s' (u, v) -> unify u v s') s $ zip us vs
        equating f x y = f x == f y

-- |
-- >>> let [v, w, x, y, z] = [1..5]
-- >>> walk (Variable z :: Term Pair Char Integer) $ M.fromList [(z, Value 'a'), (x, Variable w), (y, Variable z)]
-- 'a'
-- >>> walk (Variable y :: Term Pair Char Integer) $ M.fromList [(z, Value 'a'), (x, Variable w), (y, Variable z)]
-- 'a'
-- >>> walk (Variable x :: Term Pair Char Integer) $ M.fromList [(z, Value 'a'), (x, Variable w), (y, Variable z)]
-- var2
-- >>> walk (Variable x :: Term Pair Char Integer) $ M.fromList [(x, Variable y), (v, Variable x), (w, Variable x)]
-- var4
-- >>> walk (Variable v :: Term Pair Char Integer) $ M.fromList [(x, Variable y), (v, Variable x), (w, Variable x)]
-- var4
-- >>> walk (Variable w :: Term Pair Char Integer) $ M.fromList [(x, Variable y), (v, Variable x), (w, Variable x)]
-- var4
-- >>> walk (Variable w) $ M.fromList [(x, Value 'b'), (z, Variable y), (w, list [Variable x, Value 'e', Variable z])]
-- (var3 'e' var5)
walk :: (Ord v) => Term f a v -> Substitution f a v -> Term f a v
walk v@(Variable x) s = case M.lookup x s of
                            Just e -> walk e s
                            _ -> v
walk e _ = e

-- |
-- >>> let [x, y, z] = [1..3]
-- >>> extend x (list [Variable x]) emptySubst
-- Nothing
-- >>> extend x (list [Variable y]) (M.fromList [(y, Variable x)])
-- Nothing
-- >>> :{ 
--    let s = M.fromList [(z, Variable x), (y, Variable z)] :: Substitution Pair a Integer
--        in walk (Variable y) <$> (extend x (Value 'e') s)
-- :}
-- Just 'e'
extend :: (Ord v, Foldable f) => v -> Term f a v -> Substitution f a v -> Maybe (Substitution f a v)
extend x v s = if occurs x v s 
                then Nothing
                else Just $ M.insert x v s

-- |
-- >>> let [x, y] = [1..2]
-- >>> occurs x (Variable x) emptySubst
-- True 
-- >>> occurs x (list [Variable y]) (M.fromList [(y, Variable x)])
-- True
occurs :: (Ord v, Foldable f) => v -> Term f a v -> Substitution f a v -> Bool
occurs x v s = case walk v s of
                Variable y -> y == x
                Term ts -> any (\t -> occurs x t s) ts
                _ -> False

-- |
-- >>> list [Value 1, Value 2, Value 3, Value 4]
-- (1 2 3 4)
list :: [Expr a] -> Expr a
list = foldr (\e -> Term . Cons e) (Term Nil)

values :: [a] -> Expr a
values = list . map Value
