module MicroKanren.Functions where

import qualified Data.Map as M
import Data.Maybe
import Data.Function
import Safe

import MicroKanren.Types
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
ifte :: Goal f -> Goal f -> Goal f -> Goal f
ifte g1 g2 g3 = \s -> case g1 s of
                        [] -> g3 s
                        s' -> concatMap g2 s'

-- |
-- >>> let [x, y] = testVars 2
-- >>> (ifte (once (disj2 (Value True === x) (Value False === x))) (Value False === y) (Value True === y)) emptySubst
-- [fromList [(1,True),(2,False)]]
once :: Goal f -> Goal f
once g = maybeToList . headMay . g

-- |
-- >>> let [x] = testVars 1
-- >>> map (reify x) (runGoal 5 (disj2 (Value "olive" === x) (Value "oil" === x)))
-- ["olive","oil"]
runGoal :: Int -> Goal f -> [Substitution f]
runGoal n g = take n $ g emptySubst

-- |
-- >>> let variables = testVars 6
-- >>> let [vu, vv, vw, vx, vy, vz] = variables
-- >>> let [Variable u, Variable v, Variable w, Variable x, Variable y, Variable z] = variables
-- >>> let [ice, corn] = Value <$> ["ice", "corn"]
-- >>> let s = M.fromList [(x, list [vu, vw, vy, vz, list [ice, vz]]), (y, corn), (w, list [vv, vu])]
-- >>> (reify vx) s
-- (_0 (_1 _0) "corn" _2 ("ice" _2))
reify :: (Unifiable f) => Term f -> Substitution f -> Term f
reify v =
    \s -> let v' = walkMany v s
              r = reifySubst v' emptySubst
            in walkMany v' r

class Unifiable f where
    genericReifySubst :: f (Term f) -> Substitution f -> Substitution f

    genericWalkMany :: f (Term f) -> Substitution f -> f (Term f)

    genericUnify :: f (Term f) -> f (Term f) -> Substitution f -> Maybe (Substitution f)

    genericOccurs :: Var -> f (Term f) -> Substitution f -> Bool

instance Unifiable Pair where
    genericReifySubst (Cons a d) r = (genericReifySubst a r) & (genericReifySubst d)
    genericReifySubst _ r = r

    genericWalkMany (Cons a d) s = Cons (genericWalkMany a s) (genericWalkMany d s) 
    genericWalkMany x _ = x

    genericUnify (Cons ua ud) (Cons va vd) s = (genericUnify ua va s) >>= (genericUnify ud vd)
    genericUnify u v s = if u == v then Just s else Nothing

    genericOccurs x (Cons a d) s = genericOccurs x a s || genericOccurs x d s
    genericOccurs _ _ _ = False

    

reifySubst :: (Unifiable f) => Term f -> Substitution f -> Substitution f
reifySubst v r = case walk v r of
                        Variable x -> M.insert x (Reified $ M.size r) r
                        Term t -> genericReifySubst t r
                        _ -> r

-- |
-- >>> let [w, x, y, z] = [1..4]
-- >>> walkMany (Variable w) $ M.fromList [(x, Value 'b'), (z, Variable y), (w, list [Variable x, Value 'e', Variable z])]
-- ('b' 'e' var3)
walkMany :: (Unifiable f) => Term f -> Substitution f -> Term f
walkMany v s = case walk v s of
                Term t -> Term $ genericWalkMany t s
                x -> x


-- |
-- >>> (Value True === Value False) emptySubst
-- []
-- >>> (Value False === Value False) emptySubst
-- [fromList []]
-- >>> let [x, y] = testVars 2
-- >>> (x === y) emptySubst
-- [fromList [(1,var2)]]
-- >>> (y === x) emptySubst
-- [fromList [(2,var1)]]
(===) :: (Eq (f (Term f)), Unifiable f) => Term f -> Term f -> Goal f
u === v =
    \s -> maybeToList $ unify u v s

-- |
-- >>> success emptySubst
-- [fromList []]
success :: Goal f
success = pure

-- |
-- >>> failure emptySubst
-- []
failure :: Goal f
failure = const []

-- |
-- >>> let [x, y] = testVars 2
-- >>> (disj2 (Value "olive" === x) (Value "oil" === x)) emptySubst
-- [fromList [(1,"olive")],fromList [(1,"oil")]]
disj2 :: Goal f -> Goal f -> Goal f
disj2 g1 g2 =
    \s -> interleave (g1 s) (g2 s)
    where
        interleave :: [a] -> [a] -> [a]
        interleave (x:xs) y = x : interleave y xs
        interleave _ y = y

-- |
conj2 :: Goal f -> Goal f -> Goal f
conj2 g1 g2 = concatMap g2 . g1

-- |
-- >>> let [x, y] = testVars 2
-- >>> let [a, e] = Value <$> ['a', 'e']
-- >>> unify x a emptySubst
-- Just (fromList [(1,'a')])
-- >>> unify a y emptySubst
-- Just (fromList [(2,'a')])
-- >>> unify (Cons x a) (Cons e y) emptySubst
-- Just (fromList [(1,'e'),(2,'a')])
-- >>> unify (Cons a x) (Cons e y) emptySubst
-- Nothing
unify :: (Eq (f (Term f)), Unifiable f) => Term f -> Term f -> Substitution f -> Maybe (Substitution f)
unify u v s = 
    let u' = walk u s
        v' = walk v s in
            if u' == v' then Just s
            else case (u', v') of
                    (Variable x, _) -> extend x v' s
                    (_, Variable y) -> extend y u' s
                    (Term tu, Term tv) -> genericUnify tu tv s
                    _ -> Nothing

-- |
-- >>> let [v, w, x, y, z] = [1..5]
-- >>> walk (Variable z) $ M.fromList [(z, Value 'a'), (x, Variable w), (y, Variable z)]
-- 'a'
-- >>> walk (Variable y) $ M.fromList [(z, Value 'a'), (x, Variable w), (y, Variable z)]
-- 'a'
-- >>> walk (Variable x) $ M.fromList [(z, Value 'a'), (x, Variable w), (y, Variable z)]
-- var2
-- >>> walk (Variable x) $ M.fromList [(x, Variable y), (v, Variable x), (w, Variable x)]
-- var4
-- >>> walk (Variable v) $ M.fromList [(x, Variable y), (v, Variable x), (w, Variable x)]
-- var4
-- >>> walk (Variable w) $ M.fromList [(x, Variable y), (v, Variable x), (w, Variable x)]
-- var4
-- >>> walk (Variable w) $ M.fromList [(x, Value 'b'), (z, Variable y), (w, list [Variable x, Value 'e', Variable z])]
-- (var3 'e' var5)
walk :: Term f -> Substitution f -> Term f
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
--    let s = M.fromList [(z, Variable x), (y, Variable z)]
--        in walk (Variable y) <$> (extend x (Value 'e') s)
-- :}
-- Just 'e'
extend :: (Unifiable f) => Var -> Term f -> Substitution f -> Maybe (Substitution f)
extend x v s = if occurs x v s 
                then Nothing
                else Just $ M.insert x v s

-- |
-- >>> let [x, y] = [1..2]
-- >>> occurs x (Variable x) emptySubst
-- True 
-- >>> occurs x (list [Variable y]) (M.fromList [(y, Variable x)])
-- True
occurs :: (Unifiable f) => Var -> (Term f) -> Substitution f -> Bool
occurs x v s = case walk v s of
                Variable y -> y == x
                Term t -> genericOccurs x t s
                _ -> False

-- |
-- >>> list [Value 1, Value 2, Value 3, Value 4]
-- (1 2 3 4)
list :: [Pair a] -> Pair a
list = foldr Cons Nil

values :: [a] -> Pair a
values = list . map Value
