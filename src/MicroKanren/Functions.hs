module MicroKanren.Functions where

import qualified Data.Map as M
import Data.Maybe
import Data.Function
import Safe

import MicroKanren.Types
import MicroKanren.Testing

emptySubst = M.empty

-- |
-- >>> let [x, y] = testVars ['x', 'y']
-- >>> (ifte success (Value False === y) (Value True === y)) emptySubst
-- [fromList [((1,'y'),Value False)]]
-- >>> (ifte failure (Value False === y) (Value True === y)) emptySubst
-- [fromList [((1,'y'),Value True)]]
-- >>> (ifte (Value True === x) (Value False === y) (Value True === y)) emptySubst
-- [fromList [((0,'x'),Value True),((1,'y'),Value False)]]
-- >>> (ifte (disj2 (Value True === x) (Value False === x)) (Value False === y) (Value True === y)) emptySubst
-- [fromList [((0,'x'),Value True),((1,'y'),Value False)],fromList [((0,'x'),Value False),((1,'y'),Value False)]]
ifte :: Goal a v -> Goal a v -> Goal a v -> Goal a v
ifte g1 g2 g3 = \s -> case g1 s of
                        [] -> g3 s
                        s' -> concatMap g2 s'

-- |
-- >>> let [x, y] = testVars ['x', 'y']
-- >>> (ifte (once (disj2 (Value True === x) (Value False === x))) (Value False === y) (Value True === y)) emptySubst
-- [fromList [((0,'x'),Value True),((1,'y'),Value False)]]
once :: Goal a v -> Goal a v
once g = maybeToList . headMay . g

-- |
-- >>> let [x] = testVars ['x']
-- >>> map (reify x) (runGoal 5 (disj2 (Value "olive" === x) (Value "oil" === x)))
-- [Value "olive",Value "oil"]
runGoal :: Int -> Goal a v -> [Substitution a v]
runGoal n g = take n $ g emptySubst

-- |
-- >>> let names = pure <$> "uvwxyz" :: [String]
-- >>> let variables = testVars names
-- >>> let [vu, vv, vw, vx, vy, vz] = variables
-- >>> let [Variable u, Variable v, Variable w, Variable x, Variable y, Variable z] = variables
-- >>> let [ice, corn] = Value <$> ["ice", "corn"]
-- >>> let s = M.fromList [(x, list [vu, vw, vy, vz, list [ice, vz]]), (y, corn), (w, list [vv, vu])]
-- >>> (reify vx) s
-- Cons (Reified 0) (Cons (Cons (Reified 1) (Cons (Reified 0) Nil)) (Cons (Value "corn") (Cons (Reified 2) (Cons (Cons (Value "ice") (Cons (Reified 2) Nil)) Nil))))
reify :: Ord v => Expr a v -> Substitution a v -> Expr a v
reify v =
    \s -> let v' = walkMany v s
              r = reifySubst v' emptySubst
            in walkMany v' r

reifySubst :: Ord v => Expr a v -> Substitution a v -> Substitution a v
reifySubst v r = case walk v r of
                        Variable x -> M.insert x (Reified $ M.size r) r
                        Cons a d -> (reifySubst a r) & (reifySubst d)
                        _ -> r

-- |
-- >>> let [w, x, y, z] = indexed ['w'..'z']
-- >>> walkMany (Variable w) $ M.fromList [(x, Value 'b'), (z, Variable y), (w, list [Variable x, Value 'e', Variable z])]
-- Cons (Value 'b') (Cons (Value 'e') (Cons (Variable (2,'y')) Nil))
walkMany :: Ord v => Expr a v -> Substitution a v -> Expr a v
walkMany v s = case walk v s of
                Cons a d -> Cons (walkMany a s) (walkMany d s)
                x -> x


-- |
-- >>> (Value True === Value False) emptySubst
-- []
-- >>> (Value False === Value False) emptySubst
-- [fromList []]
-- >>> let [x, y] = testVars ['x', 'y']
-- >>> (x === y) emptySubst
-- [fromList [((0,'x'),Variable (1,'y'))]]
-- >>> (y === x) emptySubst
-- [fromList [((1,'y'),Variable (0,'x'))]]
(===) :: (Eq a, Ord v) => Expr a v -> Expr a v -> Goal a v
u === v =
    \s -> maybeToList $ unify u v s

-- |
-- >>> success emptySubst
-- [fromList []]
success :: Goal a v
success = pure

-- |
-- >>> failure emptySubst
-- []
failure :: Goal a v
failure = const []

-- |
-- >>> let [x, y] = testVars ['x', 'y']
-- >>> (disj2 (Value "olive" === x) (Value "oil" === x)) emptySubst
-- [fromList [((0,'x'),Value "olive")],fromList [((0,'x'),Value "oil")]]
disj2 :: Goal a v -> Goal a v -> Goal a v
disj2 g1 g2 =
    \s -> interleave (g1 s) (g2 s)
    where
        interleave :: [a] -> [a] -> [a]
        interleave (x:xs) y = x : interleave y xs
        interleave _ y = y

-- |
conj2 :: Goal a v -> Goal a v -> Goal a v
conj2 g1 g2 = concatMap g2 . g1

-- |
-- >>> let [x, y] = testVars ['x', 'y']
-- >>> let [a, e] = Value <$> ['a', 'e']
-- >>> unify x a emptySubst
-- Just (fromList [((0,'x'),Value 'a')])
-- >>> unify a y emptySubst
-- Just (fromList [((1,'y'),Value 'a')])
-- >>> unify (Cons x a) (Cons e y) emptySubst
-- Just (fromList [((0,'x'),Value 'e'),((1,'y'),Value 'a')])
-- >>> unify (Cons a x) (Cons e y) emptySubst
-- Nothing
unify :: (Eq a, Ord v) => Expr a v -> Expr a v -> Substitution a v -> Maybe (Substitution a v)
unify u v s = 
    let u' = walk u s
        v' = walk v s in
            if u' == v' then Just s
            else case (u', v') of
                    (Variable x, _) -> extend x v' s
                    (_, Variable y) -> extend y u' s
                    (Cons ua ud, Cons va vd) -> (unify ua va s) >>= (unify ud vd)
                    _ -> Nothing

-- |
-- >>> let [v, w, x, y, z] = indexed ['v'..'z']
-- >>> walk (Variable z) $ M.fromList [(z, Value 'a'), (x, Variable w), (y, Variable z)]
-- Value 'a'
-- >>> walk (Variable y) $ M.fromList [(z, Value 'a'), (x, Variable w), (y, Variable z)]
-- Value 'a'
-- >>> walk (Variable x) $ M.fromList [(z, Value 'a'), (x, Variable w), (y, Variable z)]
-- Variable (1,'w')
-- >>> walk (Variable x) $ M.fromList [(x, Variable y), (v, Variable x), (w, Variable x)]
-- Variable (3,'y')
-- >>> walk (Variable v) $ M.fromList [(x, Variable y), (v, Variable x), (w, Variable x)]
-- Variable (3,'y')
-- >>> walk (Variable w) $ M.fromList [(x, Variable y), (v, Variable x), (w, Variable x)]
-- Variable (3,'y')
-- >>> walk (Variable w) $ M.fromList [(x, Value 'b'), (z, Variable y), (w, list [Variable x, Value 'e', Variable z])]
-- Cons (Variable (2,'x')) (Cons (Value 'e') (Cons (Variable (4,'z')) Nil))
walk :: Ord v => Expr a v -> Substitution a v -> Expr a v
walk v@(Variable x) s = case M.lookup x s of
                            Just e -> walk e s
                            _ -> v
walk e _ = e

-- |
-- >>> let [x, y, z] = indexed ['x'..'z']
-- >>> extend x (list [Variable x]) emptySubst
-- Nothing
-- >>> extend x (list [Variable y]) (M.fromList [(y, Variable x)])
-- Nothing
-- >>> :{ 
--    let s = M.fromList [(z, Variable x), (y, Variable z)]
--        in walk (Variable y) <$> (extend x (Value 'e') s)
-- :}
-- Just (Value 'e')
extend :: Ord v => Var v -> Expr a v -> Substitution a v -> Maybe (Substitution a v)
extend x v s = if occurs x v s 
                then Nothing
                else Just $ M.insert x v s

-- |
-- >>> let [x, y] = indexed ['x', 'y']
-- >>> occurs x (Variable x) emptySubst
-- True 
-- >>> occurs x (list [Variable y]) (M.fromList [(y, Variable x)])
-- True
occurs :: Ord v => Var v -> (Expr a v) -> Substitution a v -> Bool
occurs x v s = case walk v s of
                Variable y -> y == x
                Cons a d -> occurs x a s || occurs x d s
                _ -> False

-- |
-- >>> list [Value 1, Value 2, Value 3, Value 4]
-- Cons (Value 1) (Cons (Value 2) (Cons (Value 3) (Cons (Value 4) Nil)))
list :: [Expr a v] -> Expr a v
list = foldr Cons Nil

