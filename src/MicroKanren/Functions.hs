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

ifte :: Goal f a v -> Goal f a v -> Goal f a v -> Goal f a v
ifte g1 g2 g3 = \s -> case g1 s of
                        [] -> g3 s
                        s' -> concatMap g2 s'

once :: Goal f a v -> Goal f a v
once g = maybeToList . headMay . g

runGoal :: Int -> Goal f a v -> [Substitution f a v]
runGoal n g = take n $ g emptySubst

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


walkMany :: (Functor f, Ord v) => Term f a v -> Substitution f a v -> Term f a v
walkMany v s = case walk v s of
                Term ts -> Term $ fmap (\t -> walkMany t s) ts
                x -> x

(===) :: (Foldable f, Eq a, Ord v, Eq (f (Term f a v))) => Term f a v -> Term f a v -> Goal f a v
u === v =
    \s -> maybeToList $ unify u v s

success :: Goal f a v
success = pure

failure :: Goal f a v
failure = const []

disj2 :: Goal f a v -> Goal f a v -> Goal f a v
disj2 g1 g2 =
    \s -> interleave (g1 s) (g2 s)
    where
        interleave :: [a] -> [a] -> [a]
        interleave (x:xs) y = x : interleave y xs
        interleave _ y = y

conj2 :: Goal f a v -> Goal f a v -> Goal f a v
conj2 g1 g2 = concatMap g2 . g1

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

walk :: (Ord v) => Term f a v -> Substitution f a v -> Term f a v
walk v@(Variable x) s = case M.lookup x s of
                            Just e -> walk e s
                            _ -> v
walk e _ = e

extend :: (Ord v, Foldable f) => v -> Term f a v -> Substitution f a v -> Maybe (Substitution f a v)
extend x v s = if occurs x v s 
                then Nothing
                else Just $ M.insert x v s

occurs :: (Ord v, Foldable f) => v -> Term f a v -> Substitution f a v -> Bool
occurs x v s = case walk v s of
                Variable y -> y == x
                Term ts -> any (\t -> occurs x t s) ts
                _ -> False

list :: [Expr a] -> Expr a
list = foldr (\e -> Term . Cons e) (Term Nil)

values :: [a] -> Expr a
values = list . map Value
