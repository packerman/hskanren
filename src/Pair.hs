module Pair where

import Data.Functor

import MicroKanren
import MicroKanren.Functions
import MicroKanren.Testing

-- |
-- >>> runWith $ \q -> caro (values "acorn") q
-- [Value 'a']
-- >>> runWith $ \q -> caro (values "acorn") (Value 'a')
-- [Reified 0]
-- >>> :{
--      runWith $ \r -> do
--                          x <- fresh
--                          y <- fresh
--                          conjM [caro (list [r, y]) x, 
--                                              pure $ (Value Pear) === x]
-- :}
-- [Value Pear]
--
-- >>> :{
--      runWith $ \r -> do
--                          x <- fresh
--                          y <- fresh
--                          conjM [caro (values [Grape, Raisin, Pear]) x,
--                                  caro (list [list [Value A], list [Value B], list [Value C]]) y,
--                                  pure $ (Cons x y) === r]
-- :}
-- [Cons (Value Grape) (Cons (Value A) Nil)]
caro :: Eq a => Expr a -> RelationM a
caro p a = fresh <&> (\d -> Cons a d === p)

-- |
-- >>> :{
--      runWith $ \r -> 
--                 fresh >>= (\v -> 
--                             conjM [cdro (values "acorn") v,
--                                    fresh >>= (\w -> 
--                                               conjM [cdro v w,
--                                               caro w r])])
-- :}
-- [Value 'o']
--
-- >>> :{
--      runWith $ \r -> do
--                          x <- fresh
--                          y <- fresh
--                          conjM [cdro (values [Grape, Raisin, Pear]) x,
--                                  caro (list [list [Value A], list [Value B], list [Value C]]) y,
--                                  pure $ (Cons x y) === r]
-- :}
-- [Cons (Cons (Value Raisin) (Cons (Value Pear) Nil)) (Cons (Value A) Nil)]
--
-- >>> runWith $ \x -> cdro (values "corn") (list [x, Value 'r', Value 'n'])
-- [Value 'o']
--
-- >>> :{
--      runWith $ \l -> fresh>>= (\x -> 
--                                  conjM [cdro l (values "corn"),
--                                          caro l x,
--                                          pure $ (Value 'a') === x])
-- :}
-- [Cons (Value 'a') (Cons (Value 'c') (Cons (Value 'o') (Cons (Value 'r') (Cons (Value 'n') Nil))))]
cdro :: Eq a => Expr a -> RelationM a
cdro p d = fresh <&> (\a -> Cons a d === p)

-- |
-- >>> runWith $ (\l -> conso (values "abc") (values "de") l)
-- [Cons (Cons (Value 'a') (Cons (Value 'b') (Cons (Value 'c') Nil))) (Cons (Value 'd') (Cons (Value 'e') Nil))]
-- >>> runWith $ (\x -> conso x (values "abc") (values "dabc"))
-- [Value 'd']
-- >>> :{
--      runWith $ \r -> do
--                          x <- fresh
--                          y <- fresh
--                          z <- fresh
--                          conjM [pure $ list [Value 'e', Value 'a', Value 'd', x] === r,
--                                 conso y (list [Value 'a', z, Value 'c']) r]
-- :}
-- [Cons (Value 'e') (Cons (Value 'a') (Cons (Value 'd') (Cons (Value 'c') Nil)))]
--
-- >>> runWith $ (\x -> conso x (list [Value 'a', x, Value 'c']) (list [Value 'd', Value 'a', x, Value 'c']))
-- [Value 'd']
-- >>> :{
--      runWith $ \l -> fresh >>= (\x ->
--                          conjM [ pure $ list [Value 'd', Value 'a', x, Value 'c'] === l,
--                                  conso x (list [Value 'a', x, Value 'c']) l])
-- :}
-- [Cons (Value 'd') (Cons (Value 'a') (Cons (Value 'd') (Cons (Value 'c') Nil)))]
conso :: Eq a => Expr a -> Expr a -> RelationM a
conso a d p = conj <$> sequence [caro p a, cdro p d]

-- |
-- >>> runWith (\_ -> pure $ nullo $ values [Grape, Raisin, Pear])
-- []
-- >>> runWith (\_ -> pure $ nullo Nil)
-- [Reified 0]
-- >>> runWith (\x -> pure $ nullo x)
-- [Nil]
nullo :: Eq a => Relation a
nullo x = x === Nil

-- |
-- >>> :{
-- run $ do
--         x <- fresh
--         y <- fresh
--         pure (list [x, y],
--                  appendo x y $ values "a")
-- :}
-- [Cons Nil (Cons (Cons (Value 'a') Nil) Nil),Cons (Cons (Value 'a') Nil) (Cons Nil Nil)]
appendo :: Eq a => Expr a -> Expr a -> RelationM a
appendo l t out = disjM [
                        pure $ conj [nullo l, t === out],
                        do
                            a <- fresh
                            d <- fresh
                            res <- fresh
                            conjM [
                                conso a d l,
                                conso a res out,
                                appendo d t res
                                ]
                    ]
