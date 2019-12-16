module Pair where

import Data.Functor

import MicroKanren
import MicroKanren.Functions
import MicroKanren.Testing

-- |
-- >>> runWith $ \q -> caro (values "acorn") q
-- ['a']
-- >>> runWith $ \q -> caro (values "acorn") (Value 'a')
-- [_0]
-- >>> :{
--      runWith $ \r -> do
--                          x <- fresh
--                          y <- fresh
--                          conjM [caro (list [r, y]) x, 
--                                              pure $ (Value Pear) === x]
-- :}
-- [Pear]
--
-- >>> :{
--      runWith $ \r -> do
--                          x <- fresh
--                          y <- fresh
--                          conjM [caro (values [Grape, Raisin, Pear]) x,
--                                  caro (list [list [Value A], list [Value B], list [Value C]]) y,
--                                  pure $ (Cons x y) === r]
-- :}
-- [(Grape A)]
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
-- ['o']
--
-- >>> :{
--      runWith $ \r -> do
--                          x <- fresh
--                          y <- fresh
--                          conjM [cdro (values [Grape, Raisin, Pear]) x,
--                                  caro (list [list [Value A], list [Value B], list [Value C]]) y,
--                                  pure $ (Cons x y) === r]
-- :}
-- [((Raisin Pear) A)]
--
-- >>> runWith $ \x -> cdro (values "corn") (list [x, Value 'r', Value 'n'])
-- ['o']
--
-- >>> :{
--      runWith $ \l -> fresh>>= (\x -> 
--                                  conjM [cdro l (values "corn"),
--                                          caro l x,
--                                          pure $ (Value 'a') === x])
-- :}
-- [('a' 'c' 'o' 'r' 'n')]
cdro :: Eq a => Expr a -> RelationM a
cdro p d = fresh <&> (\a -> Cons a d === p)

-- |
-- >>> runWith $ (\l -> conso (values "abc") (values "de") l)
-- [(('a' 'b' 'c') 'd' 'e')]
-- >>> runWith $ (\x -> conso x (values "abc") (values "dabc"))
-- ['d']
-- >>> :{
--      runWith $ \r -> do
--                          x <- fresh
--                          y <- fresh
--                          z <- fresh
--                          conjM [pure $ list [Value 'e', Value 'a', Value 'd', x] === r,
--                                 conso y (list [Value 'a', z, Value 'c']) r]
-- :}
-- [('e' 'a' 'd' 'c')]
--
-- >>> runWith $ (\x -> conso x (list [Value 'a', x, Value 'c']) (list [Value 'd', Value 'a', x, Value 'c']))
-- ['d']
-- >>> :{
--      runWith $ \l -> fresh >>= (\x ->
--                          conjM [ pure $ list [Value 'd', Value 'a', x, Value 'c'] === l,
--                                  conso x (list [Value 'a', x, Value 'c']) l])
-- :}
-- [('d' 'a' 'd' 'c')]
conso :: Eq a => Expr a -> Expr a -> RelationM a
conso a d p = conj <$> sequence [caro p a, cdro p d]

-- |
-- >>> runWith (\_ -> pure $ nullo $ values [Grape, Raisin, Pear])
-- []
-- >>> runWith (\_ -> pure $ nullo Nil)
-- [_0]
-- >>> runWith (\x -> pure $ nullo x)
-- [()]
nullo :: Eq a => Relation a
nullo x = x === Nil

-- |
-- >>> :{
-- run $ do
--         x <- fresh
--         y <- fresh
--         pure (list [x, y],
--                  appendo x y $ values "abcde")
-- :}
-- [(() ('a' 'b' 'c' 'd' 'e')),(('a') ('b' 'c' 'd' 'e')),(('a' 'b') ('c' 'd' 'e')),(('a' 'b' 'c') ('d' 'e')),(('a' 'b' 'c' 'd') ('e')),(('a' 'b' 'c' 'd' 'e') ())]
appendo :: Eq a => Expr a -> Expr a -> RelationM a
appendo l t out = disjM [
                        pure $ conj [nullo l, t === out],
                        fresh >>= (\a -> 
                            fresh >>= (\d -> 
                                fresh >>= (\res -> conjM [
                                                    conso a d l,
                                                    conso a res out,
                                                    appendo d t res]
                                            )
                                        )
                                    )
                        ]
