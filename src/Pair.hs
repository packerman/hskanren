module Pair where

import Data.Functor

import MicroKanren
import MicroKanren.Functions
import MicroKanren.Testing

-- |
-- >>> runWith $ \q -> goal =<< caro (values "acorn") q
-- [Value 'a']
-- >>> runWith $ \q -> goal =<< caro (values "acorn") (Value 'a')
-- [Reified 0]
-- >>> :{
--      runWith $ \r -> do
--                          x <- fresh
--                          y <- fresh
--                          goal =<< caro (list [r, y]) x
--                          goal $ (Value Pear) === x
-- :}
-- [Value Pear]
--
-- >>> :{
--      runWith $ \r -> do
--                          x <- fresh
--                          y <- fresh
--                          goal =<< caro (values [Grape, Raisin, Pear]) x
--                          goal =<< caro (list [list [Value A], list [Value B], list [Value C]]) y
--                          goal $ (Cons x y) === r
-- :}
-- [Cons (Value Grape) (Cons (Value A) Nil)]
caro :: Eq a => Expr a -> RelationM a
caro p a = fresh <&> (\d -> Cons a d === p)

-- |
-- >>> :{
--      runWith $ \r -> do
--                          v <- fresh
--                          goal =<< cdro (values "acorn") v
--                          w <- fresh
--                          goal =<< cdro v w
--                          goal =<< caro w r
-- :}
-- [Value 'o']
--
-- >>> :{
--      runWith $ \r -> do
--                          x <- fresh
--                          y <- fresh
--                          goal =<< cdro (values [Grape, Raisin, Pear]) x
--                          goal =<< caro (list [list [Value A], list [Value B], list [Value C]]) y
--                          goal $ (Cons x y) === r
-- :}
-- [Cons (Cons (Value Raisin) (Cons (Value Pear) Nil)) (Cons (Value A) Nil)]
--
-- >>> runWith $ \x -> goal =<< cdro (values "corn") (list [x, Value 'r', Value 'n'])
-- [Value 'o']
--
-- >>> :{
--      runWith $ \l -> do
--                          x <- fresh
--                          goal =<< cdro l (values "corn")
--                          goal =<< caro l x
--                          goal $ (Value 'a') === x
-- :}
-- [Cons (Value 'a') (Cons (Value 'c') (Cons (Value 'o') (Cons (Value 'r') (Cons (Value 'n') Nil))))]
cdro :: Eq a => Expr a -> RelationM a
cdro p d = fresh <&> (\a -> Cons a d === p)

-- |
-- >>> runWith $ (\l -> goal =<< conso (values "abc") (values "de") l)
-- [Cons (Cons (Value 'a') (Cons (Value 'b') (Cons (Value 'c') Nil))) (Cons (Value 'd') (Cons (Value 'e') Nil))]
-- >>> runWith $ (\x -> goal =<< conso x (values "abc") (values "dabc"))
-- [Value 'd']
conso :: Eq a => Expr a -> Expr a -> RelationM a
conso a d p = (goal =<< caro p a) >> (cdro p d) 
