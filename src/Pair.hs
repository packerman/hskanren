module Pair where

import MicroKanren
import MicroKanren.Functions
import MicroKanren.Testing

-- |
-- >>> :{
--        run $ do
--                  x <- newVar
--                  y <- newVar
--                  goal $ conde [
--                          [teacupo x, teacupo x],
--                          [Value Fail === x, teacupo y]]
--                  pure $ list [x, y]
-- :}
-- [Cons (Value Tea) (Cons (Reified 0) Nil),Cons (Value Fail) (Cons (Value Tea) Nil),Cons (Value Cup) (Cons (Reified 0) Nil),Cons (Value Fail) (Cons (Value Cup) Nil)]
teacupo :: Expr Symbol -> Goal Symbol
teacupo t = disj2 (Value Tea === t) (Value Cup === t)

-- |
-- >>> runWith $ \q -> goal =<< caro (values "acorn") q
-- [Value 'a']
-- >>> runWith $ \q -> goal =<< caro (values "acorn") (Value 'a')
-- [Reified 0]
-- >>> :{
--      runWith $ \r -> do
--                          x <- newVar
--                          y <- newVar
--                          goal =<< caro (list [r, y]) x
--                          goal $ (Value Pear) === x
-- :}
-- [Value Pear]
caro :: Eq a => Expr a -> Relation a
caro p a = do
            d <- newVar
            pure $ (Cons a d) === p
