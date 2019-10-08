module Pair where

import MicroKanren
import MicroKanren.Functions
import MicroKanren.Testing

-- |
-- >>> :{
--        run $ do
--                  x <- var X
--                  y <- var Y
--                  goal $ conde [
--                          [teacupo x, teacupo x],
--                          [Value Fail === x, teacupo y]]
--                  pure $ list [x, y]
-- :}
-- [Cons (Value Tea) (Cons (Reified 0) Nil),Cons (Value Fail) (Cons (Value Tea) Nil),Cons (Value Cup) (Cons (Reified 0) Nil),Cons (Value Fail) (Cons (Value Cup) Nil)]
teacupo :: Ord v => Relation Symbol v
teacupo t = disj2 (Value Tea === t) (Value Cup === t)

-- |
-- >> runWith Q $ \q -> caro (values "acorn") q
-- ['a']
-- >> runWith Q $ \q -> caro (values "acorn") 'a'
-- [Reified 0]
-- >> :{
--      runWith R $ \r -> do
--                          x <- var X
--                          y <- var Y
--                          goal $ caro (list [r, y]) x
--                          goal $ (Value Pear) x
-- :}
-- [Value Pear]
-- caro :: Expr a v -> Relation a v
-- caro p a = do
--             d <- var 'd'
--             goal $ (Cons a d) === p
