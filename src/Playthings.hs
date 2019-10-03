module Playthings where

import MicroKanren
import MicroKanren.Functions
import MicroKanren.Testing

-- |
-- >>> :{
--        run $ do
--                  x <- var X
--                  y <- var Y
--                  goal $ conde [
--                          [teacup x, teacup x],
--                          [Value Fail === x, teacup y]]
--                  pure $ list [x, y]
-- :}
-- [Cons (Value Tea) (Cons (Reified 0) Nil),Cons (Value Fail) (Cons (Value Tea) Nil),Cons (Value Cup) (Cons (Reified 0) Nil),Cons (Value Fail) (Cons (Value Cup) Nil)]
teacup :: Ord v => Relation Symbol v
teacup t = disj2 (Value Tea === t) (Value Cup === t)

