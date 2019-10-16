module Pair where

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
caro :: Eq a => Expr a -> Relation a
caro p a = do
            d <- fresh
            pure $ (Cons a d) === p
