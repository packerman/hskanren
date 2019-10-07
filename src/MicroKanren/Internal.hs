module MicroKanren.Internal where

data Counter = Counter Int deriving (Show)

-- | Return the current state of counter and increments
-- 
-- >>> getAndInc defaultCounter
-- (0,Counter 1)
-- >>> getAndInc (Counter 5)
-- (5,Counter 6)
-- >>> import Control.Monad.State
-- >>> :{
-- let m = do
--            a <- state getAndInc
--            b <- do
--                   state getAndInc 
--                   state getAndInc
--            c <- do
--                   state getAndInc
--            return $ a + b + c :: State Counter Int
-- in evalState m defaultCounter
-- :}
-- 5
getAndInc :: Counter -> (Int, Counter)
getAndInc (Counter n) = (n, Counter $ n + 1)

defaultCounter :: Counter
defaultCounter = Counter 0