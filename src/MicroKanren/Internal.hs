module MicroKanren.Internal where

data Counter = Counter Integer deriving (Eq, Show)

getAndInc :: Counter -> (Integer, Counter)
getAndInc (Counter n) = (n, Counter $ n + 1)

defaultCounter :: Counter
defaultCounter = Counter 0
