{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module MicroKanren.Types where

import qualified Data.Map as M

type Var = Integer

data Term f = Variable Var |
                Reified Int |
                Term (f (Term f))

deriving instance (Eq (f (Term f))) => Eq (Term f)

instance (Show (f (Term f))) => Show (Term f) where
    show = showTerm

data Pair a = Value a |
              Nil |
              Cons (Pair a) (Pair a)
              deriving (Eq)

instance Show a => Show (Pair a) where
    show e = showPair e True

type Substitution f = M.Map Var (Term f)

type Goal f = Substitution f -> [Substitution f]

-- >>> showTerm (Variable 3) True
-- "var3"
-- >>> showTerm (Reified 1) True
-- "_1"
showTerm :: (Show (f (Term f))) => Term f -> String
showTerm (Variable v) = "var" ++ show v
showTerm (Reified n) = '_':show n
showTerm (Term t) = show t

-- |
-- >>> isList Nil
-- True
-- >>> isList (Cons (Value 2) (Cons (Value 3) Nil))
-- True
-- >>> isList (Cons (Value 2) (Value 3))
-- False
-- >>> isList (Value 5)
-- False
-- >>> isList (Variable 2)
-- False
-- >>> isList (Reified 0)
-- False
isList :: Pair a -> Bool
isList Nil = True
isList (Cons a d) = isList d
isList _ = False

-- |
-- >>> showPair (Value 5) True
-- "5"
-- >>> showPair Nil True
-- "()"
-- >>> showPair (Cons (Value 5) (Value 3)) True
-- "(5 . 3)"
-- >>> showPair (Cons (Value 2) Nil) True
-- "(2)"
-- >>> showPair (Cons (Value 1) (Cons (Value 2) Nil)) True
-- "(1 2)"
-- >>> showPair (Cons (Cons (Value 1) (Cons (Value 2) Nil)) (Cons (Value 3) (Cons (Value 4) Nil))) True
-- "((1 2) 3 4)"
showPair :: Show a => Pair a -> Bool -> String
showPair (Value x) _ = show x
showPair Nil _ = "()"
showPair (Cons a Nil) showParenths = parenths showParenths $ showPair a True
showPair (Cons a d) showParenths = parenths showParenths $ showPair a True ++ 
                                                            (if isList d then " " else " . ") ++ 
                                                            showPair d False
    
parenths True s = "(" ++ s ++ ")"
parenths False s = s
