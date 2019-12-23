{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module MicroKanren.Types where

import qualified Data.Map as M

import MicroKanren.Pair

data Term f a v = Variable v |
                  Reified Int |
                  Value a |
                  Term (f (Term f a v))

deriving instance (Eq v, Eq a, Eq (f (Term f a v))) => Eq (Term f a v)

type Expr a = Term Pair a Integer

instance Show a => Show (Expr a) where
    show e = showExpr e True

type Substitution f a v = M.Map v (Term f a v)

type Goal f a v = Substitution f a v -> [Substitution f a v]

-- |
-- >>> isList (Term Nil)
-- True
-- >>> isList (Term $ Cons (Value 2) (Term $ Cons (Value 3) (Term Nil)))
-- True
-- >>> isList (Term $ Cons (Value 2) (Value 3))
-- False
-- >>> isList (Value 5)
-- False
-- >>> isList (Variable 2)
-- False
-- >>> isList (Reified 0)
-- False
isList :: Expr a -> Bool
isList (Term Nil) = True
isList (Term (Cons a d)) = isList d
isList _ = False

-- |
-- >>> showExpr (Value 5) True
-- "5"
-- >>> showExpr (Variable 3) True
-- "var3"
-- >>> showExpr (Reified 1) True
-- "_1"
-- >>> showExpr (Term Nil) True
-- "()"
-- >>> showExpr (Term $ Cons (Value 5) (Value 3)) True
-- "(5 . 3)"
-- >>> showExpr (Term $ Cons (Value 2) (Term Nil)) True
-- "(2)"
-- >>> showExpr (Term $ Cons (Value 1) (Term $ Cons (Value 2) (Term Nil))) True
-- "(1 2)"
-- >>> showExpr (Term $ Cons (Term $ Cons (Value 1) (Term $ Cons (Value 2) (Term Nil))) (Term $ Cons (Value 3) (Term $ Cons (Value 4) (Term Nil)))) True
-- "((1 2) 3 4)"
showExpr :: Show a => Expr a -> Bool -> String
showExpr (Value x) _ = show x
showExpr (Variable v) _ = "var" ++ show v
showExpr (Reified n) _ = '_':show n
showExpr (Term Nil) _ = "()"
showExpr (Term (Cons a (Term Nil))) showParenths = parenths showParenths $ showExpr a True
showExpr (Term (Cons a d)) showParenths = parenths showParenths $ showExpr a True ++ 
                                                            (if isList d then " " else " . ") ++ 
                                                            showExpr d False
    
parenths True s = "(" ++ s ++ ")"
parenths False s = s
