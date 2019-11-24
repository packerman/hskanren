module MicroKanren.Types where

import qualified Data.Map as M

type Var = Integer

data Expr a = Value a |
                Variable Var |
                Reified Int |
                Nil |
                Cons (Expr a) (Expr a)
                deriving (Eq, Show)

type Substitution a = M.Map Var (Expr a)

type Goal a = Substitution a -> [Substitution a]

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
isList :: Expr a -> Bool
isList Nil = True
isList (Cons a d) = isList d
isList _ = False

-- |
-- >>> showExpr (Value 5) True
-- "5"
-- >>> showExpr (Variable 3) True
-- "v3"
-- >>> showExpr (Reified 1) True
-- "_1"
-- >>> showExpr Nil True
-- "()"
-- >>> showExpr (Cons (Value 5) (Value 3)) True
-- "(5 . 3)"
-- >>> showExpr (Cons (Value 2) Nil) True
-- "(2)"
-- >>> showExpr (Cons (Value 1) (Cons (Value 2) Nil)) True
-- "(1 2)"
-- >>> showExpr (Cons (Cons (Value 1) (Cons (Value 2) Nil)) (Cons (Value 3) (Cons (Value 4) Nil))) True
-- "((1 2) 3 4)"
showExpr :: Show a => Expr a -> Bool -> String
showExpr (Value x) _ = show x
showExpr (Variable v) _ = 'v':show v
showExpr (Reified n) _ = '_':show n
showExpr Nil _ = "()"
showExpr (Cons a Nil) showParenths = parenths showParenths $ showExpr a True
showExpr (Cons a d) showParenths = parenths showParenths $ showExpr a True ++ 
                                                            (if isList d then " " else " . ") ++ 
                                                            showExpr d False
    
parenths True s = "(" ++ s ++ ")"
parenths False s = s
