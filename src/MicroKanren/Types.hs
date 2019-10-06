module MicroKanren.Types where

import qualified Data.Map as M

type Var v = (Int, v)

data Expr a v = Value a |
                Variable (Var v) |
                Reified Int |
                Nil |
                Cons (Expr a v) (Expr a v)
                deriving (Eq, Show)

type Substitution a v = M.Map (Var v) (Expr a v)

type Goal a v = Substitution a v -> [Substitution a v]
