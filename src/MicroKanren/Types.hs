module MicroKanren.Types where

import qualified Data.Map as M

type Var = Int

data Expr a = Value a |
                Variable Var |
                Reified Int |
                Nil |
                Cons (Expr a) (Expr a)
                deriving (Eq, Show)

type Substitution a = M.Map Var (Expr a)

type Goal a = Substitution a -> [Substitution a]
