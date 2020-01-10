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
deriving instance (Show v, Show a, Show (f (Term f a v))) => Show (Term f a v)

type Expr a = Term Pair a Integer

type Substitution f a v = M.Map v (Term f a v)

type Goal f a v = Substitution f a v -> [Substitution f a v]
