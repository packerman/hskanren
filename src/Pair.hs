{-# LANGUAGE FlexibleContexts #-}

module Pair where

import Data.Functor

import MicroKanren
import MicroKanren.Functions
import MicroKanren.Pair
import MicroKanren.Testing

caro :: (Eq a) => Expr a -> RelationM Pair a Integer
caro p a = fresh <&> (\d -> Term (Cons a d) === p)

cdro :: (Eq a) => Expr a -> RelationM Pair a Integer
cdro p d = fresh <&> (\a -> Term (Cons a d) === p)

conso :: (Eq a) => Expr a -> Expr a -> RelationM Pair a Integer
conso a d p = conj <$> sequence [caro p a, cdro p d]


nullo :: (Eq a) => Relation Pair a Integer
nullo x = x === Term Nil

appendo :: (Eq a) => Expr a -> Expr a -> RelationM Pair a Integer
appendo l t out = disjM [
                        pure $ conj [nullo l, t === out],
                        fresh3 >>= (\(a, d, res) -> conjM [
                                                        conso a d l,
                                                        conso a res out,
                                                        appendo d t res]
                                    )
                        ]
