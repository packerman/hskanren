module MicroKanren.Pair where

data Pair a = Cons a a | Nil deriving (Eq)

instance Foldable Pair where
    foldMap f (Cons a b) = f a <> f b
    foldMap _ _ = mempty

instance Functor Pair where
    fmap f (Cons a b) = Cons (f a) (f b)
    fmap _ _ = Nil
