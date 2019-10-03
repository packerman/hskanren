module Playthings where

import MicroKanren
import MicroKanren.Functions
import MicroKanren.Testing

teacup :: Ord v => Relation Symbol v
teacup t = disj2 (Value Tea === t) (Value Cup === t)

