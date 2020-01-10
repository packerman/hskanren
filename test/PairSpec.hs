{-# LANGUAGE ScopedTypeVariables #-}

module PairSpec where

import Test.Hspec

import MicroKanren
import MicroKanren.Functions
import MicroKanren.Testing
import MicroKanren.Pair
import Pair

spec :: Spec
spec = do
    describe "Pair" $ do
        it "caro" $ do
            (runWith $ \q -> caro (values "acorn") q) `shouldBe` [Value 'a']
            (runWith $ \q -> caro (values "acorn") (Value 'a')) `shouldBe` [Reified 0]
            (runWith $ \r -> fresh2 >>= (\(x, y) ->
                                conjM [caro (list [r, y]) x,
                                        pure $ (Value Pear) === x])) `shouldBe` [Value Pear]
            (runWith $ \r -> fresh2 >>= (\(x, y) ->
                                conjM [caro (values [Grape, Raisin, Pear]) x,
                                        caro (list [list [Value A], list [Value B], list [Value C]]) y,
                                        pure $ (Term $ Cons x y) === r]))
                `shouldBe` [list [Value Grape, Value A]]
        it "cdro" $ do
            (runWith $ \r ->
                        fresh >>= (\v ->
                                    conjM [cdro (values "acorn") v,
                                            fresh >>= (\w ->
                                                        conjM [cdro v w,
                                                        caro w r])]))
                `shouldBe` [Value 'o']
            (runWith $ \r -> fresh2 >>= (\(x, y) ->
                                conjM [cdro (values [Grape, Raisin, Pear]) x,
                                        caro (list [list [Value A], list [Value B], list [Value C]]) y,
                                        pure $ (Term $ Cons x y) === r]))
                `shouldBe` [list [list [Value Raisin, Value Pear], Value A]]
            (runWith $ \x -> cdro (values "corn") (list [x, Value 'r', Value 'n'])) `shouldBe` [Value 'o']
            (runWith $ \l -> fresh>>= (\x ->
                                conjM [cdro l (values "corn"),
                                    caro l x,
                                    pure $ (Value 'a') === x]))
                `shouldBe` [list [Value 'a', Value 'c', Value 'o', Value 'r', Value 'n']]
        it "conso" $ do
            (runWith $ (\l -> conso (values "abc") (values "de") l)) `shouldBe` [list [list [Value 'a', Value 'b', Value 'c'], Value 'd', Value 'e']]
            (runWith $ (\x -> conso x (values "abc") (values "dabc"))) `shouldBe` [Value 'd']
            (runWith $ \r -> fresh3 >>= (\(x, y, z) ->
                                conjM [pure $ list [Value 'e', Value 'a', Value 'd', x] === r,
                                        conso y (list [Value 'a', z, Value 'c']) r]))
                `shouldBe` [list [Value 'e', Value 'a', Value 'd', Value 'c']]
            (runWith $ (\x -> conso x (list [Value 'a', x, Value 'c']) (list [Value 'd', Value 'a', x, Value 'c']))) `shouldBe` [Value 'd']
            (runWith $ \l -> fresh >>= (\x ->
                                conjM [pure $ list [Value 'd', Value 'a', x, Value 'c'] === l,
                                        conso x (list [Value 'a', x, Value 'c']) l]))
                `shouldBe` [list [Value 'd', Value 'a', Value 'd', Value 'c']]
        it "nullo" $ do
            (runWith (\_ -> pure $ nullo $ values [Grape, Raisin, Pear])) `shouldBe` []
            (runWith (\(_ :: Term Pair Symbol Integer) -> pure $ nullo $ Term Nil)) `shouldBe` [Reified 0]
            (runWith (\(x :: Term Pair Symbol Integer) -> pure $ nullo x)) `shouldBe` [list []]
        it "appendo" $ do
            runWith2 (\x y -> appendo x y $ values "abcde") `shouldBe` [list [list [], list [Value 'a', Value 'b', Value 'c', Value 'd', Value 'e']],
                                                                        list [list [Value 'a'], list [Value 'b', Value 'c', Value 'd', Value 'e']],
                                                                        list [list [Value 'a', Value 'b'], list [Value 'c', Value 'd', Value 'e']],
                                                                        list [list [Value 'a', Value 'b', Value 'c'], list [Value 'd', Value 'e']],
                                                                        list [list [Value 'a', Value 'b', Value 'c', Value 'd'], list [Value 'e']],
                                                                        list [list [Value 'a', Value 'b', Value 'c', Value 'd', Value 'e'], list []]]
