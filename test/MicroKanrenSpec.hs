{-# LANGUAGE ScopedTypeVariables #-}

module MicroKanrenSpec where

import Test.Hspec

import Data.Functor

import MicroKanren
import MicroKanren.Types
import MicroKanren.Functions
import MicroKanren.Pair
import MicroKanren.Testing

spec :: Spec
spec = do
    describe "MicroKanren" $ do
        it "run" $ do
            let [pea, pod] = (Value <$> ["pea", "pod"] :: [Term Pair String Integer])
            (eval $ fresh <&> (flip run (failure :: Goal Pair () Integer))) `shouldBe` []
            (eval $ fresh <&> (flip run $ pea === pod)) `shouldBe` []
            (eval $ fresh <&> (\q -> run q $ q === pea)) `shouldBe` [pea]
            (eval $ fresh <&> (\q -> run q $ pea === q)) `shouldBe` [pea]
            (eval $ fresh <&> (flip run (success :: Goal Pair () Integer))) `shouldBe` [Reified 0]
            (eval $ fresh <&> (\(q :: Term Pair () Integer) -> run q $ q === q)) `shouldBe` [Reified 0]
        it "runWith" $ do
            let [pea, pod] = Value <$> ["pea", "pod"]
                [olive, oil] = Value <$> ["olive", "oil"]
            (runWith $ \(q :: Term Pair String Integer) -> pure $ q === pea) `shouldBe` [pea]
            (runWith $ \(q :: Term Pair String Integer) -> fresh <&> (\x -> pea === q)) `shouldBe` [pea]
            (runWith $ \(q :: Term Pair String Integer) -> fresh <&> (\x -> pea === x)) `shouldBe` [Reified 0]
            (runWith $ \(q :: Term Pair String Integer) -> fresh <&> (\x -> list [x] === q)) `shouldBe` [list [Reified 0]]
            (runWith $ \(q :: Term Pair String Integer) -> fresh <&> (\x -> x === q )) `shouldBe` [Reified 0]
            (runWith $ \q -> pure $ list [pea, pod] === list [pea, q]) `shouldBe` [pod]
            (runWith $ \q -> pure $ list [pea, pod] === list[q, pod]) `shouldBe` [pea]
            (runWith $ \q -> fresh <&> (\x -> list [q, x] === list [x, pod])) `shouldBe` [pod]
            (runWith $ \(q :: Term Pair String Integer) -> fresh <&> (\x -> list [x, x] === q)) `shouldBe` [list [Reified 0, Reified 0]]
            (runWith $ \(q :: Term Pair String Integer) -> do { x <- fresh; y <- fresh; pure $ list [q, y] === list [list [x, y], x] }) `shouldBe` [list [Reified 0, Reified 0]]
            (runWith $ \(q :: Term Pair String Integer) -> do { x <- fresh; y <- fresh; pure $ list [x, y] === q }) `shouldBe` [list [Reified 0, Reified 1]]
            (runWith $ \(q :: Term Pair String Integer) -> do { x <- fresh; y <- fresh; pure $ list [x, y, x] === q }) `shouldBe` [list [Reified 0, Reified 1, Reified 0]]
            (runWith $ \(q :: Term Pair String Integer) -> pure $ disj2 (olive === q) (oil === q)) `shouldBe` [olive, oil]
        it "conde" $ do
            (runWith $ \(x :: Term Pair Symbol Integer) -> pure $ conde [
                                                                    [Value Olive === x, failure],
                                                                    [Value Oil === x]])
                                                                    `shouldBe` [Value Oil]
        it "fresh" $ do
            let go = do x1 :: (Term Pair () Integer) <- fresh
                        x2 <- fresh
                        y <- fresh
                        return (x1 == x2, x1 == y, y == x2)
            eval go `shouldBe` (False, False, False)
        it "fresh2" $ do
            (eval $ fresh2 :: (Term Pair () Integer, Term Pair () Integer)) `shouldBe` (Variable 0, Variable 1)
        it "fresh3" $ do
            (eval $ fresh3 :: (Term Pair () Integer, Term Pair () Integer, Term Pair () Integer)) `shouldBe` (Variable 0, Variable 1, Variable 2)
