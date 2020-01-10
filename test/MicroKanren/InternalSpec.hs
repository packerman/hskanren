module MicroKanren.InternalSpec where

import Test.Hspec
import Control.Monad.State

import MicroKanren.Internal

spec :: Spec
spec = do
    describe "Internal" $ do
        it "getAndInc" $ do
            getAndInc defaultCounter `shouldBe` (0, Counter 1)
            getAndInc (Counter 5) `shouldBe` (5, Counter 6)
            let m = do
                        a <- state getAndInc
                        b <- do
                                state getAndInc
                                state getAndInc
                        c <- do
                                state getAndInc
                        return $ a + b + c :: State Counter Integer
            evalState m defaultCounter `shouldBe` 5
