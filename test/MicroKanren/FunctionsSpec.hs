module MicroKanren.FunctionsSpec where

import Test.Hspec

import qualified Data.Map as M

import MicroKanren.Types
import MicroKanren.Functions
import MicroKanren.Pair
import MicroKanren.Testing

spec :: Spec
spec = do
    describe "Functions" $ do
        it "ifte" $ do
            let [x, y] = testVars 2
            (ifte success (Value False === y) (Value True === y)) emptySubst `shouldBe` [M.fromList [(2, Value False)]]
            (ifte failure (Value False === y) (Value True === y)) emptySubst `shouldBe` [M.fromList [(2, Value True)]]
            (ifte (Value True === x) (Value False === y) (Value True === y)) emptySubst `shouldBe` [M.fromList [(1, Value True),(2, Value False)]]
            (ifte (disj2 (Value True === x) (Value False === x)) (Value False === y) (Value True === y)) emptySubst `shouldBe` [M.fromList [(1, Value True), (2, Value False)], M.fromList [(1, Value False), (2, Value False)]]
        it "once" $ do
            let [x, y] = testVars 2
            (ifte (once (disj2 (Value True === x) (Value False === x))) (Value False === y) (Value True === y)) emptySubst `shouldBe` [M.fromList [(1, Value True),(2, Value False)]]
        it "runGoal" $ do
            let [x] = testVars 1
            map (reify x) (runGoal 5 (disj2 (Value "olive" === x) (Value "oil" === x))) `shouldBe` [Value "olive", Value "oil"]
        it "reify" $ do
            let variables = testVars 6
                [vu, vv, vw, vx, vy, vz] = variables
                [Variable u, Variable v, Variable w, Variable x, Variable y, Variable z] = variables
                [ice, corn] = Value <$> ["ice", "corn"]
                s = M.fromList [(x, list [vu, vw, vy, vz, list [ice, vz]]), (y, corn), (w, list [vv, vu])]
            (reify vx) s `shouldBe` list [Reified 0, list [Reified 1, Reified 0], corn, Reified 2, list [ice, Reified 2]]
        it "walkMany" $ do
            let [w, x, y, z] = [1..4]
            walkMany (Variable w) (M.fromList [(x, Value 'b'), (z, Variable y), (w, list [Variable x, Value 'e', Variable z])]) `shouldBe` list [Value 'b', Value 'e', Variable y]
        it "===" $ do
            (Value True === Value False) (emptySubst :: Substitution Pair Bool Integer) `shouldBe` []
            (Value False === Value False) (emptySubst :: Substitution Pair Bool Integer) `shouldBe` [M.empty]
            let [x, y] = testVars 2
            (x === y) (emptySubst :: Substitution Pair () Integer) `shouldBe` [M.fromList [(1, y)]]
            (y === x) (emptySubst :: Substitution Pair () Integer) `shouldBe` [M.fromList [(2, x)]]
        it "walk" $ do
            let [v, w, x, y, z] = [1..5]
            walk (Variable z :: Term Pair Char Integer) (M.fromList [(z, Value 'a'), (x, Variable w), (y, Variable z)]) `shouldBe` Value 'a'
            walk (Variable y :: Term Pair Char Integer) (M.fromList [(z, Value 'a'), (x, Variable w), (y, Variable z)]) `shouldBe` Value 'a'
            walk (Variable x :: Term Pair Char Integer) (M.fromList [(z, Value 'a'), (x, Variable w), (y, Variable z)]) `shouldBe` Variable w
            walk (Variable x :: Term Pair Char Integer) (M.fromList [(x, Variable y), (v, Variable x), (w, Variable x)]) `shouldBe` Variable y
            walk (Variable v :: Term Pair Char Integer) (M.fromList [(x, Variable y), (v, Variable x), (w, Variable x)]) `shouldBe` Variable y
            walk (Variable w :: Term Pair Char Integer) (M.fromList [(x, Variable y), (v, Variable x), (w, Variable x)]) `shouldBe` Variable y
            walk (Variable w) (M.fromList [(x, Value 'b'), (z, Variable y), (w, list [Variable x, Value 'e', Variable z])]) `shouldBe` list [Variable x, Value 'e', Variable z]
