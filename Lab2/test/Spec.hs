import Poly
import SimpleLang
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "poly" $ do
        describe "applyPoly" $ do
            it "constant" $ applyPoly (P [5]) 3 `shouldBe` 5
            it "zero" $ applyPoly (P []) 3 `shouldBe` 0
            it "x" $ applyPoly x 7 `shouldBe` (7 :: Integer)
            it "linear" $ applyPoly (P [1, 2]) 3 `shouldBe` 7
            it "quadratic" $ applyPoly (P [1, 2, 3]) 2 `shouldBe` 17
            it "expression" $ applyPoly (3 * x * x + 1 :: Poly Integer) 2 `shouldBe` 13
        describe "Eq" $ do
            it "ignores trailing zeros" $
                (P [1, 2] :: Poly Integer) `shouldBe` P [1, 2, 0, 0]
            it "different" $
                (P [1, 2] :: Poly Integer) `shouldNotBe` P [1, 3]
        describe "Show" $ do
            it "example from task" $
                show (3 * x * x + 1 :: Poly Integer) `shouldBe` "3 * x^2 + 1"
            it "zero" $ show (P [] :: Poly Integer) `shouldBe` "0"
            it "x" $ show (x :: Poly Integer) `shouldBe` "x"
            it "negative" $
                show (x - 1 :: Poly Integer) `shouldBe` "x - 1"
        describe "plus/times" $ do
            it "plus" $ plus (P [1, 2]) (P [3, 4]) `shouldBe` (P [4, 6] :: Poly Integer)
            it "times" $ times (P [1, 1]) (P [1, 1]) `shouldBe` (P [1, 2, 1] :: Poly Integer)
        describe "deriv" $ do
            it "basic" $
                deriv (P [1, 2, 3] :: Poly Integer) `shouldBe` P [2, 6]
            it "nderiv" $
                nderiv 2 (P [1, 2, 3] :: Poly Integer) `shouldBe` P [6]

    describe "simpleLang" $ do
        describe "eval" $ do
            it "constants" $ eval empty (Val 5) `shouldBe` 5
            it "variables" $ eval (extend empty "x" 10) (Var "x") `shouldBe` 10
            it "ops" $ eval empty (Op (Val 1) Plus (Val 2)) `shouldBe` 3
            it "cmp" $ do
                eval empty (Op (Val 5) Gt (Val 3)) `shouldBe` 1
                eval empty (Op (Val 3) Gt (Val 5)) `shouldBe` 0
        describe "desugar" $ do
            it "assign" $ desugar (Assign "x" (Val 1)) `shouldBe` DAssign "x" (Val 1)
            it "incr"    $ desugar (Incr "x") `shouldBe` DAssign "x" (Op (Var "x") Plus (Val 1))
            it "skip"    $ desugar Skip `shouldBe` DSkip
            it "empty block" $ desugar (Block []) `shouldBe` DSkip
            it "block of two" $
                desugar (Block [Assign "x" (Val 1), Assign "y" (Val 2)])
                  `shouldBe` DSequence (DAssign "x" (Val 1)) (DAssign "y" (Val 2))
        describe "run" $ do
            it "factorial" $
                run (extend empty "In" 5) factorial "Out" `shouldBe` 120
            it "fibonacci" $ do
                run (extend empty "In" 0)  fibonacci "Out" `shouldBe` 1
                run (extend empty "In" 1)  fibonacci "Out" `shouldBe` 1
                run (extend empty "In" 5)  fibonacci "Out" `shouldBe` 8
                run (extend empty "In" 10) fibonacci "Out" `shouldBe` 89
            it "squareRoot" $ do
                run (extend empty "A" 0)  squareRoot "B" `shouldBe` 0
                run (extend empty "A" 10) squareRoot "B" `shouldBe` 3
                run (extend empty "A" 16) squareRoot "B" `shouldBe` 4
                run (extend empty "A" 17) squareRoot "B" `shouldBe` 4