import FirstSteps
import Lists
import Luhn
import Test.Hspec
import Control.Exception (evaluate, anyErrorCall)

main :: IO ()
main = hspec $ do
    describe "first steps" $ do
        it "xor" $ do
            xor True True `shouldBe` False
            xor True False `shouldBe` True
            xor False True `shouldBe` True
            xor False False `shouldBe` False
        it "max3" $ do
            max3 1 3 2 `shouldBe` 3
            max3 5 2 5 `shouldBe` 5
            max3 (-1) (-5) (-3) `shouldBe` -1
        it "median3" $ do
            median3 1 3 2 `shouldBe` 2
            median3 5 2 5 `shouldBe` 5
            median3 1 1 1 `shouldBe` 1
        it "rgbToCmyk" $ do
            rgbToCmyk (RGB 0 0 0) `shouldBe` CMYK 0 0 0 1
            rgbToCmyk (RGB 255 255 255) `shouldBe` CMYK 0 0 0 0
            rgbToCmyk (RGB 255 0 0) `shouldBe` CMYK 0 1.0 1.0 0.0
        it "geomProgression" $ do
            geomProgression 3.0 2.0 2 `shouldBe` 12.0
            geomProgression 3.0 2.0 0 `shouldBe` 3.0
            evaluate (geomProgression 3.0 2.0 (-1)) `shouldThrow` anyErrorCall
        it "coprime" $ do
            coprime 10 15 `shouldBe` False
            coprime 12 35 `shouldBe` True
            coprime 1 1 `shouldBe` True
            coprime 0 1 `shouldBe` True
            coprime (-5) 10 `shouldBe` False

    describe "lists" $ do
        it "distance" $ do
            distance (Point [1.0, 0.0]) (Point [0.0, 1.0]) `shouldBe` sqrt 2.0
            distance (Point [1.0]) (Point [1.0]) `shouldBe` 0.0
            evaluate (distance (Point [1.0]) (Point [1.0, 2.0])) `shouldThrow` anyErrorCall
        it "removeFirst" $ do
            removeFirst 2 [1, 2, 3, 2] `shouldBe` [1, 3, 2]
            removeFirst 4 [1, 2, 3] `shouldBe` [1, 2, 3]
            removeFirst 2 [] `shouldBe` ([] :: [Integer])
        it "interleave" $ do
            interleave [1, 2, 3] [10, 20, 30] `shouldBe` [1, 10, 2, 20, 3, 30]
            interleave [1, 2, 3] [] `shouldBe` [1, 2, 3]
            interleave [] [] `shouldBe` ([] :: [Integer])
        it "find" $ do
            find (> 0) [-1, 2, -3, 4] `shouldBe` Just 2
            find (> 0) [-1, -2] `shouldBe` Nothing
        it "findLast" $ do
            findLast (> 0) [-1, 2, -3, 4] `shouldBe` Just 4
            findLast (> 0) [] `shouldBe` (Nothing :: Maybe Integer)
        it "mapFuncs" $ do
            mapFuncs [\x -> x*x, (1 +), \x -> if even x then 1 else 0] 3 `shouldBe` [9, 4, 0]
            mapFuncs [] 3 `shouldBe` ([] :: [Integer])
        it "satisfiesAll" $ do
            satisfiesAll [even, \x -> x `rem` 5 == 0] 10 `shouldBe` True
            satisfiesAll [] 4 `shouldBe` True
            satisfiesAll [even] 5 `shouldBe` False
        it "NEL functions" $ do
            tailNel (NEL 1 [2, 3]) `shouldBe` [2, 3]
            tailNel (NEL 1 []) `shouldBe` ([] :: [Integer])
            lastNel (NEL 1 [2, 3]) `shouldBe` 3
            zipNel (NEL 1 [2]) (NEL 'a' ['b']) `shouldBe` NEL (1, 'a') [(2, 'b')]
            listToNel [1, 2] `shouldBe` Just (NEL 1 [2])
            listToNel [] `shouldBe` (Nothing :: Maybe (NEL Integer))
            nelToList (NEL 1 [2, 3]) `shouldBe` [1, 2, 3]

    describe "luhn" $ do
        it "isLuhnValid" $ do
            isLuhnValid 4532015112830366 `shouldBe` True
            isLuhnValid 1234567812345678 `shouldBe` False
            isLuhnValid 0 `shouldBe` True
            isLuhnValid (-4532015112830366) `shouldBe` False