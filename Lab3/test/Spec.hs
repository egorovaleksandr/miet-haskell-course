import FunctorsMonads
import Streams hiding (main)
import Test.Hspec
import Test.Hspec.QuickCheck

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

max3 :: Integer -> Integer -> Integer -> Integer
max3 x y z = max x (max y z)

median3 :: Integer -> Integer -> Integer -> Integer
median3 x y z = x + y + z - max3 x y z - min3 x y z
  where min3 a b c = min a (min b c)

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave (x:xs) ys = x : interleave ys xs

main :: IO ()
main = hspec $ do
    describe "functors and monads" $ do
        it "liftA2'" $ do
            liftA2' (+) (Just 1) (Just 2) `shouldBe` Just 3
            liftA2' (+) Nothing (Just 2) `shouldBe` (Nothing :: Maybe Int)
        it "seqA" $ do
            seqA [Just 1, Just 2] `shouldBe` Just [1, 2]
            seqA [Just 1, Just 2, Nothing] `shouldBe` (Nothing :: Maybe [Int])
        it "traverseA" $ do
            traverseA Just [1, 2] `shouldBe` Just [1, 2]
            traverseA (\a -> if a > 2 then Just a else Nothing) [1, 3] `shouldBe` (Nothing :: Maybe [Int])
        it "filterA" $ do
            filterA (\a -> if a > 10 then Nothing else Just (a > 0)) [-1, -2, 1, 2] `shouldBe` Just [1, 2]
            filterA (\a -> if a < 0 then Nothing else Just (a > 1)) [-1, -2, 1, 2] `shouldBe` (Nothing :: Maybe [Int])
        it "composeM" $ do
            composeM Just Just 5 `shouldBe` Just 5
            composeM Just (const Nothing) 5 `shouldBe` (Nothing :: Maybe Int)
        it "Either instances" $ do
            (Right (+1) <**> Right 5 :: Either String Int) `shouldBe` Right 6
            (Right 5 >>== \x -> Right (x * 2) :: Either String Int) `shouldBe` Right 10
        it "Function instances" $ do
            ((+1) <**> (*2) $ 5 :: Int) `shouldBe` 11
            ((*2) >>== \x -> (+x) $ 3 :: Int) `shouldBe` 9

    describe "streams" $ do
        it "streamToList and sTake" $ do
            sTake 5 (sRepeat 1) `shouldBe` [1,1,1,1,1]
            sTake 6 (sCycle [1,2,3]) `shouldBe` [1,2,3,1,2,3]
            take 5 (streamToList nats) `shouldBe` [0,1,2,3,4]
        it "sIterate and sInterleave" $ do
            sTake 5 (sIterate (+1) 0) `shouldBe` [0,1,2,3,4]
            sTake 6 (sInterleave nats nats) `shouldBe` [0,0,1,1,2,2]
        it "ruler" $ do
            sTake 8 ruler `shouldBe` [0,1,0,2,0,1,0,3]
        it "minMax, minMaxBang" $ do
            minMax [1, 2, 3, 4] `shouldBe` Just (1, 4)
            minMaxBang [1, 2, 3, 4] `shouldBe` Just (1, 4)
            minMax ([] :: [Int]) `shouldBe` Nothing
        it "Functor, Applicative, Monad, Foldable, Traversable" $ do
            sTake 5 (fmap (*2) nats) `shouldBe` [0,2,4,6,8]
            sTake 5 (pure 1 :: Stream Int) `shouldBe` [1,1,1,1,1]
            sTake 5 (nats >>= \x -> sRepeat x) `shouldBe` [0,1,2,3,4]
            foldr (+) 0 (sTake 5 nats) `shouldBe` 10
            sTake 5 (traverse Just nats) `shouldBe` [Just 0, Just 1, Just 2, Just 3, Just 4]

    describe "property tests (Lab 1 functions)" $ do
        prop "xor is commutative" $ \x y -> xor x y == (xor y x :: Bool)
        prop "xor with False is identity" $ \x -> xor x False == (x :: Bool)
        prop "max3 is >= all arguments" $ \x y z -> let m = max3 x y z in m >= (x :: Integer) && m >= y && m >= z
        prop "max3 matches max" $ \x y z -> max3 x y z == (max x (max y z) :: Integer)
        prop "median3 matches sorted middle" $ \x y z -> median3 x y z == (sort [x,y,z] !! 1 :: Integer)
        prop "interleave length is sum of lengths" $ \xs ys -> length (interleave xs ys) == (length (xs :: [Int]) + length ys)