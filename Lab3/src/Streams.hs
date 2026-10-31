{-# OPTIONS_GHC -Wall #-}
module Streams where

import Data.List(intercalate)

data Stream a = a :> Stream a

instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ sTake 10 s)
             ++ ", ..."

streamToList :: Stream a -> [a]
streamToList (x :> xs) = x : streamToList xs

sTake :: Int -> Stream a -> [a]
sTake 0 _ = []
sTake n (x :> xs) = x : sTake (n - 1) xs

sRepeat :: a -> Stream a
sRepeat x = let s = x :> s in s

sCycle :: [a] -> Stream a
sCycle [] = error "sCycle: empty list"
sCycle xs = s
  where
    s = go xs
    go [] = s
    go (y:ys) = y :> go ys

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = x :> sIterate f (f x)

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (x :> xs) ys = x :> sInterleave ys xs

nats :: Stream Integer
nats = sIterate (+ 1) 0

ruler :: Stream Integer
ruler = sInterleave (sRepeat 0) (fmap (+ 1) ruler)

minMaxSlow, minMax, minMaxBang :: Ord a => [a] -> Maybe (a, a)
minMaxSlow [] = Nothing
minMaxSlow xs = Just (minimum xs, maximum xs)

minMax [] = Nothing
minMax (x:xs) = Just (foldl (\(mn, mx) y -> (min mn y, max mx y)) (x, x) xs)

minMaxBang [] = Nothing
minMaxBang (x:xs) = Just (go x x xs)
  where
    go mn mx [] = (mn, mx)
    go mn mx (y:ys) =
      let mn' = min mn y
          mx' = max mx y
      in mn' `seq` mx' `seq` go mn' mx' ys

main :: IO ()
main = print $ minMaxSlow $ sTake 1000000 $ ruler

instance Functor Stream where
    fmap f (x :> xs) = f x :> fmap f xs

instance Applicative Stream where
    pure x = let s = x :> s in s
    (f :> fs) <*> (x :> xs) = f x :> (fs <*> xs)

instance Monad Stream where
    return = pure
    xs >>= f = sJoin (fmap f xs)
      where
        sJoin xss = go nats xss
        go (n :> ns) (ys :> yss) = sIndex ys n :> go ns yss
        sIndex (y :> _) 0 = y
        sIndex (_ :> ys) k = sIndex ys (k - 1)

instance Foldable Stream where
    foldr f z (x :> xs) = f x (foldr f z xs)

instance Traversable Stream where
    traverse f (x :> xs) = liftA2 (:>) (f x) (traverse f xs)