module Lists where

removeFirst :: Integer -> [Integer] -> [Integer]
removeFirst _ [] = []
removeFirst x (y:ys)
  | x == y = ys
  | otherwise = y : removeFirst x ys

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave (x:xs) ys = x : interleave ys xs

newtype Point = Point [Double] deriving (Eq, Show, Read)

distance :: Point -> Point -> Double
distance (Point xs) (Point ys)
  | length xs /= length ys = error "Разная размерность"
  | otherwise = sqrt (sumSquares (zip xs ys))
  where
    sumSquares :: [(Double, Double)] -> Double
    sumSquares [] = 0
    sumSquares ((a, b) : rest) = (a - b) ** 2 + sumSquares rest

findRec :: (a -> Bool) -> [a] -> Maybe a
findRec _ [] = Nothing
findRec f (x:xs)
  | f x = Just x
  | otherwise = findRec f xs

findStd :: (a -> Bool) -> [a] -> Maybe a
findStd f xs = case filter f xs of
  (x:_) -> Just x
  []    -> Nothing

find :: (a -> Bool) -> [a] -> Maybe a
find = findRec

findLast :: (a -> Bool) -> [a] -> Maybe a
findLast f xs = foldl (\acc x -> if f x then Just x else acc) Nothing xs

mapFuncs :: [a -> b] -> a -> [b]
mapFuncs fs x = map (\f -> f x) fs

satisfiesAll :: [a -> Bool] -> a -> Bool
satisfiesAll preds x = all (\f -> f x) preds

data NEL a = NEL a [a] deriving (Eq, Show, Read)

tailNel :: NEL a -> [a]
tailNel (NEL _ xs) = xs

lastNel :: NEL a -> a
lastNel (NEL x []) = x
lastNel (NEL _ (x:xs)) = lastNel (NEL x xs)

zipNel :: NEL a -> NEL b -> NEL (a, b)
zipNel (NEL x xs) (NEL y ys) = NEL (x, y) (zip xs ys)

listToNel :: [a] -> Maybe (NEL a)
listToNel [] = Nothing
listToNel (x:xs) = Just (NEL x xs)

nelToList :: NEL a -> [a]
nelToList (NEL x xs) = x : xs