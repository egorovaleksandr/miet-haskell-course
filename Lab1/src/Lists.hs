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
  | otherwise = sqrt (sum (zipWith (\a b -> (a - b) ** 2) xs ys))

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find f (x:xs)
  | f x = Just x
  | otherwise = find f xs

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