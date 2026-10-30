module Poly where

-- Бонус (обратный порядок коэффициентов):
-- Если бы коэффициенты шли от старшей степени к младшей:
-- * Show стал бы проще (можно проходить список в прямом порядке);
-- * applyPoly можно было бы считать через foldl по схеме Горнера: foldl (\acc c -> acc*x + c) 0 coeffs;
-- * fromInteger стал бы проще (добавление нулей в конец);
-- * НО plus и times стали бы заметно сложнее (нужно выравнивать списки по старшей степени);
-- * deriv требовал бы знания степени; Eq требовал бы отбрасывания ведущих нулей.
newtype Poly a = P [a]

-- Задание 1
x :: Num a => Poly a
x = P [0, 1]

-- Задание 2
applyPoly :: Num a => Poly a -> a -> a
applyPoly (P coeffs) v = foldr (\c acc -> c + v * acc) 0 coeffs

-- Задание 3: многочлены, отличающиеся только хвостовыми нулями, равны
instance (Num a, Eq a) => Eq (Poly a) where
    P xs == P ys = dropZeros xs == dropZeros ys
      where
        dropZeros = reverse . dropWhile (== 0) . reverse

-- Задание 4
instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P coeffs) =
      let terms = [(i, c) | (i, c) <- zip [0..] coeffs, c /= 0]
      in if null terms then "0" else go True (reverse terms)
      where
        go _ [] = ""
        go isFirst ((i, c):rest) =
          let s = termStr i c
              neg = case s of ('-':_) -> True; _ -> False
          in if isFirst
               then s ++ go False rest
               else if neg
                 then " - " ++ drop 1 s ++ go False rest
                 else " + " ++ s ++ go False rest
        termStr 0 c = show c
        termStr 1 c = coeffStr c "x"
        termStr i c = coeffStr c ("x^" ++ show i)
        coeffStr c v
          | c == 1 = v
          | c == -1 = "-" ++ v
          | otherwise = show c ++ " * " ++ v

-- Задание 5
plus :: Num a => Poly a -> Poly a -> Poly a
plus (P xs) (P ys) = P (addL xs ys)
  where
    addL [] ys = ys
    addL xs [] = xs
    addL (a:as) (b:bs) = (a + b) : addL as bs

-- Задание 6
times :: Num a => Poly a -> Poly a -> Poly a
times (P xs) (P ys) = P (mulL xs ys)
  where
    mulL [] _ = []
    mulL _ [] = []
    mulL (a:as) bs = addL (map (a *) bs) (0 : mulL as bs)
    addL [] ys = ys
    addL xs [] = xs
    addL (a:as) (b:bs) = (a + b) : addL as bs

-- Задание 7
instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P cs) = P (map negate cs)
    fromInteger n = P [fromInteger n]
    abs    = undefined
    signum = undefined

-- Задание 8
class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n p
      | n <= 0 = p
      | otherwise = nderiv (n - 1) (deriv p)

-- Задание 9
instance Num a => Differentiable (Poly a) where
    deriv (P []) = P []
    deriv (P (_:cs)) = P (zipWith (*) (map fromInteger [1..]) cs)