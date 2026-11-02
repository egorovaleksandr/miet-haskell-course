-- Не забудьте добавить тесты.

module Poly where

-- Многочлены
-- a -- тип коэффициентов, список начинается со свободного члена.
newtype Poly a = P [a]

-- Задание 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Задание 2 -----------------------------------------

applyPoly :: Num a => Poly a -> a -> a
applyPoly (P cs) t = foldr (\c acc -> c + t * acc) 0 cs

-- Задание 3 -----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (P xs) == (P ys) =
        reverse (dropWhile (== 0) (reverse xs)) == reverse (dropWhile (== 0) (reverse ys))

-- Задание 4 -----------------------------------------

degree :: (Num a, Eq a) => Poly a -> Maybe Int
degree (P cs) = case dropWhile (== 0) (reverse cs) of
    []      -> Nothing
    trimmed -> Just (length trimmed - 1)

-- Задание 5 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P xs) (P ys) = P (zipWithPad (+) xs ys)
  where
    zipWithPad f as bs =
        let n = max (length as) (length bs)
        in zipWith f (as ++ replicate (n - length as) 0)
                     (bs ++ replicate (n - length bs) 0)

-- Задание 6 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P []) _ = P []
times _ (P []) = P []
times (P xs) (P ys) = P (take (length xs + length ys - 1) (go xs))
  where
    go []     = []
    go (a:as) = zipWith (+) (map (a*) ys ++ repeat 0) (0 : go as ++ repeat 0)

-- Задание 7 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P cs) = P (map negate cs)
    fromInteger n = P [fromInteger n]
    -- Эти функции оставить как undefined, поскольку для 
    -- многочленов они не имеют математического смысла
    abs    = undefined
    signum = undefined

-- Задание 8 -----------------------------------------

composePoly :: Num a => Poly a -> Poly a -> Poly a
composePoly (P ps) q = foldr (\c acc -> P [c] + q * acc) (P []) ps

-- Задание 9 -----------------------------------------

class Differentiable f where
    apply  :: Num a => f a -> a -> a
    deriv  :: Num a => f a -> f a
    nderiv :: Num a => Int -> f a -> f a
    nderiv 0 p = p
    nderiv n p = nderiv (n - 1) (deriv p)

-- Задание 10 ----------------------------------------

instance Differentiable Poly where
    apply = applyPoly
    deriv (P cs) = P (zipWith (\i c -> fromIntegral (i :: Int) * c) [1..] (drop 1 cs))

-- Готовый вывод многочленов --------------------------------------------

instance (Num a, Ord a, Show a) => Show (Poly a) where
    showsPrec precedence (P coefficients) =
        case terms of
            [] -> showString "0"
            [(0, coefficient)] -> showsPrec precedence coefficient
            first : rest ->
                showParen (precedence > expressionPrecedence first rest) $
                    showString (firstTerm first ++ concatMap nextTerm rest)
      where
        terms = [(power, coefficient)
                | (power, coefficient) <- reverse (zip [0 :: Int ..] coefficients)
                , coefficient /= 0]

        firstTerm term@(_, coefficient) =
            (if coefficient < 0 then "-" else "") ++ termBody term

        nextTerm term@(_, coefficient) =
            (if coefficient < 0 then " - " else " + ") ++ termBody term

        termBody (0, coefficient) = magnitudeText 0 coefficient
        termBody (power, coefficient)
            | coefficient == 1 || coefficient == -1 = powerText power
            | otherwise = magnitudeText 8 coefficient ++ " * " ++ powerText power

        powerText 1 = "x"
        powerText power = "x^" ++ show power

        magnitudeText context coefficient
            | magnitude < 0 = case show magnitude of
                '-' : digits -> digits
                text -> text
            | otherwise = showsPrec context magnitude ""
          where
            magnitude = if coefficient < 0 then negate coefficient else coefficient

        expressionPrecedence (_, _) (_ : _) = 6
        expressionPrecedence (power, coefficient) []
            | coefficient < 0 = 6
            | coefficient == 1 && power == 1 = 11
            | coefficient == 1 = 8
            | otherwise = 7