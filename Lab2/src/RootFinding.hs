-- Не забудьте добавить тесты.
module RootFinding where

import Poly (Poly(..), Differentiable(..))

-- Задание 1 -----------------------------------------

newtonStep :: (Differentiable f, Fractional a, Eq a) => f a -> a -> Maybe a
newtonStep f t =
    let ft   = apply f t
        ft'  = apply (deriv f) t
    in if ft' == 0 then Nothing else Just (t - ft / ft')

-- Задание 2 -----------------------------------------

newton :: (Differentiable f, Fractional a, Ord a) => a -> Int -> f a -> a -> Maybe a
newton epsilon maxSteps f t0
    | epsilon <= 0  = Nothing
    | maxSteps < 0  = Nothing
    | otherwise     = go maxSteps t0
  where
    go n t
        | abs (apply f t) <= epsilon = Just t
        | n == 0                     = Nothing
        | otherwise                  = case newtonStep f t of
            Nothing -> Nothing
            Just t' -> go (n - 1) t'

-- Необязательное задание ----------------------------------------------

halleyStep :: (Differentiable f, Fractional a, Eq a) => f a -> a -> Maybe a
halleyStep f t =
    let ft    = apply f t
        ft'   = apply (deriv f) t
        ft''  = apply (deriv (deriv f)) t
        denom = 2 * ft'^2 - ft * ft''
    in if denom == 0 then Nothing else Just (t - 2 * ft * ft' / denom)

halley :: (Differentiable f, Fractional a, Ord a) => a -> Int -> f a -> a -> Maybe a
halley epsilon maxSteps f t0
    | epsilon <= 0 = Nothing
    | maxSteps < 0 = Nothing
    | otherwise    = go maxSteps t0
  where
    go n t
        | abs (apply f t) <= epsilon = Just t
        | n == 0                     = Nothing
        | otherwise                  = case halleyStep f t of
            Nothing -> Nothing
            Just t' -> go (n - 1) t'