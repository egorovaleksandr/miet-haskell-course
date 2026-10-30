module Luhn where

isLuhnValid :: Integer -> Bool
isLuhnValid n 
  | n < 0 = False
  | otherwise = luhnSum (reverse (toDigits n)) `mod` 10 == 0
  where
    toDigits :: Integer -> [Integer]
    toDigits 0 = [0]
    toDigits x = go x []
      where
        go 0 acc = acc
        go y acc = go (y `div` 10) (fromIntegral (y `mod` 10) : acc)

    luhnSum :: [Integer] -> Integer
    luhnSum [] = 0
    luhnSum [x] = x
    luhnSum (x:y:zs) =
      let y' = y * 2
          y'' = if y' > 9 then y' - 9 else y'
      in x + y'' + luhnSum zs