module FirstSteps where
import Data.Word (Word8)

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

max3, median3 :: Integer -> Integer -> Integer -> Integer
max3 x y z = max x (max y z)

min3 :: Integer -> Integer -> Integer -> Integer
min3 x y z = min x (min y z)

median3 x y z = x + y + z - max3 x y z - min3 x y z

data RGB = RGB { red :: Word8, green :: Word8, blue :: Word8 } deriving (Eq, Show, Read)
data CMYK = CMYK { cyan :: Double, magenta :: Double, yellow :: Double, black :: Double } deriving (Eq, Show, Read)

rgbToCmyk :: RGB -> CMYK
rgbToCmyk (RGB r8 g8 b8) =
  let r = fromIntegral r8 / 255.0
      g = fromIntegral g8 / 255.0
      b = fromIntegral b8 / 255.0
      k = min (1 - r) (min (1 - g) (1 - b))
  in if k == 1.0
     then CMYK 0 0 0 1
     else CMYK ((1 - r - k) / (1 - k)) ((1 - g - k) / (1 - k)) ((1 - b - k) / (1 - k)) k

geomProgression :: Double -> Double -> Integer -> Double
geomProgression _ _ n | n < 0 = error "Индекс не может быть отрицательным"
geomProgression b _ 0 = b
geomProgression b q n = q * geomProgression b q (n - 1)

coprime :: Integer -> Integer -> Bool
coprime 0 0 = False
coprime a 0 = abs a == 1
coprime a b = coprime b (a `mod` b)