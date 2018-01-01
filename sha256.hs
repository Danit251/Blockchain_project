import Data.Bits
import Data.Int
import Data.Char

ch :: Int32 -> Int32 -> Int32 -> Int32
ch x y z = (x .&. y) `xor` ((complement x) .|. z)

maj :: Int32 -> Int32 -> Int32 -> Int32
maj x y z = (x .&. y) `xor` (x .&. z) `xor` (y .&. z)

sigma0 :: Int32 -> Int32
sigma0 x = (rotate x (-2)) `xor` (rotate x (-13)) `xor` (rotate x (-22))

sigma1 :: Int32 -> Int32
sigma1 x = (rotate x (-6)) `xor` (rotate x (-11)) `xor` (rotate x (-25))

sigma'0 :: Int32 -> Int32
sigma'0 x = (rotate x (-7)) `xor` (rotate x (-18)) `xor` (rotate x (-3))

sigma'1 :: Int32 -> Int32
sigma'1 x = (rotate x (-17)) `xor` (rotate x (-19)) `xor` (rotate x (-10))

a32 = fromIntegral (1 :: Int8) ::Int
b32 = fromIntegral (2 :: Int8) ::Int

concatBytes :: Int8 -> Int8 -> Int8 -> Int8 -> Int32
concatBytes a b c d = (fromIntegral a ::Int32) *(2^24) + (fromIntegral b ::Int32) *(2^16) + (fromIntegral c ::Int32) *(2^8) + (fromIntegral d ::Int32)


--type Block = [Int8]
--prepMsg :: String -> [Block]
--prepMsg m = 

--decToBin :: Int -> String

asciiToBin x = map intToDigit (reverse $ asciiToBin' (ord x))
  where
    asciiToBin' 0 = []
    asciiToBin' y = let (a,b) = quotRem y 2 in [b] ++ asciiToBin' a
      
ascii256ToBin x = map intToDigit (take (8 - length(asciiToBin x)) [0,0..]) ++ asciiToBin x

--msgToBin :: String -> String
msgToBin m = map ascii256ToBin m
    
