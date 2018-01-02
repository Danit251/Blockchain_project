import Data.Bits
import Data.Int
import Data.Char
import Data.List

{-# LANGUAGE BinaryLiterals #-}

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

concatBytes :: Int8 -> Int8 -> Int8 -> Int8 -> Int32
concatBytes a b c d = (fromIntegral a ::Int32) *(2^24) + (fromIntegral b ::Int32) *(2^16) + (fromIntegral c ::Int32) *(2^8) + (fromIntegral d ::Int32)

--convert ascii msg with chars between 0-255 to the binary represention
msgToBin m = intercalate "" (map ascii256ToBin m)

--convert ascii char between 0-255 to the coresponding binary represention of 8bit
ascii256ToBin x = (take (8 - length(a_x)) ['0','0'..]) ++ a_x
  where a_x = asciiToBin x

--convert ascii char to the coresponding binary represention
asciiToBin x =  decToBin (ord x)


decToBin x = map intToDigit (reverse $ decToBin' x)
  where
    decToBin' 0 = []
    decToBin' y = let (a,b) = quotRem y 2 in [b] ++ decToBin' a
        
        
prepMsg m = b ++ "1" ++ (take k ['0','0'..]) ++ (take (64 - length(decToBin l)) ['0','0'..]) ++ decToBin l 
  where b = msgToBin m
        l = length b
        k = (448 - (l+1)) `mod` 512
    

padBlock block = (padBlock' 63 block)

padBlock' 15 b = b
padBlock' n b =  (padBlock' (n-1) b) ++ "3"

--w_j block j =  

slice from to xs = take (to - from + 1) (drop from xs)