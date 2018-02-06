import Data.Bits
import Data.Int
import Data.Char
import Data.List
import Data.Word
import Numeric (showHex, showIntAtBase)

import Data.Char  (digitToInt)
import Data.Maybe (listToMaybe)
import Numeric    (readInt)

k = map (fromIntegral) [1116352408, 1899447441, 3049323471, 3921009573, 961987163, 1508970993, 2453635748, 2870763221, 3624381080, 310598401, 607225278, 1426881987, 1925078388, 2162078206, 2614888103, 3248222580, 3835390401, 4022224774, 264347078, 604807628, 770255983, 1249150122, 1555081692, 1996064986, 2554220882, 2821834349, 2952996808, 3210313671, 3336571891, 3584528711, 113926993, 338241895, 666307205, 773529912, 1294757372, 1396182291, 1695183700, 1986661051, 2177026350, 2456956037, 2730485921, 2820302411, 3259730800, 3345764771, 3516065817, 3600352804, 4094571909, 275423344, 430227734, 506948616, 659060556, 883997877, 958139571, 1322822218, 1537002063, 1747873779, 1955562222, 2024104815, 2227730452, 2361852424, 2428436474, 2756734187, 3204031479, 3329325298] :: [Word32]

h' = map (fromIntegral) [1779033703, 3144134277, 1013904242, 2773480762, 1359893119, 2600822924, 528734635, 1541459225] :: [Word32]


ch :: Word32 -> Word32 -> Word32 -> Word32
ch x y z = (x .&. y) `xor` ((complement x) .&. z)

maj :: Word32 -> Word32 -> Word32 -> Word32
maj x y z = (x .&. y) `xor` (x .&. z) `xor` (y .&. z)


sigma0 :: Word32 -> Word32
sigma0 x = (rotate x (-2)) `xor` (rotate x (-13)) `xor` (rotate x (-22))


sigma1 :: Word32 -> Word32
sigma1 x = (rotate x (-6)) `xor` (rotate x (-11)) `xor` (rotate x (-25))


sigma'0 x = (rotate a (-7)) `xor` (rotate a (-18)) `xor` (shift a (-3))
  where
    a = bin2dec x


sigma'1 x = (rotate a (-17)) `xor` (rotate a (-19)) `xor` (shift a (-10))
  where
    a = bin2dec x

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
    
decTo32Bin :: Int -> String
decTo32Bin x = take (32-len) ['0','0'..] ++ decToBin x
  where 
    len = length (decToBin x)
        
prepMsg :: String -> String        
prepMsg m = b ++ "1" ++ (take k ['0','0'..]) ++ (take (64 - length(decToBin l)) ['0','0'..]) ++ decToBin l 
  where b = msgToBin m
        l = length b
        k = (448 - (l+1)) `mod` 512
    

padBlock block = (padBlock' 16 block)


padBlock' 64 b = b
padBlock' j b = (padBlock' (j+1) (b ++ decTo32Bin (fromIntegral ((sigma'1(slice ((j-2)*32) ((j-1)*32) b)) + (bin2dec(slice ((j-7)*32) ((j-6)*32) b)) + (sigma'0(slice ((j-15)*32) ((j-14)*32) b)) + (bin2dec(slice ((j-16)*32) ((j-15)*32) b)))  :: Int)))




slice from to xs = take (to - from) (drop from xs)

bin2dec :: String -> Word32
bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where c2i c = if c == '0' then 0 else 1


update8Vals 64 block a b c d e f g h = (a, b, c, d, e, f, g, h)
update8Vals j  block a b c d e f g h = update8Vals (j+1) block (t1+t2) a b c (d+t1) e f g 
  where 
    t1 = h + (sigma1 e) + (ch e f g) + (k!!j) + (bin2dec (slice (j*32) ((j+1)*32) block))
    t2 = (sigma0 a) + (maj a b c)
    
compress i n w hs = if i==n then (concat (zipWith showHex hs (replicate 8 ""))) else compress (i+1) n w [a+hs!!0, b+hs!!1, c+hs!!2, d+hs!!3, e+hs!!4, f+hs!!5, g+hs!!6, h+hs!!7]   
  where
    (a,b,c,d,e,f,g,h) = update8Vals 0 (padBlock (slice (i*512) ((i+1)*512) w)) (h'!!0) (h'!!1) (h'!!2) (h'!!3) (h'!!4) (h'!!5) (h'!!6) (h'!!7)

sha256 msg = compress 0 n padMsg h'
  where
    padMsg = prepMsg msg
    n = (length padMsg) `div` 512






