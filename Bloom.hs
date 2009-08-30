module Bloom where
import Data.Word (Word32, Word8)
import Data.Bits
    
type BloomFilter = [Word32]

class Bloom a where
    initFilter :: [a] -> BloomFilter
    initFilter (x:[]) = (bloomHash x)
    initFilter (x:xs) = orBitOctets (bloomHash x) (initFilter xs)

    bloomHash :: a -> BloomFilter

    isExists :: BloomFilter -> a -> Bool
    isExists filter x = (sameBitOctets (bloomHash x)  filter)

instance Bloom Int where
    bloomHash x = hash x

hash :: Int -> [Word32]
hash x | x <= 1024 = fill $ reverse $ convert x 1
hash x             = hash (x `mod` 1024)

fill ls = (zerolist (32 - (length ls))) ++ ls

convert :: Int -> Int -> [Word32]
convert x i | (x - i) < 32 = (shiftL (1::Word32) (x - i)):[]
convert x i                = 0:(convert x (i + 32))

zerolist :: Int -> [Word32]
zerolist 0         = []
zerolist n | n < 0 = []             
zerolist n         = 0:(zerolist (n - 1))

-----
sameBitOctets :: [Word32] -> [Word32] -> Bool
sameBitOctets []     []     = True
sameBitOctets []     _      = False                              
sameBitOctets _      []     = False
sameBitOctets (a:as) (b:bs) = if (a .&. b) == a
                              then sameBitOctets as bs
                              else False
                  
orBitOctets  :: [Word32] -> [Word32] -> [Word32]
orBitOctets  a      []     = a                
orBitOctets  []     b      = b
orBitOctets  (a:as) (b:bs) = (a .|. b):(orBitOctets as bs)
                
andBitOctets :: [Word32] -> [Word32] -> [Word32]
andBitOctets _      []     = []                
andBitOctets []     _      = []
andBitOctets (a:as) (b:bs) = (a .&. b):(andBitOctets as bs)

-----                             
intToOctet :: Int -> [Word8]
intToOctet a = map (\n -> (fromIntegral (shiftR a n))) [24, 16, 8, 0]
          