module Bloom where
import Data.Word (Word32, Word8)
import Data.Bits
    
type BloomFilter = Integer

class Bloom a where
    initFilter :: [a] -> BloomFilter
    initFilter (x:[]) = (bloomHash x)
    initFilter (x:xs) = (bloomHash x) .|. (initFilter xs)

    bloomHash :: a -> BloomFilter
                 
    isExists :: BloomFilter -> a -> Bool
    isExists filter x = ((bloomHash x) .&. filter) == (bloomHash x)

instance Bloom Int where
    bloomHash x = hash x

hash :: Int -> BloomFilter
hash x | x <= 500 = bit (x - 1)
hash x            = hash (x `mod` 500)
                     
-----
fill ls = (zerolist (16 - (length ls))) ++ ls
            
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
          