module Bloom where
import qualified Data.Digest.SHA256 as SHA
import Data.Word (Word32, Word8)
import Data.Bits
--import Codec.Utils (Octet)
    
type BloomFilter = Integer

class Bloom a where
    initFilter :: [a] -> BloomFilter
    initFilter []     = 0
    initFilter (x:xs) = (bloomHash x) .|. (initFilter xs)

    bloomHash :: a -> BloomFilter

    isExists :: BloomFilter -> a -> Bool
    isExists filter x = ((bloomHash x) .&. filter) == (bloomHash x)

instance Bloom Int where
    bloomHash x = octetToInteger $ SHA.hash $ intToOctet x

octetToInteger :: [Word8] -> Integer
octetToInteger list = toi (reverse list) 0
    where
      toi []     _ = 0
      toi (x:xs) n = (shiftL (fromIntegral x) n) .|. (toi xs (n + 8))

intToOctet :: Int -> [Word8]
intToOctet a = map (\n -> (fromIntegral (shiftR a n))) [24, 16, 8, 0]
          