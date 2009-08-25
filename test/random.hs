import Data.List
import System.IO.Unsafe
import System.Random
    
randomize :: [Int] -> [Int]
randomize list = sortBy comp list
                 where comp a b = compare (random (a * b)) (random (b * a))
                       random n = unsafePerformIO $ getStdRandom (randomR (1,n))
                                  
                                  