module Main where
import Data.List
import Data.Time.Clock
    
check f = do from <- getCurrentTime
             _    <- f ()
             to   <- getCurrentTime
             return $ to `diffUTCTime` from

checkPrelude n list = check (\_ -> print $ Prelude.lookup n list)
