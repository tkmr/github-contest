module Main where
import Data.HashTable
import Data.List
import Data.Maybe
import GcType
import GcParser
import Recommend
import Text.ParserCombinators.Parsec

main = do rs <- readFile "data/repos.txt"
          ls <- readFile "data/lang.txt"
          us <- readFile "data/data.txt"
          repotable <- parseRepos (take 10000 $ lines rs) (take 10000 $ lines ls)
          usertable <- parseUsers $ take 10000 $ lines us

          users <- toList usertable
          repos <- toList repotable
          repos2 <- withUsers users repos
                    
          --print $ take 20 $ reverse $ sortBy comp $ map score $ testlist 100 $ hasItem [100..130] $ hevyuser users
          print $ take 10 $ reverse $ sortBy comp $ map repoScore $ take 10000 $
                    [(x, y) | x <- (hevyrepos repos2), y <- (hevyrepos repos2), (repo_id x) < (repo_id y)]

                                        
    where
      hevyrepos = filter (\repos -> (length (watch_users repos)) > 50)
      
      repoScore (r1, r2) = ((similarDistance r1 r2), (repo_name r1), (repo_name r2))
                 
      withUsers users repos = do return (map (withUser users) repos)
      withUser users (_, repo) = addWatchUsers repo $ map (\(_, user) -> user) users
      
      hasItem []        _    = []
      hasItem (rid:ids) list = (filter (\(User id repos) -> isJust $ Prelude.lookup rid repos) list) ++ (hasItem ids list)

      score (userA, userB) = (similarDistance userA userB, (user_id userA), (user_id userB))

      hevyuser = filter (\(User id repos) -> (length repos) > 20) . map (\(_, user) -> user)

      comp (score, a, b) (score2, a2, b2) = compare score score2

      testlist n list =  [(a,b) | a <- (take n list), b <- (take n list), (user_id a) /= (user_id b), (user_id a) < (user_id b)]