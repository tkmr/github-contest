module Main where
import Data.HashTable
import Data.List
import Data.Maybe
import GcType
import GcParser
import Recommend
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec

main = do loadNum  <- return $ 20000
          scoreNum <- return $ 100000
                     
          rs <- readFile "data/repos.txt"
          ls <- readFile "data/lang.txt"
          us <- readFile "data/data.txt"
          usertable <- return $ parseUsers $ take loadNum $ lines us
          repotable <- return $ parseRepos (take loadNum $ lines rs) (take loadNum $ lines ls)
          repos2    <- return $ withUsers usertable repotable
                    
          --print $ take 20 $ reverse $ sortBy comp $ map score $ testlist 100 $ hasItem [100..130] $ hevyuser users
          print $ take 10 $ reverse $ sortBy comp $ map repoScore $ take scoreNum $
                    [(x, y) | x <- (hevyrepos repos2), y <- (hevyrepos repos2), (repo_id x) < (repo_id y)]
                                        
    where
      hevyrepos repos = filter (\repos -> (Map.size (watch_users repos)) > 10) $ map snd $ Map.toList repos
      
      repoScore (r1, r2) = ((similarDistance r1 r2), (repo_name r1), (repo_name r2))
                 
      withUsers users repos = Map.map (withUser $ transform users) repos

      transform :: Map.Map UserID User -> Map.Map RepoID (Map.Map UserID Score)
      transform users = foldl add Map.empty $ Map.toList users
          where
            add umap (_, (User uid repo)) = foldl (merge uid) umap $ Map.toList repo
            merge uid umap (rid, score)   = case Map.lookup rid umap of
                                              Nothing -> Map.insert rid (Map.fromList [(uid, score)]) umap
                                              Just _  -> Map.update (\m -> Just $ Map.insert uid score m) rid umap

      withUser  userids repo = case Map.lookup (repo_id repo) userids of
                                 Nothing         -> repo
                                 Just userscores -> setRepositoryUsers repo userscores

      ----------------------------------------------------------------------------
      hasItem []        _    = []
      hasItem (rid:ids) list = (filter (\(User id repos) -> isJust $ Map.lookup rid repos) list) ++ (hasItem ids list)

      score (userA, userB) = (similarDistance userA userB, (user_id userA), (user_id userB))

      hevyuser = filter (\(User id repos) -> (length $ Map.toList repos) > 20) . map (\(_, user) -> user)

      comp (score, a, b) (score2, a2, b2) = compare score score2

      testlist n list =  [(a,b) | a <- (take n list), b <- (take n list), (user_id a) /= (user_id b), (user_id a) < (user_id b)]