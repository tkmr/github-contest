module Main where
import Data.HashTable
import Data.List
import Data.Maybe
import GcType
import GcParser
import Recommend
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec

main = do loadNum  <- return $ 80000 -- 80000
          scoreNum <- return $ 1000000
          rs <- readFile "data/repos.txt"
          ls <- readFile "data/lang.txt"
          us <- readFile "data/data.txt"
          usertable <- return $ parseUsers $ take loadNum $ lines us
          repotable <- return $ parseRepos (take loadNum $ lines rs) (take loadNum $ lines ls)
          repos2    <- return $ hevyrepos $ withUsers usertable repotable

          scoreidmapper <- return $ getUidRidMapper usertable
          scores        <- return $ toScoreTables scoreidmapper repos2
                    
          --print $ take 20 $ reverse $ sortBy comp $ map score $ testlist 100 $ hasItem [100..130] $ hevyuser users
          print $ appendName repotable $ take 10 $ reverse $ sortBy comp $ map repoScore $ take scoreNum $
                    [(x, y) | x <- scores, y <- scores, (scoreTableId x) < (scoreTableId y)]

          print "end"

appendName :: Map.Map Int Repository -> [(Float, Int, Int)] -> [(Float, String, String)]
appendName repMapper list = map (\(s, a, b) -> (s, find a, find b)) list
    where
      find id = case Map.lookup id repMapper of
                  Nothing  -> ""
                  Just rep -> (repo_name rep)
                    
getUidRidMapper :: Map.Map UserID User -> Map.Map UserID [RepoID]
getUidRidMapper users = Map.map (\u -> (map fst $ Map.toList $ watch_repos u)) users
                    
repoScore (r1, r2) = ((distanceScore r1 r2), (scoreTableId r1), (scoreTableId r2))

comp (score, a, b) (score2, a2, b2) = compare score score2
                           
hevyrepos repos = filter (\repos -> (Map.size (watch_users repos)) > 2) $ map snd $ Map.toList repos
                                                   
withUsers users repos = Map.map (withUser $ transform users) repos

withUser  userids repo = case Map.lookup (repo_id repo) userids of
                           Nothing         -> repo
                           Just userscores -> setRepositoryUsers repo userscores

transform :: Map.Map UserID User -> Map.Map RepoID (Map.Map UserID Score)
transform users = foldl add Map.empty $ Map.toList users
    where
      add umap (_, (User uid repo)) = foldl (merge uid) umap $ Map.toList repo
      merge uid umap (rid, score)   = case Map.lookup rid umap of
                                        Nothing -> Map.insert rid (Map.fromList [(uid, score)]) umap
                                        Just _  -> Map.update (\m -> Just $ Map.insert uid score m) rid umap
