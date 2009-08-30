module Main where
import Data.HashTable
import Data.List
import Data.Maybe
import GcType
import GcParser
import Recommend
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec

-- print $ appendName repotable $ take 10 $ reverse $ sortBy comp $ map repoScore $ take scoreNum $ [(x, y) | x <- scores, y <- scores, (scoreTableId x) < (scoreTableId y)]
    
main = do loadNum  <- return $ 200000
          scoreNum <- return $ 100000000000000
          rs <- readFile "data/repos.txt"
          ls <- readFile "data/lang.txt"
          us <- readFile "data/data.txt"
          ts <- readFile "data/test.txt"
                
          usertable <- return $ parseUsers $ take loadNum $ lines us
          repotable <- return $ parseRepos (take loadNum $ lines rs) (take loadNum $ lines ls)
          repos2    <- return $ hevyrepos $ withUsers usertable repotable

          scoreidmapper <- return $ getUidRidMapper usertable
          scores        <- return $ toScoreTables scoreidmapper repos2

          similarScoreMap <- return $ foldl addtomap Map.empty $ filter (\(s, _, _) -> s >= 0.01) $  map repoScore $ take scoreNum $ 
                             [(x, y) | x <- scores, y <- scores, (scoreTableId x) < (scoreTableId y)]

          -------
          testids <- return $ parseTestIds ts                             
          result  <- return $ map (recommend usertable similarScoreMap) testids

          _ <- writeFile "results.txt" $ outputResult similarScoreMap result
          print "end"
    where
      outputResult sc []                = []
      outputResult sc ((uid, repos):xs) = (show uid) ++ ":" ++ (joinByStr (fillresult $ map fst $ take 10 repos) ",") ++ "\n" ++ (outputResult sc xs)
          where
            fillresult l = l ++ (take (10 - (length l)) $ map fst $ Map.toList sc)

--joinByStr :: [Show] -> String -> String
joinByStr []     _   = []
joinByStr (x:[]) _   = (show x)
joinByStr (x:xs) sep = (show x) ++ sep ++ (joinByStr xs sep)
                                 
recommend usermap scoremap uid = case Map.lookup uid usermap of
                                   Nothing             -> (uid, [])
                                   Just (User _ repos) -> (uid, (reverse $ sortBy compareScore $ foldl (++) [] $ map snd $ Map.toList $ Map.mapMaybeWithKey getScore repos))
    where
      compareScore (_, sa) (_, sb) = compare sa sb

      getScore :: Int -> Float -> Maybe [(Int, Float)]
      getScore rid orgscore = case Map.lookup rid scoremap of
                                Nothing     -> Nothing
                                Just scores -> Just $ map (\(id, sc) -> (id, (sc * orgscore))) scores
                
addtomap :: Map.Map Int [(Int, Float)] -> (Float, Int, Int) -> Map.Map Int [(Int, Float)]
addtomap map (score, aid, bid) = case Map.lookup aid map of
                                   Nothing       -> Map.insert aid [(bid, score)] map
                                   Just scoremap -> Map.update (\x -> Just $ (bid, score):x) aid map

parseTestIds :: String -> [Int]
parseTestIds []   = []
parseTestIds "\n" = []
parseTestIds str = (read $ fst parsed):(parseTestIds (tail $ snd parsed))
    where
      parsed = break (\i -> i == '\n') str

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
                           
hevyrepos repos = filter (\repos -> (Map.size (watch_users repos)) > 12) $ map snd $ Map.toList repos
                                                   
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
