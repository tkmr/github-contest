module Recommend where
import qualified Bloom as Bloom
import Data.Maybe
import GcType
import GcParser
import Data.List
import qualified Data.Map as Map    
import System.IO.Unsafe

data ScoreTable = ScoreTable { scoreTableId::Int, scores::Map.Map Int Float, related_ids_filter::Bloom.BloomFilter }

class Similar a where
    toScoreTable  :: Map.Map Int [Int] -> a -> ScoreTable
                     
    toScoreTables :: Map.Map Int [Int] -> [a] -> [ScoreTable]
    toScoreTables mapper = map (toScoreTable mapper)
                
--instance Similar User where
--    toScoreTable user  = ScoreTable (user_id user)  (watch_repos user)

instance Similar Repository where
    toScoreTable users repos = ScoreTable (repo_id repos) (watch_users repos) (makefilter users (map fst $ Map.toList $ watch_users repos))

makefilter :: Map.Map Int [Int] -> [Int] -> Bloom.BloomFilter
makefilter mapper scoreids = Bloom.initFilter $ reduce $ mapMaybe (\n -> find n) scoreids
    where
      reduce = foldl (++) []
      find id = Map.lookup id mapper

---------------------------                                                                 
distanceScore :: ScoreTable -> ScoreTable -> Float
distanceScore sa sb = case Bloom.isExists (related_ids_filter sb) (scoreTableId sa) of
                        False -> 0.0
                        True  -> base / ((sqrt  $ sum $ [(scoreA - scoreB)^2 | ((_, scoreA), (_, scoreB)) <- couples ]) + 1.0)
    where
      couples            = sameCouples sa sb
      base               = (baseScore (Map.size $ scores sa) (Map.size $ scores sb) (length $ sameCouples sa sb))
      baseScore a b same = default_score * (fromIntegral (same * 2) / fromIntegral (a + b))

sameCouples :: ScoreTable -> ScoreTable -> [((Int, Float), (Int, Float))]
sameCouples (ScoreTable aId a _) (ScoreTable bId b _) = map snd $ Map.toList $ Map.mapMaybeWithKey same a
    where
      same aKey aValue = case Map.lookup aKey b of
                           Nothing     -> Nothing
                           Just bValue -> Just ((aKey, aValue), (aKey, bValue))