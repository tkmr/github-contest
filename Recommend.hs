module Recommend where
import Data.Maybe
import GcType
import GcParser

data ScoreTable = ScoreTable { id::Int, scores::[(Int, Float)] }

class Similar a where
    toScoreTable :: a -> ScoreTable    
    similarDistance :: a -> a -> Float
    similarDistance x y = similar_distance (toScoreTable x) (toScoreTable y)
                
instance Similar User where
    toScoreTable user = ScoreTable (user_id user) (watch_repos user)

instance Similar Repository where
    toScoreTable repos = ScoreTable (repo_id repos) (mapMaybe (myScore $ repo_id repos) (watch_users repos))
                            where
                              myScore rid user = case Prelude.lookup rid (watch_repos user) of
                                                   Nothing    -> Nothing
                                                   Just score -> Just ((user_id user), score)

similar_distance :: ScoreTable -> ScoreTable -> Float
similar_distance sa sb = score_distance
                           (sameList sa sb)
                           (baseScore (length $ scores sa) (length $ scores sb) (length $ sameList sa sb))

score_distance :: [((Int, Float), (Int, Float))] -> Float -> Float
score_distance []   _    = 0.0
score_distance list base = base / ((sqrt  $ sum $ [(scoreA - scoreB)^2 | ((_, scoreA), (_, scoreB)) <- list ]) + 1.0)

                           
sameList :: ScoreTable -> ScoreTable -> [((Int, Float), (Int, Float))]
sameList (ScoreTable _ scoresA) (ScoreTable _ scoresB) = mapMaybe (sameScore scoresA) scoresB
    where
      sameScore scoresA (b_id, b_score) = case Prelude.lookup b_id scoresA of
                                            Nothing      -> Nothing
                                            Just a_score -> Just ((b_id, a_score), (b_id, b_score))

baseScore a b same = default_score * (fromIntegral (same * 2) / fromIntegral (a + b))
