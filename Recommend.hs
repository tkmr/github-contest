module Recommend where
import Data.Maybe
import GcType
import GcParser
import Data.List
import System.IO.Unsafe
import System.Random

--util-------------------    
randomize :: [Int] -> [Int]
randomize list = sortBy comp list
                 where comp a b = compare (random (a * b)) (random (a * b))
                       random n = unsafePerformIO $ getStdRandom (randomR (1,n))

treeSize :: ScoreTree -> Int
treeSize (ScoreTree _ _ l r) = 1 + (size l) + (size r)

buildtree :: [(Int, Float)] -> ScoreTree
buildtree []             = ScoreTree 
buildtree (id, score):xs = ScoreTree id score (lnodes xs id) (rnodes xs id)
    where
      lnodes list sepid = buildtree [(id, score) | (id, score) <- list, id < sepid]
      rnodes list sepid = buildtree [(id, score) | (id, score) <- list, id > sepid]
                               
-------------------------    
data ScoreTable = ScoreTable { scores::[(Int, Float)] }
data ScoreTree  = ScoreTree  { scoretree_id::Int, scoretree_value::Float, left::ScoreTree, right::ScoreTree }
                
class Similar a where
    toScoreTable :: a -> ScoreTable
                    
    toScoreTree :: a -> ScoreTree
    toScoreTree x = transform $ toScoreTable x
        where
          transform (ScoreTable scores) = buildtree $ toRandom scores
          toRandom list = map (\(_, v) -> v) $ sortBy comp $ zip (randomkey list) list
          comp (a, _) (b, _) = compare a b
          randomkey list = randomize $ randomize $ map (\(i, _) -> i) list
              
    similarDistance :: a -> a -> Float
    similarDistance x y = similar_distance (toScoreTree x) (toScoreTtree y)

instance Similar User where
    toScoreTable user = ScoreTable (watch_repos user)

instance Similar Repository where
    toScoreTable repos = ScoreTable (mapMaybe (myScore $ repo_id repos) (watch_users repos))
                            where
                              myScore rid user = case Prelude.lookup rid (watch_repos user) of
                                                   Nothing    -> Nothing
                                                   Just score -> Just ((user_id user), score)

---------------------------                                                                 
similarDistance :: ScoreTree -> ScoreTree -> Float
similarDistance sa sb = distanceScore
                          (sameCouples sa sb)
                          (baseScore (treeSize sa) (treeSize sb) (length $ sameCouples sa sb))

distanceScore :: [((Int, Float), (Int, Float))] -> Float -> Float
distanceScore []   _    = 0.0
distanceScore list base = base / ((sqrt  $ sum $ [(scoreA - scoreB)^2 | ((_, scoreA), (_, scoreB)) <- list ]) + 1.0)

sameCouples :: ScoreTree -> ScoreTree -> [((Int, Float), (Int, Float))]
sameCouples scoresA scoresB = tryAll (findFrom scoresA) scoresB
    where
      tryAll _ () = []
      tryAll find (ScoreTree id value left right) =
          case find id of
            Nothing                            -> (tryAll f left) ++ (tryAll f right)
            Just    (ScoreTree idA valueA _ _) -> ((id, value), (idA, valueA)):((tryAll f left) ++ (tryAll f right))
                            
      findFrom :: ScoreTree -> Int -> Maybe ScoreTree
      findFrom ()                              target = Nothing
      findFrom (ScoreTree id value left right) target = case id `compare` target of
                                                          EQ -> Just (ScoreTree id value left right)
                                                          GT -> (findFrom left target) 
                                                          LT -> (findFrom right target) 
          
baseScore a b same = default_score * (fromIntegral (same * 2) / fromIntegral (a + b))
