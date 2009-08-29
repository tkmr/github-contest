module Recommend where
import Data.Maybe
import GcType
import GcParser
import Data.List
import System.IO.Unsafe

--util-------------------    
randomize :: [Int] -> [Int]
randomize list = sortBy comp list
                 where comp a b = compare (random ((a - b) * b)) (random ((a + b) * a))
                       random n = mod (n ^ 3) 9876823 

treeSize :: Maybe ScoreTree -> Int
treeSize Nothing                    = 0
treeSize (Just (ScoreTree _ _ l r)) = 1 + (treeSize l) + (treeSize r)

buildtree :: [(Int, Float)] -> ScoreTree
buildtree ((id, score):xs) = ScoreTree id score (lnodes xs id) (rnodes xs id)
    where
      lnodes list sepid = treeOrNothing [(id, score) | (id, score) <- list, id < sepid]
      rnodes list sepid = treeOrNothing [(id, score) | (id, score) <- list, id > sepid]
                          
      treeOrNothing :: [(Int, Float)] -> Maybe ScoreTree
      treeOrNothing [] = Nothing
      treeOrNothing x  = Just $ buildtree x

toRandom :: [a] -> [a]
toRandom list = map (\(_, v) -> v) $ sortBy comp $ zip (randomkey list) list
    where
      comp (a, _) (b, _) = compare a b
      randomkey list     = randomize $ randomize $ [0..(length list)]

-------------------------    
data ScoreTable = ScoreTable { scores::[(Int, Float)] }
data ScoreTree  = ScoreTree { scoretree_id::Int, scoretree_value::Float, left::Maybe ScoreTree, right::Maybe ScoreTree }

class Similar a where
    toScoreTable :: a -> ScoreTable
                    
    toScoreTree :: a -> ScoreTree
    toScoreTree x = transform $ toScoreTable x
        where
          transform (ScoreTable scores) = buildtree $ toRandom scores
              
    similarDistance :: a -> a -> Float
    similarDistance x y = distanceScore (toScoreTree x) (toScoreTree y)

instance Similar User where
    toScoreTable user = ScoreTable (watch_repos user)

instance Similar Repository where
    toScoreTable repos = ScoreTable (mapMaybe (myScore $ repo_id repos) (watch_users repos))
                            where
                              myScore rid user = case Prelude.lookup rid (watch_repos user) of
                                                   Nothing    -> Nothing
                                                   Just score -> Just ((user_id user), score)

---------------------------                                                                 
distanceScore :: ScoreTree -> ScoreTree -> Float
distanceScore sa sb = base / ((sqrt  $ sum $ [(scoreA - scoreB)^2 | ((_, scoreA), (_, scoreB)) <- sameCouples sa sb ]) + 1.0)
    where
      base               = (baseScore (treeSize $ Just sa) (treeSize $ Just sb) (length $ sameCouples sa sb))
      baseScore a b same = default_score * (fromIntegral (same * 2) / fromIntegral (a + b))

-----
sameCouples :: ScoreTree -> ScoreTree -> [((Int, Float), (Int, Float))]
sameCouples scoresA scoresB = tryAll (findFrom $ Just scoresA) (Just scoresB)
    where
      tryAll :: (Int -> Maybe ScoreTree) -> Maybe ScoreTree -> [((Int, Float), (Int, Float))]
      tryAll _     Nothing                                = []
      tryAll findA (Just (ScoreTree id value left right)) = 
          case findA id of
            Nothing                            -> (tryAll findA left) ++ (tryAll findA right)
            Just    (ScoreTree idA valueA _ _) -> ((id, value), (idA, valueA)):((tryAll findA left) ++ (tryAll findA right))
                            
      findFrom :: Maybe ScoreTree -> Int -> Maybe ScoreTree
      findFrom Nothing  _                                    = Nothing
      findFrom (Just (ScoreTree id value left right)) target = case id `compare` target of
                                                               EQ -> Just (ScoreTree id value Nothing Nothing)
                                                               GT -> (findFrom left target)
                                                               LT -> (findFrom right target)
