module Main where
import Data.HashTable
import GcType
import GcParser
import Text.ParserCombinators.Parsec

main = do rs <- readFile "data/repos.txt"
          ls <- readFile "data/lang.txt"
          repotable <- parseRepos (take 100 $ lines rs) (take 100 $ lines ls)
          repos <- toList repotable
          print repos
          --xs <- readFile "data/data.txt"
          --usertable <- parseUsers $ take 100 $ lines xs
          --users <- toList usertable
          --print users
