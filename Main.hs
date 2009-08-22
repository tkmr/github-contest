module Main where
import Data.HashTable
import GcType
import GcParser
import Text.ParserCombinators.Parsec

main = do rs <- readFile "data/repos.txt"
          ls <- readFile "data/lang.txt"
          us <- readFile "data/data.txt"
          repotable <- parseRepos (take 100 $ lines rs) (take 100 $ lines ls)
          usertable <- parseUsers $ take 100 $ lines xs

          users <- toList usertable
          print users
