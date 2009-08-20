module Main where
import GcType
import GcParser
import Text.ParserCombinators.Parsec

main = do xs <- readFile "data/repos.txt"
          print $ map doParse $ take 10 $ lines xs
    where doParse x = parse repository "" x
--          case (parse repository "" $ take 1 xs) of
--            Left err = print err
--            Right x  = print x
