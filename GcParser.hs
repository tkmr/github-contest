module GcParser where
import GcType
import Data.Map as Map
import Data.HashTable
import Text.ParserCombinators.Parsec

--for debug
run :: Show a => Parser a -> String -> IO ()
run p input = case (parse p "" input) of
                Left err -> do { putStr "parse error at "
                               ; print err
                               }
                Right x  -> print x

runLex :: Show a => Parser a -> String -> IO ()
runLex p input = run (do { many whiteSpace
                         ; x <- p
                         ; eof
                         ; return x
                           }) input

runParse parser []     = []
runParse parser (x:xs) = case (parse parser "" x) of
                           Left error -> runParse parser xs
                           Right datas -> datas:(runParse parser xs)
                                          
--Parser
parseRepos :: [String] -> [String] -> IO (HashTable Int Repository)
parseRepos repo_source lang_source = newtable (runParse repository_file repo_source) (runParse language_file lang_source)
    where
      newtable repos langs = do table <- new (==) hashInt :: IO (HashTable Int Repository)
                                foldr (>>) (return table) $ (map (addrepos table langs) repos)

      addrepos table langs repository = case Prelude.lookup (repo_id repository) langs of
                                          Nothing -> insert table (repo_id repository) repository
                                          Just lang -> insert table (repo_id repository) (setRepositoryLang repository lang)


parseUsers :: [String] -> IO (HashTable Int User)
parseUsers user_source = newtable $ runParse user_file user_source
    where
      newtable users = do table <- new (==) hashInt :: IO (HashTable Int User)
                          foldr (>>) (return table) $ map (adduser table) users

      adduser table (User id repos) = do user <- Data.HashTable.lookup table id
                                         case user of
                                           Nothing                -> update table id (User id repos)
                                           Just (User _ oldrepos) -> update table id (User id (oldrepos ++ repos)) --(adjust_score $ oldrepos ++ repos))

      adjust_score repos = map (\(id, score) -> (id, (default_score / (fromIntegral $ length repos)))) repos

--Parsec
default_score = 100.0

whiteSpace = many1 (char ' ')

number :: Parser Int
number = do{ ds <- many1 digit
           ; return (read ds)
           }
         <?> "number"

repository_file :: Parser Repository
repository_file = do { id <- number
                     ; char ':'
                     ; name <- many (noneOf "/")
                     ; project <- many (noneOf ",")
                     ; char ','
                     ; many (noneOf ",")
                     ; do { try(char ',')
                          ; forked <- number
                          ; return $ ForkedRepository id name project [] forked []
                          }
                       <|> (return $ Repository id name project [] [])
                     }
                  <?> "repo.txt"

user_file :: Parser User
user_file = do { id <- number
               ; char ':'
               ; repo_id <- number
               ; return $ User id $ Map.fromList [(repo_id, default_score)]
               }
            <?> "data.txt"


language_file :: Parser (Int, [(Language, Int)])
language_file = do { repo_id <- number
                   ; char ':'
                   ; langs <- many1 language_row
                   ; return (repo_id, langs)
                   }
                <?> "lang.txt"

language_row :: Parser (Language, Int)
language_row = do { try(char ',')
                  ; lang_name <- many1 (noneOf ";")
                  ; char ';'
                  ; row <- number
                  ; return (Language lang_name, row)
                  }
               <|>
               do { lang_name <- many1 (noneOf ";")
                  ; char ';'
                  ; row <- number
                  ; return (Language lang_name, row)
                  }
