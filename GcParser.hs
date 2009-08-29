module GcParser where
import GcType
import qualified Data.Map as Map
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
parseRepos :: [String] -> [String] -> Map.Map Int Repository
parseRepos repo_source lang_source = foldl (addrepos $ Map.fromList langs) Map.empty repos
    where
      langs = runParse language_file lang_source
      repos = runParse repository_file repo_source

      addrepos langmap reposmap repository = case Map.lookup (repo_id repository) langmap of
                                               Nothing   -> Map.insert (repo_id repository) repository reposmap
                                               Just lang -> Map.insert (repo_id repository) (setRepositoryLang repository lang) reposmap

parseUsers :: [String] -> Map.Map Int User
parseUsers user_source = foldl adduser Map.empty $ runParse user_file user_source
    where
      adduser usermap user = case Map.lookup (user_id user) usermap of
                               Nothing                -> Map.insert (user_id user) user usermap
                               Just (User _ oldrepos) -> Map.update (merge $ watch_repos user) (user_id user) usermap
                                                         
      merge newrepos (User id oldrepos) = Just $ User id (Map.union oldrepos newrepos)

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
                          ; return $ ForkedRepository id name project Map.empty forked Map.empty
                          }
                       <|> (return $ Repository id name project Map.empty Map.empty)
                     }
                  <?> "repo.txt"

user_file :: Parser User
user_file = do { id <- number
               ; char ':'
               ; repo_id <- number
               ; return $ User id $ Map.fromList [(repo_id, default_score)]
               }
            <?> "data.txt"


language_file :: Parser (Int, Map.Map Language Int)
language_file = do { repo_id <- number
                   ; char ':'
                   ; langs <- many1 language_row
                   ; return (repo_id, Map.fromList langs)
                   }
                <?> "lang.txt"

language_row :: Parser (Language, Int)
language_row = do { try(char ',')
                  ; lang_name <- many1 (noneOf ";")
                  ; char ';'
                  ; row <- number
                  ; return (languageFromStr lang_name, row)
                  }
               <|>
               do { lang_name <- many1 (noneOf ";")
                  ; char ';'
                  ; row <- number
                  ; return (languageFromStr lang_name, row)
                  }
