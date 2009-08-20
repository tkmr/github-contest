module GcParser where
import GcType
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
                          ; return $ ForkedRepository id name project [] forked
                          }
                       <|> (return $ Repository id name project [])
                     }
                  <?> "repo.txt"

user_file :: Parser User
user_file = do { id <- number
               ; char ':'
               ; repo_id <- number
               ; return $ User id [repo_id]
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
