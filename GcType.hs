module GcType where
import Data.Maybe as Maybe
import Data.Map as Map

setRepositoryLang (Repository id owner name _ users) lang            = Repository id owner name lang users
setRepositoryLang (ForkedRepository id owner name _ from users) lang = ForkedRepository id owner name lang from users

setRepositoryUsers (Repository id owner name lang _) users            = Repository id owner name lang users
setRepositoryUsers (ForkedRepository id owner name lang from _) users = ForkedRepository id owner name lang from users

data Repository = Repository {
      repo_id::RepoID,
      repo_owner::String,
      repo_name::String,
      repo_lang::Map Language Int,                 
      watch_users::Map UserID Score
    }
  | ForkedRepository {
      repo_id::RepoID,
      repo_owner::String,
      repo_name::String,
      repo_lang::Map Language Int,                 
      fork_from::RepoID,
      watch_users::Map UserID Score
    } deriving Show

data User = User {
      user_id::UserID,
      watch_repos::Map RepoID Score -- (repo_id, Score)
    } deriving Show

type Score = Float
type UserID = Int
type RepoID = Int
type Language = Int

languageFromStr :: String -> Language
languageFromStr name = case Prelude.lookup name langmap of
                         Nothing -> 0
                         Just id -> id
    where
      langmap = [("Emacs", 4),  ("VimL", 3),  ("FORTRAN", 2),  ("Max/MSP", 1),  ("ActionScript", 5),  ("Visual", 11),  ("PHP", 10),
                 ("Common", 9), ("Lua", 8),   ("Haskell", 7),  ("Eiffel", 6),   ("C++", 15),          ("C", 14),       ("OCaml", 13),
                 ("Self", 12),  ("Ruby", 18), ("Java", 17),    ("D", 16),       ("Shell", 20),        ("Erlang", 19),  ("JavaScript", 27),
                 ("Perl", 26),  ("Scala", 25),("Objective-C", 24), ("Clojure", 23), ("Pure", 22),     ("Io", 21),      ("Groovy", 33),
                 ("Scheme", 32),("Nu", 31),   ("R", 30),       ("Tcl", 29),     ("Arc", 28),          ("Python", 38),  ("SuperCollider", 37),
                 ("VHDL", 36),  ("sclang", 35), ("Verilog", 34), ("Smalltalk", 39)]
              
