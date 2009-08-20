module GcType where

data Repository = Repository {
      repo_id::Int,
      repo_owner::String,
      repo_name::String,
      repo_lang::[(Language, Int)]
    }
  | ForkedRepository {
      repo_id::Int,
      repo_owner::String,
      repo_name::String,
      repo_lang::[(Language, Int)],
      fork_from::Int
    } deriving Show

data Language = Language {
      lang_name::String
    } deriving Show

data User = User {
      user_id::Int,
      watch_repos::[Int]
    } deriving Show

