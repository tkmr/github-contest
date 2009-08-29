module GcType where
import Data.Maybe as Maybe
import Data.Map as Map

setRepositoryLang (Repository id owner name _ users) lang            = Repository id owner name lang users
setRepositoryLang (ForkedRepository id owner name _ from users) lang = ForkedRepository id owner name lang from users

addRepositoryUsers (Repository id owner name lang oldusers) users            = Repository id owner name lang (users ++ oldusers)
addRepositoryUsers (ForkedRepository id owner name lang from oldusers) users = ForkedRepository id owner name lang from (users ++ oldusers)

addWatchUsers :: Repository -> [User] -> Repository
addWatchUsers repo users = addRepositoryUsers repo $ Maybe.mapMaybe (hasRepos (repo_id repo)) users
    where
      hasRepos rid user = case Map.lookup rid (watch_repos user) of
                            Nothing -> Nothing
                            Just _  -> Just user

data Repository = Repository {
      repo_id::Int,
      repo_owner::String,
      repo_name::String,
      repo_lang::Map Language Int,
      watch_users::[User]
    }
  | ForkedRepository {
      repo_id::Int,
      repo_owner::String,
      repo_name::String,
      repo_lang::Map Language Int,                 
      fork_from::Int,
      watch_users::[User]                 
    } deriving Show


data Language = Language {
      lang_name::String
    } deriving Show

data User = User {
      user_id::Int,
      watch_repos::Map Int Float -- (repo_id, Score)
    } deriving Show

