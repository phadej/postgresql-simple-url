-----------------------------------------------------------------------------
-- |
-- Module      :  Pulmurice.Heroku.PostgreSQL
-- Copyright   :  2014 © Futurice OY, Oleg Grenrus
-- License     :  MIT (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
-- Stability   :  experimental
-- Portability :  non-portable (GHC only)
--
-- parse DATABASE_URL into ConnectInfo
--
----------------------------------------------------------------------------
module Pulmurice.Heroku.PostgreSQL (parseDatabaseUrl) where

import Control.Applicative
import Data.List.Split
import Database.PostgreSQL.Simple
import Network.URI

parseDatabaseUrl :: String -> Maybe ConnectInfo
parseDatabaseUrl databaseUrl = parseURI databaseUrl >>= f
  where f uri | uriScheme uri /= "postgres:" = Nothing
              | otherwise                    = ($ emptyInfo) <$> mkConnectInfo uri

type ConnectInfoChange = ConnectInfo -> ConnectInfo

emptyInfo :: ConnectInfo
emptyInfo = ConnectInfo "" 0 "" "" ""

mkConnectInfo :: URI -> Maybe ConnectInfoChange
mkConnectInfo uri = case uriPath uri of
                           ('/' : rest) | not (null rest) -> Just $ uriParameters uri
                           _                              -> Nothing

uriParameters :: URI -> ConnectInfoChange
uriParameters uri = (\info -> info { connectDatabase = tail $ uriPath uri }) . maybe id uriAuthParameters (uriAuthority uri)

dropLast :: [a] -> [a]
dropLast []     = []
dropLast [_]    = []
dropLast (x:xs) = x : dropLast xs

uriAuthParameters :: URIAuth -> ConnectInfoChange
uriAuthParameters uriAuth = port . host . auth
  where port = case uriPort uriAuth of
                 (':' : p) -> \info -> info { connectPort = read p }
                 _         -> id
        host = case uriRegName uriAuth of
                 "" -> id
                 h  -> \info -> info { connectHost = h }
        auth = case splitOn ":" (uriUserInfo uriAuth) of
                 [""]   -> id
                 [u]    -> \info -> info { connectUser = dropLast u }
                 [u, p] -> \info -> info { connectUser = u, connectPassword = dropLast p }
                 _      -> id
