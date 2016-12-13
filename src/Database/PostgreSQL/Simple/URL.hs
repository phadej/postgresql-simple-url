-----------------------------------------------------------------------------
-- |
-- Module      :  Database.PostgreSQL.Simple.URL
-- Copyright   :  2014, 2015 © Futurice OY, Oleg Grenrus
-- License     :  MIT (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Database.PostgreSQL.Simple.URL (parseDatabaseUrl, uriToConnectInfo) where

import Control.Applicative
import Data.List.Split
import Database.PostgreSQL.Simple
import Network.URI
import Prelude

-- | Parse string url into `ConnectInfo`.
--
-- > parseDatabaseURL "postgres://foo:bar@example.com:2345/database" == ConnectInfo "example.com" 2345 "foo" "bar" "database"
parseDatabaseUrl :: String -> Maybe ConnectInfo
parseDatabaseUrl databaseUrl = parseURI databaseUrl >>= uriToConnectInfo

uriToConnectInfo :: URI -> Maybe ConnectInfo
uriToConnectInfo uri
  | uriScheme uri /= "postgres:" = Nothing
  | otherwise                    = ($ defaultConnectInfo) <$> mkConnectInfo uri

type ConnectInfoChange = ConnectInfo -> ConnectInfo

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
                 h  -> \info -> info { connectHost = h }
        auth = case splitOn ":" (uriUserInfo uriAuth) of
                 [""]   -> id
                 [u]    -> \info -> info { connectUser = dropLast u }
                 [u, p] -> \info -> info { connectUser = u, connectPassword = dropLast p }
                 _      -> id
