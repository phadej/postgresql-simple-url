import Database.PostgreSQL.Simple
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Pulmurice.Heroku.PostgreSQL

main :: IO ()
main = defaultMain casesProps

cases :: [(String, Maybe ConnectInfo)]
cases =
  [ ("postgres:///local", Just $ ConnectInfo "" 0 "" "" "local")
  , ("mysql:///typo", Nothing)
  , ("postgres://foo:bar@example.com:2345/database", Just $ ConnectInfo "example.com" 2345 "foo" "bar" "database")
  ]

casesProps :: TestTree
casesProps = testGroup "different cases" $ map f cases
  where f (str, expected) = QC.testProperty str $ expected === parseDatabaseUrl str
