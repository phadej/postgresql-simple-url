import Database.PostgreSQL.Simple
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Database.PostgreSQL.Simple.URL

main :: IO ()
main = defaultMain casesProps

cases :: [(String, Maybe ConnectInfo)]
cases =
  [ ("postgres:///local", Just $ defaultConnectInfo
    { connectHost = "", connectDatabase = "local"} )
  , ("postgres://localhost/local", Just $ defaultConnectInfo
    { connectHost = "localhost", connectDatabase = "local"} )
  , ("postgres://user@/local", Just $ defaultConnectInfo
    { connectHost = "", connectUser = "user", connectDatabase = "local"} )
  , ("mysql:///typo", Nothing)
  , ("postgres://foo:bar@example.com:2345/database", Just $
    ConnectInfo "example.com" 2345 "foo" "bar" "database")
  ]

casesProps :: TestTree
casesProps = testGroup "different cases" $ map f cases
  where f (str, expected) = QC.testProperty str $ once $ property $expected == parseDatabaseUrl str
