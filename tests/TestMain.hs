import Test.Tasty

import qualified TestPlugin.DBus.Serialization


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ TestPlugin.DBus.Serialization.tests
                          ]
