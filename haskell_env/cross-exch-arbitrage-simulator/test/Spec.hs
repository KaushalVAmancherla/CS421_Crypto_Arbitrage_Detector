import Test.Tasty

import Tests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit Tests" [Tests.firstTest, Tests.secondTest, Tests.thirdTest, Tests.fourthTest]