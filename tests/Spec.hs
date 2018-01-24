import Test.Tasty

import Test.Regex.Internal.NFA
import Test.Regex.Internal.Graph

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "slang-compiler" $
    [
        graphTests,
        nfaTests
    ]