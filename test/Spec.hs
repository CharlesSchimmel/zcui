import           Test.Hspec

import           Zcui.Test.Archive
import           Zcui.Test.Convert

main :: IO ()
main = hspec $ do
    archiveTests
    convertTests

