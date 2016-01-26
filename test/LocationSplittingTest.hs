import Test.Hspec

import System.HDFS.InternalUtils

main :: IO ()
main = hspec $ do
  describe "System.HDFS.InternalUtils" $ do
        context "Protocol/path splitting" $ do
            it "splits hdfs path" $ do
              splitLocation "hdfs://localhost:8020/path" `shouldBe` ("hdfs://localhost:8020", "/path")
