import System.Environment (getArgs)

import qualified Data.Text.Lazy as TL
import System.HDFS.HDFSClient

main :: IO ()
main = do
  args <- getArgs
  case args of
   ["ls", host, port, path] -> hdfsListFiles (host, read port) path >>= print
   ["readlen", host, port, path] -> hdfsReadCompleteFile (host, read port) path >>= return . TL.length >>= print
   _ -> error "usage: <ls|readlen> <host> <port> <path>"
