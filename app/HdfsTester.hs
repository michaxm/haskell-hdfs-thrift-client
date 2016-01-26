import System.Environment (getArgs)

import System.HDFS.HDFSClient

main :: IO ()
main = do
  args <- getArgs
  case args of
   [host, port, path] -> hdfsListFiles (host, read port) path >>= print
   _ -> error "usage: <host> <port> <path>"
