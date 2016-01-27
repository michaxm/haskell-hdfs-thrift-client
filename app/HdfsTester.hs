import System.Environment (getArgs)

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import System.HDFS.HDFSClient

main :: IO ()
main = do
  args <- getArgs
  case args of
   ["ls", host, port, path] -> hdfsListFiles (host, read port) path >>= print
   ["blockLocations", host, port, path] -> hdfsFileDistribution (host, read port) path >>= print
   ["readlen", host, port, path] -> hdfsReadCompleteFile (host, read port) path >>= return . TL.length >>= print
   ["copyToLocal", localPath, host, port, path] -> hdfsReadCompleteFile (host, read port) path >>= TL.writeFile localPath
   _ -> error "usage: <ls|blockLocations|readlen|copyToLocal <localPath>> <host> <port> <path>"
