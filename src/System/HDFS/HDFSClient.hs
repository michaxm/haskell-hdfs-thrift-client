module System.HDFS.HDFSClient where

import qualified Data.Text.Lazy as TL
import Data.Vector (toList)
import qualified GHC.IO.Handle.Types as GHC
import qualified Hadoopfs_Types as Types
import qualified Network as N
import qualified ThriftHadoopFileSystem_Client as C
import qualified Thrift.Transport as T
import qualified Thrift.Transport.Handle as H
import Thrift.Protocol.Binary (BinaryProtocol(..))

type Config = (String, Int)
type Path = String

{-
 List file names at path.
-}
hdfsListFiles :: Config -> Path -> IO [String]
hdfsListFiles config path = do
  res <- withThriftChannels config (\channels -> C.listStatus channels (asHdfsPath path))
  return $ map (TL.unpack . Types.fileStatus_path) (toList res)

{-
 Read file content of path
  - throws an exception if path does not point at a regular file.
-}
hdfsReadFile :: Config -> Path -> IO TL.Text
hdfsReadFile config path =
  withThriftChannels config (
    \channels -> withThriftHandle channels (asHdfsPath path) (
      \thriftHandle -> C.read channels thriftHandle 0 1000)) -- FIXME read more than 1 KB

-- helper utils, TODO defined module export list

asHdfsPath :: String -> Types.Pathname
asHdfsPath path = (Types.Pathname $ TL.pack $ "hdfs://"++path)

type Channels = (BinaryProtocol GHC.Handle,
                 BinaryProtocol GHC.Handle) -- TODO generalize?

withThriftChannels :: Config -> (Channels -> IO result) -> IO result
withThriftChannels (host, port) action = do
  handle <- H.hOpen (host, N.PortNumber (toEnum port))
  res <- action (BinaryProtocol handle, BinaryProtocol handle)
  T.tClose handle
  return res

withThriftHandle :: Channels -> Types.Pathname -> (Types.ThriftHandle -> IO result) -> IO result
withThriftHandle channels hdfsPath action = do
  thriftHandle <- C.open channels hdfsPath
  res <- action thriftHandle
  C.close channels thriftHandle
  return res
