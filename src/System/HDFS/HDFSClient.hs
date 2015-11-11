module System.HDFS.HDFSClient (hdfsListFiles, hdfsReadCompleteFile) where

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
 List file names at path. The HDFS will return full qualified file names including protocol, host and port.
 For simplicity, these can be used for opening files etc without stripping, however you should not try to
 construct these in the same form (just use a simple file path), since port and hostname are local knowledge
 of the thrift server, strictly speaking.
-}
hdfsListFiles :: Config -> Path -> IO [String]
hdfsListFiles config path = do
  res <- withThriftChannels config (\channels -> C.listStatus channels (toThriftPath path))
  return $ map (TL.unpack . Types.fileStatus_path) (toList res)

{-
 Read file content of path
  - throws an exception if path does not point at a regular file.
-}
hdfsReadCompleteFile :: Config -> Path -> IO TL.Text
hdfsReadCompleteFile config path =
  withThriftChannels config (
    \channels -> withThriftHandle channels (toThriftPath path) (
      \thriftHandle -> C.read channels thriftHandle 0 1000)) -- FIXME read more than 1 KB

-- internals

toThriftPath :: String -> Types.Pathname
toThriftPath = Types.Pathname . TL.pack

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
