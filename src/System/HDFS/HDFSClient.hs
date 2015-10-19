module System.HDFS.HDFSClient where

import qualified Data.Text.Lazy as TL
import Data.Vector (toList)
import Hadoopfs_Types
import Network
import ThriftHadoopFileSystem_Client
import Thrift.Transport.Handle
import Thrift.Protocol.Binary

type Config = (String, Int)
type Path = String

hdfsListStatus :: Config -> Path -> IO [String]
hdfsListStatus (host, port) path = do
  handle <- hOpen (host, PortNumber (toEnum port))
  res <- listStatus (BinaryProtocol handle, BinaryProtocol handle) (Pathname $ TL.pack $ "hdfs://"++path)
  return $ map (TL.unpack . fileStatus_path) (toList res)
