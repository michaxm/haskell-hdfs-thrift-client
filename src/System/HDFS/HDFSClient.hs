module System.HDFS.HDFSClient (
  Config,
  hdfsListFiles,
  hdfsReadCompleteFile,
  hdfsFileBlockLocations, hdfsFileDistribution,
  hdfsWriteNewFile
  ) where

import Control.Exception (catch, SomeException)
import Data.Bool (bool)
import qualified Data.Int as I
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

{-|
 List file names at path. The HDFS will return full qualified file names including protocol, host and port.
 For simplicity, these can be used for opening files etc without stripping, however you should not try to
 construct these in the same form (just use a simple file path), since port and hostname are local knowledge
 of the thrift server, strictly speaking.
|-}
hdfsListFiles :: Config -> Path -> IO [String]
hdfsListFiles config path = do
  putStrLn $ "looking at " ++ path
  res <- listStatus config path
  return $ map (TL.unpack . Types.fileStatus_path) res

{-|
 Read file content of path
  - throws an exception if path does not point at a regular file.
|-}
hdfsReadCompleteFile :: Config -> Path -> IO TL.Text
hdfsReadCompleteFile config path =
  forRegularFilePath config path $ \thriftPath fileSize ->
    withThriftChannelsForRead config (
      \channels -> withThriftHandleForReading channels thriftPath (
        \thriftHandle -> C.read channels thriftHandle 0 (fromIntegral fileSize) -- TODO: unchecked conversion Int64 to Int32
        ))

{-|
 Get the data locations for a file
  - throws an exception if path does not point at a regular file.
|-}
hdfsFileBlockLocations :: Config -> Path -> IO [Types.BlockLocation]
hdfsFileBlockLocations config path =
  forRegularFilePath config path $ \thriftPath fileSize ->
    withThriftChannelsForRead config (
      \channels -> C.getFileBlockLocations channels thriftPath 0 fileSize >>= return . toList)

hdfsFileDistribution :: Config -> Path -> IO [(String, Int)]
hdfsFileDistribution config path = do
  blockLocations <- hdfsFileBlockLocations config path
  return $ groupCount (concat $ map hostNames blockLocations)
    where
      hostNames :: Types.BlockLocation -> [String]
      hostNames = map TL.unpack . toList . Types.blockLocation_names

hdfsWriteNewFile :: Config -> Path -> TL.Text -> IO ()
hdfsWriteNewFile config path content =
  withThriftChannelsForWrite config (
    \channels -> withThriftHandleForWriteNew channels (toThriftPath path) (
      \thriftHandle -> C.write channels thriftHandle content >>= return . bool (error "unknown write handle") ()
      ))

-- internals

forRegularFilePath :: Config -> Path -> (Types.Pathname -> I.Int64 -> IO a) -> IO a
forRegularFilePath config path action = do
  fileStatus <- listStatus config path
  if length fileStatus /= 1
    then error $ path ++ " is not a regular file: " ++ (show fileStatus)
    else action (toThriftPath path) (filesize fileStatus)
    where
      filesize = Types.fileStatus_length . head

listStatus :: Config -> Path -> IO [Types.FileStatus]
listStatus config path = withThriftChannelsForRead config (\channels -> C.listStatus channels (toThriftPath path)) >>= return . toList

toThriftPath :: String -> Types.Pathname
toThriftPath = Types.Pathname . TL.pack

type Channels = (BinaryProtocol GHC.Handle,
                 BinaryProtocol GHC.Handle) -- TODO generalize?

withThriftChannelsForRead :: Config -> (Channels -> IO result) -> IO result
withThriftChannelsForRead = withThriftChannels T.tClose

withThriftChannelsForWrite :: Config -> (Channels -> IO result) -> IO result
withThriftChannelsForWrite = withThriftChannels T.tClose

type CloseHandleAction = (GHC.Handle -> IO ())

withThriftChannels :: CloseHandleAction -> Config -> (Channels -> IO result) -> IO result
withThriftChannels closeAction (host, port) action = do
  handle <- openHandle host port
  res <- action (BinaryProtocol handle, BinaryProtocol handle)
  closeAction handle
  return res

openHandle :: String -> Int -> IO GHC.Handle
openHandle host port = H.hOpen (host, N.PortNumber (toEnum port)) `catch` wrapException
  where
    wrapException :: SomeException -> a
    wrapException e = error $ "Cannot connect to "++host++":"++(show port)++": "++(show e)

withThriftHandleForReading :: Channels -> Types.Pathname -> (Types.ThriftHandle -> IO result) -> IO result
withThriftHandleForReading = withThriftHandle C.open C.closeReadHandle

withThriftHandleForWriteNew :: Channels -> Types.Pathname -> (Types.ThriftHandle -> IO result) -> IO result
withThriftHandleForWriteNew = withThriftHandle C.create C.closeWriteHandle

type CloseChannelAction = Channels -> Types.ThriftHandle -> IO Bool

withThriftHandle :: (Channels -> Types.Pathname -> IO Types.ThriftHandle) -> CloseChannelAction -> Channels -> Types.Pathname -> (Types.ThriftHandle -> IO result) -> IO result
withThriftHandle allocationMethod closeAction channels hdfsPath action = do
  thriftHandle <- allocationMethod channels hdfsPath
  res <- action thriftHandle
  _ <- closeAction channels thriftHandle
  return res

groupCount :: (Eq key) => [key] -> [(key, Int)]
groupCount = foldr groupCount' []
  where
    groupCount' :: (Eq key) => key -> [(key, Int)] -> [(key, Int)]
    groupCount' key [] = [(key, 1)]
    groupCount' key (next:collected) = if (fst next == key) then (fst next, snd next +1):collected else next:(groupCount' key collected)

{-
updateOrInsert :: (Eq key) => (value -> value -> value) -> [(key, value)] -> (key, value) -> [(key, value)]
updateOrInsert _ [] entry = [entry]
updateOrInsert updater (next:rest) entry =
  if (fst entry == fst next)
  then (fst next, updater (snd next) (snd entry)):rest
  else next:(updateOrInsert updater rest entry)
-}
