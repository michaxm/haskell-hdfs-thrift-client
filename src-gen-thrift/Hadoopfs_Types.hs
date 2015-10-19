{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-----------------------------------------------------------------
-- Autogenerated by Thrift Compiler (0.9.3)                      --
--                                                             --
-- DO NOT EDIT UNLESS YOU ARE SURE YOU KNOW WHAT YOU ARE DOING --
-----------------------------------------------------------------

module Hadoopfs_Types where
import Prelude (($), (.), (>>=), (==), (++))
import qualified Prelude as P
import qualified Control.Exception as X
import qualified Control.Monad as M ( liftM, ap, when )
import Data.Functor ( (<$>) )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Hashable as H
import qualified Data.Int as I
import qualified Data.Maybe as M (catMaybes)
import qualified Data.Text.Lazy.Encoding as E ( decodeUtf8, encodeUtf8 )
import qualified Data.Text.Lazy as LT
import qualified GHC.Generics as G (Generic)
import qualified Data.Typeable as TY ( Typeable )
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Vector as Vector
import qualified Test.QuickCheck.Arbitrary as QC ( Arbitrary(..) )
import qualified Test.QuickCheck as QC ( elements )

import qualified Thrift as T
import qualified Thrift.Types as T
import qualified Thrift.Arbitraries as T


data ThriftHandle = ThriftHandle  { thriftHandle_id :: I.Int64
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable ThriftHandle where
  hashWithSalt salt record = salt   `H.hashWithSalt` thriftHandle_id record  
instance QC.Arbitrary ThriftHandle where 
  arbitrary = M.liftM ThriftHandle (QC.arbitrary)
  shrink obj | obj == default_ThriftHandle = []
             | P.otherwise = M.catMaybes
    [ if obj == default_ThriftHandle{thriftHandle_id = thriftHandle_id obj} then P.Nothing else P.Just $ default_ThriftHandle{thriftHandle_id = thriftHandle_id obj}
    ]
from_ThriftHandle :: ThriftHandle -> T.ThriftVal
from_ThriftHandle record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v2 -> P.Just (-1, ("id",T.TI64 _v2))) $ thriftHandle_id record
  ]
write_ThriftHandle :: (T.Protocol p, T.Transport t) => p t -> ThriftHandle -> P.IO ()
write_ThriftHandle oprot record = T.writeVal oprot $ from_ThriftHandle record
encode_ThriftHandle :: (T.Protocol p, T.Transport t) => p t -> ThriftHandle -> LBS.ByteString
encode_ThriftHandle oprot record = T.serializeVal oprot $ from_ThriftHandle record
to_ThriftHandle :: T.ThriftVal -> ThriftHandle
to_ThriftHandle (T.TStruct fields) = ThriftHandle{
  thriftHandle_id = P.maybe (thriftHandle_id default_ThriftHandle) (\(_,_val4) -> (case _val4 of {T.TI64 _val5 -> _val5; _ -> P.error "wrong type"})) (Map.lookup (-1) fields)
  }
to_ThriftHandle _ = P.error "not a struct"
read_ThriftHandle :: (T.Transport t, T.Protocol p) => p t -> P.IO ThriftHandle
read_ThriftHandle iprot = to_ThriftHandle <$> T.readVal iprot (T.T_STRUCT typemap_ThriftHandle)
decode_ThriftHandle :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> ThriftHandle
decode_ThriftHandle iprot bs = to_ThriftHandle $ T.deserializeVal iprot (T.T_STRUCT typemap_ThriftHandle) bs
typemap_ThriftHandle :: T.TypeMap
typemap_ThriftHandle = Map.fromList [(-1,("id",T.T_I64))]
default_ThriftHandle :: ThriftHandle
default_ThriftHandle = ThriftHandle{
  thriftHandle_id = 0}
data Pathname = Pathname  { pathname_pathname :: LT.Text
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable Pathname where
  hashWithSalt salt record = salt   `H.hashWithSalt` pathname_pathname record  
instance QC.Arbitrary Pathname where 
  arbitrary = M.liftM Pathname (QC.arbitrary)
  shrink obj | obj == default_Pathname = []
             | P.otherwise = M.catMaybes
    [ if obj == default_Pathname{pathname_pathname = pathname_pathname obj} then P.Nothing else P.Just $ default_Pathname{pathname_pathname = pathname_pathname obj}
    ]
from_Pathname :: Pathname -> T.ThriftVal
from_Pathname record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v8 -> P.Just (-1, ("pathname",T.TString $ E.encodeUtf8 _v8))) $ pathname_pathname record
  ]
write_Pathname :: (T.Protocol p, T.Transport t) => p t -> Pathname -> P.IO ()
write_Pathname oprot record = T.writeVal oprot $ from_Pathname record
encode_Pathname :: (T.Protocol p, T.Transport t) => p t -> Pathname -> LBS.ByteString
encode_Pathname oprot record = T.serializeVal oprot $ from_Pathname record
to_Pathname :: T.ThriftVal -> Pathname
to_Pathname (T.TStruct fields) = Pathname{
  pathname_pathname = P.maybe (pathname_pathname default_Pathname) (\(_,_val10) -> (case _val10 of {T.TString _val11 -> E.decodeUtf8 _val11; _ -> P.error "wrong type"})) (Map.lookup (-1) fields)
  }
to_Pathname _ = P.error "not a struct"
read_Pathname :: (T.Transport t, T.Protocol p) => p t -> P.IO Pathname
read_Pathname iprot = to_Pathname <$> T.readVal iprot (T.T_STRUCT typemap_Pathname)
decode_Pathname :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> Pathname
decode_Pathname iprot bs = to_Pathname $ T.deserializeVal iprot (T.T_STRUCT typemap_Pathname) bs
typemap_Pathname :: T.TypeMap
typemap_Pathname = Map.fromList [(-1,("pathname",T.T_STRING))]
default_Pathname :: Pathname
default_Pathname = Pathname{
  pathname_pathname = ""}
data FileStatus = FileStatus  { fileStatus_path :: LT.Text
  , fileStatus_length :: I.Int64
  , fileStatus_isdir :: P.Bool
  , fileStatus_block_replication :: I.Int16
  , fileStatus_blocksize :: I.Int64
  , fileStatus_modification_time :: I.Int64
  , fileStatus_permission :: LT.Text
  , fileStatus_owner :: LT.Text
  , fileStatus_group :: LT.Text
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable FileStatus where
  hashWithSalt salt record = salt   `H.hashWithSalt` fileStatus_path record   `H.hashWithSalt` fileStatus_length record   `H.hashWithSalt` fileStatus_isdir record   `H.hashWithSalt` fileStatus_block_replication record   `H.hashWithSalt` fileStatus_blocksize record   `H.hashWithSalt` fileStatus_modification_time record   `H.hashWithSalt` fileStatus_permission record   `H.hashWithSalt` fileStatus_owner record   `H.hashWithSalt` fileStatus_group record  
instance QC.Arbitrary FileStatus where 
  arbitrary = M.liftM FileStatus (QC.arbitrary)
          `M.ap`(QC.arbitrary)
          `M.ap`(QC.arbitrary)
          `M.ap`(QC.arbitrary)
          `M.ap`(QC.arbitrary)
          `M.ap`(QC.arbitrary)
          `M.ap`(QC.arbitrary)
          `M.ap`(QC.arbitrary)
          `M.ap`(QC.arbitrary)
  shrink obj | obj == default_FileStatus = []
             | P.otherwise = M.catMaybes
    [ if obj == default_FileStatus{fileStatus_path = fileStatus_path obj} then P.Nothing else P.Just $ default_FileStatus{fileStatus_path = fileStatus_path obj}
    , if obj == default_FileStatus{fileStatus_length = fileStatus_length obj} then P.Nothing else P.Just $ default_FileStatus{fileStatus_length = fileStatus_length obj}
    , if obj == default_FileStatus{fileStatus_isdir = fileStatus_isdir obj} then P.Nothing else P.Just $ default_FileStatus{fileStatus_isdir = fileStatus_isdir obj}
    , if obj == default_FileStatus{fileStatus_block_replication = fileStatus_block_replication obj} then P.Nothing else P.Just $ default_FileStatus{fileStatus_block_replication = fileStatus_block_replication obj}
    , if obj == default_FileStatus{fileStatus_blocksize = fileStatus_blocksize obj} then P.Nothing else P.Just $ default_FileStatus{fileStatus_blocksize = fileStatus_blocksize obj}
    , if obj == default_FileStatus{fileStatus_modification_time = fileStatus_modification_time obj} then P.Nothing else P.Just $ default_FileStatus{fileStatus_modification_time = fileStatus_modification_time obj}
    , if obj == default_FileStatus{fileStatus_permission = fileStatus_permission obj} then P.Nothing else P.Just $ default_FileStatus{fileStatus_permission = fileStatus_permission obj}
    , if obj == default_FileStatus{fileStatus_owner = fileStatus_owner obj} then P.Nothing else P.Just $ default_FileStatus{fileStatus_owner = fileStatus_owner obj}
    , if obj == default_FileStatus{fileStatus_group = fileStatus_group obj} then P.Nothing else P.Just $ default_FileStatus{fileStatus_group = fileStatus_group obj}
    ]
from_FileStatus :: FileStatus -> T.ThriftVal
from_FileStatus record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v14 -> P.Just (1, ("path",T.TString $ E.encodeUtf8 _v14))) $ fileStatus_path record
  , (\_v14 -> P.Just (2, ("length",T.TI64 _v14))) $ fileStatus_length record
  , (\_v14 -> P.Just (3, ("isdir",T.TBool _v14))) $ fileStatus_isdir record
  , (\_v14 -> P.Just (4, ("block_replication",T.TI16 _v14))) $ fileStatus_block_replication record
  , (\_v14 -> P.Just (5, ("blocksize",T.TI64 _v14))) $ fileStatus_blocksize record
  , (\_v14 -> P.Just (6, ("modification_time",T.TI64 _v14))) $ fileStatus_modification_time record
  , (\_v14 -> P.Just (7, ("permission",T.TString $ E.encodeUtf8 _v14))) $ fileStatus_permission record
  , (\_v14 -> P.Just (8, ("owner",T.TString $ E.encodeUtf8 _v14))) $ fileStatus_owner record
  , (\_v14 -> P.Just (9, ("group",T.TString $ E.encodeUtf8 _v14))) $ fileStatus_group record
  ]
write_FileStatus :: (T.Protocol p, T.Transport t) => p t -> FileStatus -> P.IO ()
write_FileStatus oprot record = T.writeVal oprot $ from_FileStatus record
encode_FileStatus :: (T.Protocol p, T.Transport t) => p t -> FileStatus -> LBS.ByteString
encode_FileStatus oprot record = T.serializeVal oprot $ from_FileStatus record
to_FileStatus :: T.ThriftVal -> FileStatus
to_FileStatus (T.TStruct fields) = FileStatus{
  fileStatus_path = P.maybe (fileStatus_path default_FileStatus) (\(_,_val16) -> (case _val16 of {T.TString _val17 -> E.decodeUtf8 _val17; _ -> P.error "wrong type"})) (Map.lookup (1) fields),
  fileStatus_length = P.maybe (fileStatus_length default_FileStatus) (\(_,_val16) -> (case _val16 of {T.TI64 _val18 -> _val18; _ -> P.error "wrong type"})) (Map.lookup (2) fields),
  fileStatus_isdir = P.maybe (fileStatus_isdir default_FileStatus) (\(_,_val16) -> (case _val16 of {T.TBool _val19 -> _val19; _ -> P.error "wrong type"})) (Map.lookup (3) fields),
  fileStatus_block_replication = P.maybe (fileStatus_block_replication default_FileStatus) (\(_,_val16) -> (case _val16 of {T.TI16 _val20 -> _val20; _ -> P.error "wrong type"})) (Map.lookup (4) fields),
  fileStatus_blocksize = P.maybe (fileStatus_blocksize default_FileStatus) (\(_,_val16) -> (case _val16 of {T.TI64 _val21 -> _val21; _ -> P.error "wrong type"})) (Map.lookup (5) fields),
  fileStatus_modification_time = P.maybe (fileStatus_modification_time default_FileStatus) (\(_,_val16) -> (case _val16 of {T.TI64 _val22 -> _val22; _ -> P.error "wrong type"})) (Map.lookup (6) fields),
  fileStatus_permission = P.maybe (fileStatus_permission default_FileStatus) (\(_,_val16) -> (case _val16 of {T.TString _val23 -> E.decodeUtf8 _val23; _ -> P.error "wrong type"})) (Map.lookup (7) fields),
  fileStatus_owner = P.maybe (fileStatus_owner default_FileStatus) (\(_,_val16) -> (case _val16 of {T.TString _val24 -> E.decodeUtf8 _val24; _ -> P.error "wrong type"})) (Map.lookup (8) fields),
  fileStatus_group = P.maybe (fileStatus_group default_FileStatus) (\(_,_val16) -> (case _val16 of {T.TString _val25 -> E.decodeUtf8 _val25; _ -> P.error "wrong type"})) (Map.lookup (9) fields)
  }
to_FileStatus _ = P.error "not a struct"
read_FileStatus :: (T.Transport t, T.Protocol p) => p t -> P.IO FileStatus
read_FileStatus iprot = to_FileStatus <$> T.readVal iprot (T.T_STRUCT typemap_FileStatus)
decode_FileStatus :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> FileStatus
decode_FileStatus iprot bs = to_FileStatus $ T.deserializeVal iprot (T.T_STRUCT typemap_FileStatus) bs
typemap_FileStatus :: T.TypeMap
typemap_FileStatus = Map.fromList [(1,("path",T.T_STRING)),(2,("length",T.T_I64)),(3,("isdir",T.T_BOOL)),(4,("block_replication",T.T_I16)),(5,("blocksize",T.T_I64)),(6,("modification_time",T.T_I64)),(7,("permission",T.T_STRING)),(8,("owner",T.T_STRING)),(9,("group",T.T_STRING))]
default_FileStatus :: FileStatus
default_FileStatus = FileStatus{
  fileStatus_path = "",
  fileStatus_length = 0,
  fileStatus_isdir = P.False,
  fileStatus_block_replication = 0,
  fileStatus_blocksize = 0,
  fileStatus_modification_time = 0,
  fileStatus_permission = "",
  fileStatus_owner = "",
  fileStatus_group = ""}
data BlockLocation = BlockLocation  { blockLocation_hosts :: (Vector.Vector LT.Text)
  , blockLocation_names :: (Vector.Vector LT.Text)
  , blockLocation_offset :: I.Int64
  , blockLocation_length :: I.Int64
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable BlockLocation where
  hashWithSalt salt record = salt   `H.hashWithSalt` blockLocation_hosts record   `H.hashWithSalt` blockLocation_names record   `H.hashWithSalt` blockLocation_offset record   `H.hashWithSalt` blockLocation_length record  
instance QC.Arbitrary BlockLocation where 
  arbitrary = M.liftM BlockLocation (QC.arbitrary)
          `M.ap`(QC.arbitrary)
          `M.ap`(QC.arbitrary)
          `M.ap`(QC.arbitrary)
  shrink obj | obj == default_BlockLocation = []
             | P.otherwise = M.catMaybes
    [ if obj == default_BlockLocation{blockLocation_hosts = blockLocation_hosts obj} then P.Nothing else P.Just $ default_BlockLocation{blockLocation_hosts = blockLocation_hosts obj}
    , if obj == default_BlockLocation{blockLocation_names = blockLocation_names obj} then P.Nothing else P.Just $ default_BlockLocation{blockLocation_names = blockLocation_names obj}
    , if obj == default_BlockLocation{blockLocation_offset = blockLocation_offset obj} then P.Nothing else P.Just $ default_BlockLocation{blockLocation_offset = blockLocation_offset obj}
    , if obj == default_BlockLocation{blockLocation_length = blockLocation_length obj} then P.Nothing else P.Just $ default_BlockLocation{blockLocation_length = blockLocation_length obj}
    ]
from_BlockLocation :: BlockLocation -> T.ThriftVal
from_BlockLocation record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v28 -> P.Just (1, ("hosts",T.TList T.T_STRING $ P.map (\_v30 -> T.TString $ E.encodeUtf8 _v30) $ Vector.toList _v28))) $ blockLocation_hosts record
  , (\_v28 -> P.Just (2, ("names",T.TList T.T_STRING $ P.map (\_v32 -> T.TString $ E.encodeUtf8 _v32) $ Vector.toList _v28))) $ blockLocation_names record
  , (\_v28 -> P.Just (3, ("offset",T.TI64 _v28))) $ blockLocation_offset record
  , (\_v28 -> P.Just (4, ("length",T.TI64 _v28))) $ blockLocation_length record
  ]
write_BlockLocation :: (T.Protocol p, T.Transport t) => p t -> BlockLocation -> P.IO ()
write_BlockLocation oprot record = T.writeVal oprot $ from_BlockLocation record
encode_BlockLocation :: (T.Protocol p, T.Transport t) => p t -> BlockLocation -> LBS.ByteString
encode_BlockLocation oprot record = T.serializeVal oprot $ from_BlockLocation record
to_BlockLocation :: T.ThriftVal -> BlockLocation
to_BlockLocation (T.TStruct fields) = BlockLocation{
  blockLocation_hosts = P.maybe (blockLocation_hosts default_BlockLocation) (\(_,_val34) -> (case _val34 of {T.TList _ _val35 -> (Vector.fromList $ P.map (\_v36 -> (case _v36 of {T.TString _val37 -> E.decodeUtf8 _val37; _ -> P.error "wrong type"})) _val35); _ -> P.error "wrong type"})) (Map.lookup (1) fields),
  blockLocation_names = P.maybe (blockLocation_names default_BlockLocation) (\(_,_val34) -> (case _val34 of {T.TList _ _val38 -> (Vector.fromList $ P.map (\_v39 -> (case _v39 of {T.TString _val40 -> E.decodeUtf8 _val40; _ -> P.error "wrong type"})) _val38); _ -> P.error "wrong type"})) (Map.lookup (2) fields),
  blockLocation_offset = P.maybe (blockLocation_offset default_BlockLocation) (\(_,_val34) -> (case _val34 of {T.TI64 _val41 -> _val41; _ -> P.error "wrong type"})) (Map.lookup (3) fields),
  blockLocation_length = P.maybe (blockLocation_length default_BlockLocation) (\(_,_val34) -> (case _val34 of {T.TI64 _val42 -> _val42; _ -> P.error "wrong type"})) (Map.lookup (4) fields)
  }
to_BlockLocation _ = P.error "not a struct"
read_BlockLocation :: (T.Transport t, T.Protocol p) => p t -> P.IO BlockLocation
read_BlockLocation iprot = to_BlockLocation <$> T.readVal iprot (T.T_STRUCT typemap_BlockLocation)
decode_BlockLocation :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> BlockLocation
decode_BlockLocation iprot bs = to_BlockLocation $ T.deserializeVal iprot (T.T_STRUCT typemap_BlockLocation) bs
typemap_BlockLocation :: T.TypeMap
typemap_BlockLocation = Map.fromList [(1,("hosts",(T.T_LIST T.T_STRING))),(2,("names",(T.T_LIST T.T_STRING))),(3,("offset",T.T_I64)),(4,("length",T.T_I64))]
default_BlockLocation :: BlockLocation
default_BlockLocation = BlockLocation{
  blockLocation_hosts = Vector.empty,
  blockLocation_names = Vector.empty,
  blockLocation_offset = 0,
  blockLocation_length = 0}
data MalformedInputException = MalformedInputException  { malformedInputException_message :: LT.Text
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance X.Exception MalformedInputException
instance H.Hashable MalformedInputException where
  hashWithSalt salt record = salt   `H.hashWithSalt` malformedInputException_message record  
instance QC.Arbitrary MalformedInputException where 
  arbitrary = M.liftM MalformedInputException (QC.arbitrary)
  shrink obj | obj == default_MalformedInputException = []
             | P.otherwise = M.catMaybes
    [ if obj == default_MalformedInputException{malformedInputException_message = malformedInputException_message obj} then P.Nothing else P.Just $ default_MalformedInputException{malformedInputException_message = malformedInputException_message obj}
    ]
from_MalformedInputException :: MalformedInputException -> T.ThriftVal
from_MalformedInputException record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v45 -> P.Just (-1, ("message",T.TString $ E.encodeUtf8 _v45))) $ malformedInputException_message record
  ]
write_MalformedInputException :: (T.Protocol p, T.Transport t) => p t -> MalformedInputException -> P.IO ()
write_MalformedInputException oprot record = T.writeVal oprot $ from_MalformedInputException record
encode_MalformedInputException :: (T.Protocol p, T.Transport t) => p t -> MalformedInputException -> LBS.ByteString
encode_MalformedInputException oprot record = T.serializeVal oprot $ from_MalformedInputException record
to_MalformedInputException :: T.ThriftVal -> MalformedInputException
to_MalformedInputException (T.TStruct fields) = MalformedInputException{
  malformedInputException_message = P.maybe (malformedInputException_message default_MalformedInputException) (\(_,_val47) -> (case _val47 of {T.TString _val48 -> E.decodeUtf8 _val48; _ -> P.error "wrong type"})) (Map.lookup (-1) fields)
  }
to_MalformedInputException _ = P.error "not a struct"
read_MalformedInputException :: (T.Transport t, T.Protocol p) => p t -> P.IO MalformedInputException
read_MalformedInputException iprot = to_MalformedInputException <$> T.readVal iprot (T.T_STRUCT typemap_MalformedInputException)
decode_MalformedInputException :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> MalformedInputException
decode_MalformedInputException iprot bs = to_MalformedInputException $ T.deserializeVal iprot (T.T_STRUCT typemap_MalformedInputException) bs
typemap_MalformedInputException :: T.TypeMap
typemap_MalformedInputException = Map.fromList [(-1,("message",T.T_STRING))]
default_MalformedInputException :: MalformedInputException
default_MalformedInputException = MalformedInputException{
  malformedInputException_message = ""}
data ThriftIOException = ThriftIOException  { thriftIOException_message :: LT.Text
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance X.Exception ThriftIOException
instance H.Hashable ThriftIOException where
  hashWithSalt salt record = salt   `H.hashWithSalt` thriftIOException_message record  
instance QC.Arbitrary ThriftIOException where 
  arbitrary = M.liftM ThriftIOException (QC.arbitrary)
  shrink obj | obj == default_ThriftIOException = []
             | P.otherwise = M.catMaybes
    [ if obj == default_ThriftIOException{thriftIOException_message = thriftIOException_message obj} then P.Nothing else P.Just $ default_ThriftIOException{thriftIOException_message = thriftIOException_message obj}
    ]
from_ThriftIOException :: ThriftIOException -> T.ThriftVal
from_ThriftIOException record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v51 -> P.Just (-1, ("message",T.TString $ E.encodeUtf8 _v51))) $ thriftIOException_message record
  ]
write_ThriftIOException :: (T.Protocol p, T.Transport t) => p t -> ThriftIOException -> P.IO ()
write_ThriftIOException oprot record = T.writeVal oprot $ from_ThriftIOException record
encode_ThriftIOException :: (T.Protocol p, T.Transport t) => p t -> ThriftIOException -> LBS.ByteString
encode_ThriftIOException oprot record = T.serializeVal oprot $ from_ThriftIOException record
to_ThriftIOException :: T.ThriftVal -> ThriftIOException
to_ThriftIOException (T.TStruct fields) = ThriftIOException{
  thriftIOException_message = P.maybe (thriftIOException_message default_ThriftIOException) (\(_,_val53) -> (case _val53 of {T.TString _val54 -> E.decodeUtf8 _val54; _ -> P.error "wrong type"})) (Map.lookup (-1) fields)
  }
to_ThriftIOException _ = P.error "not a struct"
read_ThriftIOException :: (T.Transport t, T.Protocol p) => p t -> P.IO ThriftIOException
read_ThriftIOException iprot = to_ThriftIOException <$> T.readVal iprot (T.T_STRUCT typemap_ThriftIOException)
decode_ThriftIOException :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> ThriftIOException
decode_ThriftIOException iprot bs = to_ThriftIOException $ T.deserializeVal iprot (T.T_STRUCT typemap_ThriftIOException) bs
typemap_ThriftIOException :: T.TypeMap
typemap_ThriftIOException = Map.fromList [(-1,("message",T.T_STRING))]
default_ThriftIOException :: ThriftIOException
default_ThriftIOException = ThriftIOException{
  thriftIOException_message = ""}