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

module ThriftHadoopFileSystem_Iface where
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


import Hadoopfs_Types

class ThriftHadoopFileSystem_Iface a where
  setInactivityTimeoutPeriod :: a -> I.Int64 -> P.IO ()
  shutdown :: a -> I.Int32 -> P.IO ()
  create :: a -> Pathname -> P.IO ThriftHandle
  createFile :: a -> Pathname -> I.Int16 -> P.Bool -> I.Int32 -> I.Int16 -> I.Int64 -> P.IO ThriftHandle
  open :: a -> Pathname -> P.IO ThriftHandle
  append :: a -> Pathname -> P.IO ThriftHandle
  write :: a -> ThriftHandle -> LT.Text -> P.IO P.Bool
  read :: a -> ThriftHandle -> I.Int64 -> I.Int32 -> P.IO LT.Text
  closeReadHandle :: a -> ThriftHandle -> P.IO P.Bool
  closeWriteHandle :: a -> ThriftHandle -> P.IO P.Bool
  rm :: a -> Pathname -> P.Bool -> P.IO P.Bool
  rename :: a -> Pathname -> Pathname -> P.IO P.Bool
  mkdirs :: a -> Pathname -> P.IO P.Bool
  exists :: a -> Pathname -> P.IO P.Bool
  stat :: a -> Pathname -> P.IO FileStatus
  listStatus :: a -> Pathname -> P.IO (Vector.Vector FileStatus)
  chmod :: a -> Pathname -> I.Int16 -> P.IO ()
  chown :: a -> Pathname -> LT.Text -> LT.Text -> P.IO ()
  setReplication :: a -> Pathname -> I.Int16 -> P.IO ()
  getFileBlockLocations :: a -> Pathname -> I.Int64 -> I.Int64 -> P.IO (Vector.Vector BlockLocation)
