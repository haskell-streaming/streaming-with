{- |
   Module      : Streaming.With
   Description : with/bracket-style idioms for use with streaming
   Copyright   : Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Streaming.With
  ( -- * File-handling
    withFile
  , withBinaryFile
    -- ** Common file-handling cases
  , writeBinaryFile
  , appendBinaryFile
  , withBinaryFileContents
    -- ** Re-exports
    -- $reexports
  , MonadMask
  , bracket
  ) where

import           Data.ByteString.Streaming (ByteString)
import qualified Data.ByteString.Streaming as B

import Control.Monad.Catch    (MonadMask, bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO              (Handle, IOMode(..), hClose, openBinaryFile,
                               openFile)

--------------------------------------------------------------------------------

-- | A lifted variant of 'System.IO.withFile'.
--
--   You almost definitely don't want to use this; instead, use
--   'withBinaryFile' in conjunction with "Data.ByteString.Streaming".
withFile :: (MonadMask m, MonadIO m) => FilePath -> IOMode -> (Handle -> m r) -> m r
withFile fp md = bracket (liftIO (openFile fp md)) (liftIO . hClose)

-- | A lifted variant of 'System.IO.withBinaryFile'.
withBinaryFile :: (MonadMask m, MonadIO m) => FilePath -> IOMode -> (Handle -> m r) -> m r
withBinaryFile fp md = bracket (liftIO (openBinaryFile fp md)) (liftIO . hClose)

-- | Write to the specified file.
writeBinaryFile :: (MonadMask m, MonadIO m) => FilePath -> ByteString m r -> m r
writeBinaryFile fp = withBinaryFile fp WriteMode . flip B.hPut

-- | Append to the specified file.
appendBinaryFile :: (MonadMask m, MonadIO m) => FilePath -> ByteString m r -> m r
appendBinaryFile fp = withBinaryFile fp AppendMode . flip B.hPut

-- | Apply a function to the contents of the file.
--
--   Note that a different monadic stack is allowed for the
--   'ByteString' input, as long as it later gets resolved to the
--   required output type (e.g. remove transformer).
withBinaryFileContents :: (MonadMask m, MonadIO m, MonadIO n) => FilePath
                          -> (ByteString n () -> m r) -> m r
withBinaryFileContents fp f = withBinaryFile fp ReadMode (f . B.hGetContents)

--------------------------------------------------------------------------------

{- $reexports

These may assist in writing your own bracket-style functions.

Note that not everything is re-exported: for example, 'Handle' isn't
re-exported for use with 'withFile' as it's unlikely that you will
write another wrapper around it, and furthermore it wouldn't be a
common enough extension to warrant it.

-}
