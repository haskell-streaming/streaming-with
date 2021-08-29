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
    -- ** Temporary files
  , withSystemTempFile
  , withTempFile
    -- *** Re-exports
    -- $tempreexports
  , withSystemTempDirectory
  , withTempDirectory
    -- * Re-exports
    -- $reexports
  , MonadMask
  , bracket
  ) where

import           Streaming.ByteString   (ByteStream)
import qualified Streaming.ByteString   as B

import           Control.Monad.Catch    (MonadMask, bracket)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           System.IO              (Handle, IOMode (..), hClose,
                                         openBinaryFile, openFile)
import           System.IO.Temp         (withSystemTempDirectory,
                                         withTempDirectory)
import qualified System.IO.Temp         as T

--------------------------------------------------------------------------------

-- | A lifted variant of 'System.IO.withFile'.
--
--   You almost definitely don't want to use this; instead, use
--   'withBinaryFile' in conjunction with "Streaming.ByteString".
withFile :: (MonadMask m, MonadIO m) => FilePath -> IOMode -> (Handle -> m r) -> m r
withFile fp md = bracket (liftIO (openFile fp md)) (liftIO . hClose)

-- | A lifted variant of 'System.IO.withBinaryFile'.
withBinaryFile :: (MonadMask m, MonadIO m) => FilePath -> IOMode -> (Handle -> m r) -> m r
withBinaryFile fp md = bracket (liftIO (openBinaryFile fp md)) (liftIO . hClose)

-- | Write to the specified file.
writeBinaryFile :: (MonadMask m, MonadIO m) => FilePath -> ByteStream m r -> m r
writeBinaryFile fp = withBinaryFile fp WriteMode . flip B.hPut

-- | Append to the specified file.
appendBinaryFile :: (MonadMask m, MonadIO m) => FilePath -> ByteStream m r -> m r
appendBinaryFile fp = withBinaryFile fp AppendMode . flip B.hPut

-- | Apply a function to the contents of the file.
--
--   Note that a different monadic stack is allowed for the
--   'ByteStream' input, as long as it later gets resolved to the
--   required output type (e.g. remove transformer).
withBinaryFileContents :: (MonadMask m, MonadIO m, MonadIO n) => FilePath
                          -> (ByteStream n () -> m r) -> m r
withBinaryFileContents fp f = withBinaryFile fp ReadMode (f . B.hGetContents)

--------------------------------------------------------------------------------

-- | /This is 'T.withSystemTempFile' from the @temporary@ package/
--   /with the continuation re-structured to only take one argument./
--
--   Create and use a temporary file in the system standard temporary
--   directory.
--
--   Behaves exactly the same as 'withTempFile', except that the
--   parent temporary directory will be that returned by
--   'T.getCanonicalTemporaryDirectory'.
--
--   @since 0.1.1.0
withSystemTempFile :: (MonadIO m, MonadMask m)
                   => String -- ^ File name template.  See 'T.openTempFile'
                   -> ((FilePath, Handle) -> m r)
                   -> m r
withSystemTempFile template = T.withSystemTempFile template . curry

-- | /This is 'T.withTempFile' from the @temporary@ package with the/
--   /continuation re-structured to only take one argument./
--
--   Use a temporary filename that doesn't already exist.
--
--   Creates a new temporary file inside the given directory, making
--   use of the template. The temp file is deleted after use. For
--   example:
--
--   > withTempFile "src" "sdist." $ \(tmpFile, hFile) -> ...
--
--   The @tmpFile@ will be file in the given directory, e.g.
--   @src/sdist.342@.
--
--   @since 0.1.1.0
withTempFile :: (MonadIO m, MonadMask m)
             => FilePath -- ^ Temp dir to create the file in
             -> String   -- ^ File name template.  See
                         --   'T.openTempFile'.
             -> ((FilePath, Handle) -> m r)
             -> m r
withTempFile dir template = T.withTempFile dir template . curry

{- $tempreexports

These functions are re-exported from the
<http://hackage.haskell.org/package/temporary temporary> package as-is
as their structure already matches those found here.

@since 0.1.1.0

-}

--------------------------------------------------------------------------------

{- $reexports

These may assist in writing your own bracket-style functions.

Note that not everything is re-exported: for example, 'Handle' isn't
re-exported for use with 'withFile' as it's unlikely that you will
write another wrapper around it, and furthermore it wouldn't be a
common enough extension to warrant it.

-}
