{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes, TypeFamilies #-}

{- |
   Module      : Streaming.With.Lifted
   Description : Lifted with/bracket-style idioms for use with streaming
   Copyright   : Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com

   Both the 'ContT' and 'Managed' (which is a specialised variant of
   'ContT') monads can help with writing heavily nested bracketed
   code, by being able to pass around the argument to each continuation.

   This module - through the use of the 'Withable' class - provides
   lifted variants of "Streaming.With" to be able to automatically use
   these functions for resource management in your choice of monad.

   Note that you still need to use the specific monad's running
   function, as it is not possible to encapsulate those in a generic
   fashion (unless we wanted to constrain the 'ContT' instance to
   @ContT ()@).

   To ensure resources don't leak out, it is preferred that if using
   'ContT', you keep the final result type to @()@ (which is what
   'Managed' recommends with its 'runManaged' function).

   As an example using 'Managed', this function will copy the contents
   of two files into a third.:

   > copyBoth :: FilePath -> FilePath -> FilePath -> IO ()
   > copyBoth inF1 inF2 outF = runManaged $ do
   >   bs1 <- withBinaryFileContents inF1
   >   bs2 <- withBinaryFileContents inF2
   >   writeBinaryFile outF bs1
   >   appendBinaryFile outF bs2

 -}
module Streaming.With.Lifted
  ( Withable (..)
  , RunWithable (..)
  , within
  , liftActionIO
  , liftThrow
    -- * File-handling
  , withFile
  , withBinaryFile
    -- ** Common file-handling cases
  , writeBinaryFile
  , appendBinaryFile
  , withBinaryFileContents
    -- ** Temporary files
  , withSystemTempFile
  , withTempFile
  , withSystemTempDirectory
  , withTempDirectory
    -- * Re-exports
    -- $reexports
  , MonadMask
  , bracket
  ) where

import           Data.ByteString.Streaming (ByteString)
import qualified Streaming.With            as W

import Control.Exception         (Exception)
import Control.Monad.Catch       (MonadMask, bracket, throwM)
import Control.Monad.IO.Class    (MonadIO, liftIO)
import Control.Monad.Managed     (Managed, managed, runManaged)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont  (ContT(..), runContT)
import System.IO                 (Handle, IOMode)

--------------------------------------------------------------------------------

-- | How to automatically lift bracket-style expressions into a monad.
--
--   The constraints are common ones found throughout this module, and
--   as such incorporated into this class to avoid repetition in all
--   the type signatures.
--
--   It is highly recommended that you do /not/ try and layer extra
--   transformers on top of this; the intent of this class is just to
--   make writing all the underlying continuations in a nicer fashion
--   without explicit nesting, rather than as the basis of lower-level
--   code.
class (Monad w, MonadMask (WithMonad w), MonadIO (WithMonad w)) => Withable w where
  type WithMonad w :: * -> *

  liftWith :: (forall r. (a -> WithMonad w r) -> WithMonad w r) -> w a

  liftAction :: WithMonad w a -> w a

instance Withable Managed where
  type WithMonad Managed = IO

  liftWith = managed

  liftAction = liftIO

instance (MonadMask m, MonadIO m) => Withable (ContT r m) where
  type WithMonad (ContT r m) = m

  liftWith a = ContT a

  liftAction = lift

-- | Safely run the provided continuation.
--
--   A result of type '()' is required to ensure no resources are
--   leaked.
--
--   Note that you cannot write something like:
--
--   > copyBoth :: FilePath -> FilePath -> FilePath -> IO ()
--   > copyBoth inF1 inF2 outF = runWith $ do
--   >   bs1 <- withBinaryFileContents inF1
--   >   bs2 <- withBinaryFileContents inF2
--   >   writeBinaryFile outF bs1
--   >   appendBinaryFile outF bs2
--
--   as the 'RunWithable' instance cannot be inferred.  As such, you
--   will need to specify a type somewhere.
--
--   @since 0.2.1.0
class (Withable w) => RunWithable w where
  runWith :: w () -> WithMonad w ()

instance RunWithable Managed where
  runWith = runManaged

instance (MonadMask m, MonadIO m) => RunWithable (ContT () m) where
  runWith = flip runContT return

-- | A helper function to run a computation within a lifted resource
--   management expression.
--
--   @within w f = w >>= liftAction . f@
--
--   @since 0.2.1.0
within :: (Withable w) => w a -> (a -> WithMonad w b) -> w b
within w f = w >>= liftAction . f

-- | A helper function for the common case of lifting an @IO@
--   computation into a @Withable@.
--
--   @liftActionIO = liftAction . liftIO@.
--
--   @since 0.2.1.0
liftActionIO :: (Withable w) => IO a -> w a
liftActionIO = liftAction . liftIO

-- | A helper function for the common case of throwing an exception in
--   the underlying monad.
--
--   @liftThrow = liftAction . throwM@.
--
--   @since 0.2.2.0
liftThrow :: (Withable w, Exception e) => e -> w a
liftThrow = liftAction . throwM

--------------------------------------------------------------------------------

-- | A lifted variant of 'System.IO.withFile'.
--
--   You almost definitely don't want to use this; instead, use
--   'withBinaryFile' in conjunction with "Data.ByteString.Streaming".
withFile :: (Withable w) => FilePath -> IOMode -> w Handle
withFile fp md = liftWith (W.withFile fp md)

-- | A lifted variant of 'System.IO.withBinaryFile'.
withBinaryFile :: (Withable w) => FilePath -> IOMode -> w Handle
withBinaryFile fp md = liftWith (W.withBinaryFile fp md)

-- | Write to the specified file.
writeBinaryFile :: (Withable w) => FilePath -> ByteString (WithMonad w) r -> w r
writeBinaryFile fp inp = liftAction (W.writeBinaryFile fp inp)

-- | Append to the specified file.
appendBinaryFile :: (Withable w) => FilePath -> ByteString (WithMonad w) r -> w r
appendBinaryFile fp inp = liftAction (W.appendBinaryFile fp inp)

-- | Apply a function to the contents of the file.
--
--   Note that a different monadic stack is allowed for the
--   'ByteString' input, as long as it later gets resolved to the
--   required output type (e.g. remove transformer).
withBinaryFileContents :: (Withable w, MonadIO n) => FilePath -> w (ByteString n ())
withBinaryFileContents fp = liftWith (W.withBinaryFileContents fp)

--------------------------------------------------------------------------------

-- | Create and use a temporary file in the system standard temporary
--   directory.
--
--   Behaves exactly the same as 'withTempFile', except that the
--   parent temporary directory will be that returned by
--   'System.IO.Temp.getCanonicalTemporaryDirectory'.
--
--   @since 0.1.1.0
withSystemTempFile :: (Withable w)
                      => String -- ^ File name template.  See
                                --   'System.IO.Temp.openTempFile'.
                      -> w (FilePath, Handle)
withSystemTempFile template = liftWith (W.withSystemTempFile template)

-- | Use a temporary filename that doesn't already exist.
--
--   Creates a new temporary file inside the given directory, making
--   use of the template. The temp file is deleted after use. For
--   example:
--
--   > withTempFile "src" "sdist." >>= \(tmpFile, hFile) -> ...
--
--   The @tmpFile@ will be file in the given directory, e.g.
--   @src/sdist.342@.
--
--   @since 0.1.1.0
withTempFile :: (Withable w)
             => FilePath -- ^ Temp dir to create the file in
             -> String   -- ^ File name template.  See
                         --   'System.IO.Temp.openTempFile'.
             -> w (FilePath, Handle)
withTempFile dir template = liftWith (W.withTempFile dir template)

-- | Create and use a temporary directory in the system standard
--   temporary directory.
--
--   Behaves exactly the same as 'withTempDirectory', except that the
--   parent temporary directory will be that returned by
--   'System.IO.Temp.getCanonicalTemporaryDirectory'.
--
--   @since 0.1.1.0
withSystemTempDirectory :: (Withable w)
                           => String -- ^ Directory name template. See
                                     --   'System.IO.Temp.openTempFile'.
                           -> w FilePath
withSystemTempDirectory template = liftWith (W.withSystemTempDirectory template)

-- | Create and use a temporary directory.
--
--   Creates a new temporary directory inside the given directory,
--   making use of the template. The temp directory is deleted after
--   use. For example:
--
--   > withTempDirectory "src" "sdist." >>= \tmpDir -> ...
--
--   The @tmpDir@ will be a new subdirectory of the given directory, e.g.
--   @src/sdist.342@.
--
--   @since 0.1.1.0
withTempDirectory :: (Withable w)
                     => FilePath -- ^ Temp directory to create the
                                 --   directory in
                     -> String   -- ^ Directory name template. See
                                 --   'System.IO.Temp.openTempFile'.
                     -> w FilePath
withTempDirectory dir template = liftWith (W.withTempDirectory dir template)

--------------------------------------------------------------------------------

{- $reexports

These may assist in writing your own bracket-style functions.

Note that not everything is re-exported: for example, 'Handle' isn't
re-exported for use with 'withFile' as it's unlikely that you will
write another wrapper around it, and furthermore it wouldn't be a
common enough extension to warrant it.

-}
