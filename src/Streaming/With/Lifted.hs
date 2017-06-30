{-# LANGUAGE FlexibleContexts, RankNTypes, TypeFamilies #-}

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
    -- * File-handling
  , withFile
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
import qualified Streaming.With            as W

import Control.Monad.Catch       (MonadMask, bracket)
import Control.Monad.IO.Class    (MonadIO, liftIO)
import Control.Monad.Managed     (Managed, managed)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont  (ContT(..))
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
class (MonadMask (WithMonad w), MonadIO (WithMonad w)) => Withable w where
  type WithMonad w :: * -> *

  liftWith :: (forall r. (a -> WithMonad w r) -> WithMonad w r) -> w a

  liftAction :: WithMonad w a -> w a

instance Withable Managed where
  type WithMonad Managed = IO

  liftWith = managed

  liftAction = liftIO

instance (MonadMask m, MonadIO m) => Withable (ContT r m) where
  type WithMonad (ContT r m) = m

  liftWith = ContT

  liftAction = lift

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

{- $reexports

These may assist in writing your own bracket-style functions.

Note that not everything is re-exported: for example, 'Handle' isn't
re-exported for use with 'withFile' as it's unlikely that you will
write another wrapper around it, and furthermore it wouldn't be a
common enough extension to warrant it.

-}
