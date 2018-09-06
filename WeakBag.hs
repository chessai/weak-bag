-- Copyright (c) 2014, Ryan Trinkle
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
-- * Redistributions of source code must retain the above copyright notice, this
--   list of conditions and the following disclaimer.
-- 
-- * Redistributions in binary form must reproduce the above copyright notice,
--   this list of conditions and the following disclaimer in the documentation
--   and/or other materials provided with the distribution.
-- 
-- * Neither the name of the {organization} nor the names of its
--   contributors may be used to endorse or promote products derived from
--   this software without specific prior written permission.
--
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
--   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
--   FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
--   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
--   SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
--   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
--   OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module defines the 'WeakBag' type, which represents a mutable
--   collection of items that does not cause the items to be retained in memory.
--   It is useful for situations where a value needs to be inspected or modified
--   if it is still alive, but can be ignored if it is dead.
module Data.WeakBag
  ( WeakBag
  , WeakBagTicket
  , insert
  , empty
  , isEmpty
  , singleton
  , traverse
  , remove
  ) where

import Prelude hiding (traverse)

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Control.Exception (evaluate)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import System.Mem.Weak (Weak, mkWeakPtr, finalize, deRefWeak)

-- | A @WeakBag a@ holds a set of values of type @a@, but does not retain them -
--   that is, they can still be garbage-collected.  As long as the @a@s remain
--   alive, the 'WeakBag' will continue to refer to them.
data WeakBag a = WeakBag
  {-# UNPACK #-} !(IORef Int)
  {-# UNPACK #-} !(IORef (IntMap (Weak a)))

-- | When inserting an item into a 'WeakBag', a 'WeakBagTicket' is returned.
--   If the caller retains the ticket, the item is guaranteed to stay in memory (and
--   thus in the 'WeakBag').  The ticket can also be used to remove the item from
--   the 'WeakBag' prematurely (i.e. while it is still alive), using 'remove'.
data WeakBagTicket a = WeakBagTicket
  {-# UNPACK #-} !(Weak a)
  {-# UNPACK #-} !a

-- | Insert an item into a 'WeakBag'.
insert ::
     a
  -> WeakBag a
  -> IO (WeakBagTicket a)
{-# INLINE insert #-}
insert a (WeakBag nextId children) = {-# SCC "insert" #-} do
  a' <- evaluate a
  myId <- atomicModifyIORef' nextId $ \n -> (succ n, n)
  let cleanup = atomicModifyIORef' children $ \cs -> (IntMap.delete myId cs, ())
  wa <- mkWeakPtr a' $ Just cleanup
  atomicModifyIORef' children $ \cs -> (IntMap.insert myId wa cs, ())
  return $ WeakBagTicket wa a'

-- | Create an empty 'WeakBag'.
empty :: IO (WeakBag a)
{-# INLINE empty #-}
empty = {-# SCC "empty" #-} do
  nextId <- newIORef 1
  children <- newIORef IntMap.empty
  return $ WeakBag nextId children

-- | Check whether a 'WeakBag' is empty.
isEmpty :: WeakBag a -> IO Bool
{-# INLINE isEmpty #-}
isEmpty (WeakBag _ children) = {-# SCC "isEmpty" #-} IntMap.null <$> readIORef children

singleton :: a -> IO (WeakBag a, WeakBagTicket a)
{-# INLINE singleton #-}
singleton a = {-# SCC "singleton" #-} do
  bag <- empty
  ticket <- insert a bag
  return (bag, ticket)

-- | Visit every node in the given list. If new nodes are appended during the
--   traversal, they will not be visited. Every live node that was in the list
--   when the traversal began will be visited exactly once; however, no guarantee
--   is made about the order of the traversal.
traverse :: forall a m. MonadIO m => WeakBag a -> (a -> m ()) -> m ()
{-# INLINE traverse #-}
traverse (WeakBag _ children) f = {-# SCC "traverse" #-} do
  cs <- liftIO $ readIORef children
  forM_ cs $ \c -> do
    ma <- liftIO $ deRefWeak c
    mapM_ f ma

remove :: WeakBagTicket a -> IO ()
{-# INLINE remove #-}
remove (WeakBagTicket w _) = {-# SCC "remove" #-} finalize w
