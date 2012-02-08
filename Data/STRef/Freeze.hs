-----------------------------------------------------------------------------
-- |
-- Module:      Data.STRef.Freeze
-- License:     BSD3
-- Maintainer:  Reiner Pope <reiner.pope@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Description of this module goes here.
-----------------------------------------------------------------------------


module Data.STRef.Freeze (
  ST.STRef,
  newSTRef,
  readSTRef,
  writeSTRef,
  modifySTRef,
 ) where

import           Control.Monad.ST.Freeze
import qualified Data.STRef as ST

newSTRef :: (MonadST st) => a -> STNormal st s (ST.STRef s a)
newSTRef a = liftST (ST.newSTRef a)

readSTRef :: (MonadST st) => ST.STRef s a -> STRead st s a
readSTRef r = liftRead (ST.readSTRef r)

writeSTRef :: (MonadST st) => ST.STRef s a -> a -> STNormal st s ()
writeSTRef r a = liftST (ST.writeSTRef r a)

modifySTRef :: (MonadST st) => ST.STRef s a -> (a -> a) -> STNormal st s ()
modifySTRef r f = liftST (ST.modifySTRef r f)