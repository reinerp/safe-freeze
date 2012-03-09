-----------------------------------------------------------------------------
-- |
-- Module:      Data.Array.Vector.Freeze
-- License:     BSD3
-- Maintainer:  Reiner Pope <reiner.pope@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Proof-of-concept support for using vector's 'MVector's in the new ST monad. Reexports
-- only a minimal API from "Data.Vector.Generic.Mutable", with all ST operations
-- done in the new ST (indexed) monad.
--
-- Notable differences in API:
--
--  * the former @unsafeFreeze@ is in fact safe, and is now named 'freeze'.
--
-- Example demonstration of using the new ST monad:
--
-- @foo :: forall v. Vector v Int => (v Int, v Int)
--foo = 'runST' go where
--  go :: forall s. 'ST' ('Normal' s) ('Freeze' s) (v Int, v Int)
--  go = new 5 >>>= \a1 ->
--       new 6 >>>= \a2 ->
--       write a1 0 3 >>>= \() ->
--       write a2 1 2 >>>= \() ->
--       freeze a1 >>>= \v1 ->
--       freeze a2 >>>= \v2 ->
--       return (v1,v2)@
-----------------------------------------------------------------------------

module Data.Vector.Generic.Mutable.STFreeze (
  new,
  read,
  write,
  copy,
  freeze,
 ) where

import Prelude hiding(read)
import Control.Monad.ST.Freeze
import qualified Data.Vector.Generic.Mutable as V
import qualified Data.Vector.Generic as V(Vector, Mutable, unsafeFreeze)


new :: (V.MVector v a, MonadST st) => Int -> STNormal st s (v s a)
new i = liftST (V.new i)

read :: (V.MVector v a, MonadST st) => v s a -> Int -> STRead st s a
read a i = liftRead (V.read a i)

write :: (V.MVector v a, MonadST st) => v s a -> Int -> a -> STNormal st s ()
write a i e = liftST (V.write a i e)

copy :: (V.MVector v a, MonadST st) => v s a -> v s a -> STNormal st s ()
copy a b = liftST (V.copy a b)

freeze :: (V.Vector v a, MonadST st) => (V.Mutable v) s a -> STFreeze st s (v a)
freeze m = liftUnsafeFreeze (V.unsafeFreeze m)
