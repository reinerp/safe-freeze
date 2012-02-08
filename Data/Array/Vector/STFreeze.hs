-----------------------------------------------------------------------------
-- |
-- Module:      Data.Array.Vector.Freeze
-- License:     BSD3
-- Maintainer:  Reiner Pope <reiner.pope@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Support for using uvector's 'MUArr's in the new ST monad. Reexports
-- all of "Data.Array.Vector"'s API, but all the ST operations are
-- done with the new ST monad.
--
-- Notable differences in API:
--
--  * the former @unsafeFreezeMU@ is in fact safe, and is now named 'freezeMU'.
--
--  * similarly for @unsafeFreezeAllMU@
--
--  * @newU@ is no longer required as a primitive; it can be implemented using @runSTT@ and @freezeAllMU@, mentioning no unsafe operations.
--
--
-- Example demonstration of using the new ST monad:
--
-- @foo :: (UArr Int, UArr Int)
--foo = 'runST' go where
--  go :: forall s. 'ST' ('Normal' s) ('Freeze' s) (UArr Int, UArr Int)
--  go = newMU 5 >>>= \a1 ->
--       newMU 6 >>>= \a2 ->
--       writeMU a1 0 3 >>>= \() ->
--       writeMU a2 1 2 >>>= \() ->
--       freezeAllMU a1 >>>= \v1 ->
--       freezeAllMU a2 >>>= \v2 ->
--       return (v1,v2)@
-----------------------------------------------------------------------------

module Data.Array.Vector.STFreeze (

  -- * Array classes
  UA,

  -- (*) The pure and mutable array types
  UArr, MUArr,

  -- * Streaming pure arrays
  V.streamU, V.unstreamU,

  -- * Conversions to\/from lists
  V.toU, V.fromU,

  -- * Basic operations on pure arrays
  -- ** Introducing and eliminating UArrs
  V.emptyU,
  V.singletonU,

  -- ** Basic interface
  V.consU,
  V.snocU,
  -- uncons
  V.appendU,
  V.headU,
  V.lastU,
  V.tailU,
  V.initU,
  V.nullU,
  V.unitsU,
  V.lengthU,

  -- * Transforming UArrs
  V.mapU,

  -- * Reducing UArrs (folds)
  V.foldU,
  V.fold1U,
  V.fold1MaybeU,

  V.foldlU,
  V.foldl1U,
  V.foldl1MaybeU,

  -- ** Logical operations
  V.andU,
  V.orU,
  V.anyU,
  V.allU,

  -- * Arithmetic operations
  V.sumU, V.productU,
  V.maximumU, V.minimumU,
  V.maximumByU, V.minimumByU,
--  maximumIndexU, minimumIndexU,
--  maximumIndexByU, minimumIndexByU,

  -- * Building UArrs
  -- ** Scans
  V.scanlU,
  V.scanl1U,
  {-scanrU, scanr1U,-}
  V.scanU,
  V.scan1U,
  V.scanResU,

  -- ** Accumulating UArrs
  V.mapAccumLU,

  -- ** Generating UArrs
  V.iterateU,
  V.replicateU,
  V.replicateEachU,

  -- ** Unfolding UArrs

  V.unfoldU,  

  -- * Subarrays

  -- ** Breaking arrays
  V.sliceU,
--  extractU,
  V.takeU,
  V.dropU,
  V.splitAtU,
  V.takeWhileU,
  V.dropWhileU,
  {- spanU, breakU,-}

  -- * Searching Arrays

  -- ** Searching by equality
  V.elemU,
  V.notElemU,

  -- ** Searching with a predicate
  V.filterU,
  V.findU,
  V.findIndexU,

  -- * Indexing UArrs
  V.indexU,
  V.lookupU,

  -- * Zipping and unzipping
  V.zipU, V.zip3U,
  V.unzipU, V.unzip3U,
  V.zipWithU,
  V.zipWith3U,
  V.fstU,
  V.sndU,

  -- * Enumerations
  V.enumFromToU,
  V.enumFromToFracU,
  V.enumFromThenToU,
  V.enumFromStepLenU,
  V.enumFromToEachU,

{-
  -- * Low level conversions
  -- ** Copying arrays
  -- ** Packing CStrings and pointers
  -- ** Using UArrs as CStrings
  -- * I\/O with UArrs

  -- creating them, generating new arrays from old ones.
-}

------------------------------------------------------------------------

  V.combineU,
  V.packU,
  V.indexedU,
  V.repeatU,

{-
  -- * Permutations
  -- permuteU, bpermuteU, bpermuteDftU, reverseU, updateU,

  -- * Searching
  {- indexOfU,-}

  -- * Arrays of pairs
  {-crossU,-}

  -- * Random arrays
  -- randomU, randomRU,
-}

  -- * I\/O
  V.UIO(..),

  -- * Operations on mutable arrays
  V.lengthMU, newMU, readMU, writeMU, freezeMU, freezeAllMU,
  copyMU, permuteMU, atomicUpdateMU, unstreamMU,
  memcpyMU, memcpyOffMU, memmoveOffMU,
  V.unsafeZipMU, V.unsafeUnzipMU,

  
 ) where

import Control.Monad.ST.Freeze
import qualified Data.Array.Vector as V
import           Data.Array.Vector(MUArr, UArr, UA, (:*:))

newMU :: (UA e, MonadST st) => Int -> STNormal st s (MUArr e s)
newMU i = liftST (V.newMU i)

readMU :: (UA e, MonadST st) => MUArr e s -> Int -> STRead st s e
readMU a i = liftRead (V.readMU a i)

writeMU :: (UA e, MonadST st) => MUArr e s -> Int -> e -> STNormal st s ()
writeMU a i e = liftST (V.writeMU a i e)

copyMU :: (UA e, MonadST st) => MUArr e s -> Int -> UArr e -> STNormal st s ()
copyMU m i a = liftST (V.copyMU m i a)

freezeMU :: (UA e, MonadST st) => MUArr e s -> Int -> STFreeze st s (UArr e)
freezeMU m i = liftUnsafeFreeze (V.unsafeFreezeMU m i)

memcpyMU :: (UA e, MonadST st) => MUArr e s -> MUArr e s -> Int -> STNormal st s ()
memcpyMU m1 m2 i = liftST (V.memcpyMU m1 m2 i)

memcpyOffMU :: (UA e, MonadST st) => MUArr e s -> MUArr e s -> Int -> Int -> Int -> STNormal st s ()
memcpyOffMU m1 m2 i j k = liftST (V.memcpyOffMU m1 m2 i j k)

memmoveOffMU :: (UA e, MonadST st) => MUArr e s -> MUArr e s -> Int -> Int -> Int -> STNormal st s ()
memmoveOffMU m1 m2 i j k = liftST (V.memmoveOffMU m1 m2 i j k)

freezeAllMU :: (UA e, MonadST st) => MUArr e s -> STFreeze st s (UArr e)
freezeAllMU m = liftUnsafeFreeze (V.unsafeFreezeAllMU m)

permuteMU :: (UA e, MonadST st) => MUArr e s -> UArr e -> UArr Int -> STNormal st s ()
permuteMU m a b = liftST (V.permuteMU m a b)

atomicUpdateMU :: (UA e, MonadST st) => MUArr e s -> UArr (Int :*: e) -> STNormal st s ()
atomicUpdateMU m a = liftST (V.atomicUpdateMU m a)

unstreamMU m s = liftST (V.unstreamMU m s)