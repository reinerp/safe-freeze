-----------------------------------------------------------------------------
-- |
-- Module:      Control.Monad.ST.Freeze
-- License:     BSD3
-- Maintainer:  Reiner Pope <reiner.pope@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- An indexed version of the ST monad with support for safely freezing
-- multiple arrays. Compare to the usual ST monad, where the only support
-- for safely freezing arrays is runSTUArray -- only one array may be frozen.
--
-- This version of the ST monad has two distinct stages of processing:
-- the /normal/ stage, and the /freeze/ stage. Reading and writing are
-- permitted in the normal stage; reading and freezing are permitted
-- in the freeze stage. This policy ensures that no writes occur after
-- the arrays have been frozen.
--
-- This ST is an /indexed/ monad (see "Control.Monad.Indexed") as well
-- as a normal monad. That is, each monadic value will have an
-- \"ingoing\" state thread as well as an \"outgoing\" state
-- thread. These state threads are similar to the ST monad's state
-- thread, except that they are now annotated with a stage name:
-- either 'Normal' or 'Freeze'.
-----------------------------------------------------------------------------

module Control.Monad.ST.Freeze (
  -- * Stage names
  Normal,
  Freeze,
  -- * MonadST class
  MonadST(..),
  STNormal,
  STFreeze,
  STRead,
  -- * ST monad
  ST,
  runST,
  -- * ST transformer
  STT, 
  STTBase(..),
 ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Identity(Identity(..))
import qualified Control.Monad.ST as S (ST)
import qualified Control.Monad.ST.Unsafe as S
import           Control.Monad.Indexed(IxMonad(..),IxFunctor(..),IxPointed(..),IxApplicative(..))
import           System.IO.Unsafe

data Normal s
data Freeze s

-- | A computation containing some writes but no freezes: it starts
-- | and ends in the 'Normal' stage.
type STNormal st s a = st (Normal s) (Normal s) a
-- | A computation containing only reads: it starts and ends in any
-- stage, but does not change stage. (Note that there would be no loss
-- of safety in allowing the stage to change, but it may result in
-- ambiguous types, or extra type annotations being required.)
type STRead   st s a = forall stg. st (stg s) (stg s) a
-- | A computation containing some freezes but no writes: it starts in
-- | any stage, but ends in the Freeze stage.
type STFreeze st s a = forall stg. st (stg s) (Freeze s) a

class IxMonad st => MonadST st where
    -- | For lifting any operations containing writes but no freezes.
    liftST :: S.ST s a -> STNormal st s a
    -- | For lifting any operations containing reads but no writes or freezes.
    liftRead :: S.ST s a -> STRead st s a
    -- | For lifting an @unsafeFreeze@ operation
    liftUnsafeFreeze :: S.ST s a -> STFreeze st s a

-- | An ST monad transformer. However, this is not a genuine monad
-- transformer, as that would be unsafe (see
-- <http://www.haskell.org/pipermail/glasgow-haskell-users/2009-February/016554.html>). To
-- retain safety, it may only act as a monad transformer over the
-- 'Identity' and 'IO' monads, enforced by the 'STTBase' typeclass.
--
-- Defining 'STT' as a monad transformer rather than just a monad
-- allows ST arrays to be used in the 'IO' monad, bringing safe
-- freezing also to the 'IO' monad.
newtype STT (m :: * -> *) s t a = STT { unSTT :: IO a }

class STTBase m where
    -- | Runs the monad
    runSTT :: (forall s. STT m (stg1 s) (stg2 s) a) -> m a
    -- | A restricted analog of Control.Monad.Trans.lift.
    lift :: m a -> STT m (stg s) (stg s) a

type ST = STT Identity

-- | @runST = 'runIdentity' . 'runSTT'@
runST :: (forall s. STT Identity (stg1 s) (stg2 s) a) -> a
runST = runIdentity . runSTT

instance STTBase IO where
    runSTT m = unSTT m
    lift = STT
instance STTBase Identity where
    runSTT m = Identity (unsafePerformIO (unSTT m))
    lift = STT . return . runIdentity

instance IxFunctor (STT m) where
    imap f (STT m) = STT (fmap f m)
instance IxPointed (STT m) where
    ireturn a = STT (return a)
instance IxApplicative (STT m) where
    iap (STT m) (STT n) = STT (m `ap` n)
instance IxMonad (STT m) where
    ibind k (STT m) = STT (m >>= (unSTT . k))
instance MonadST (STT m) where
    liftST s = STT (S.unsafeSTToIO s)
    liftRead s = STT (S.unsafeSTToIO s)
    liftUnsafeFreeze s = STT (S.unsafeSTToIO s)

instance Functor (STT m s t) where
    fmap f = imap f
instance Applicative (STT m s s) where
    pure = ireturn
    (<*>) = iap
instance Monad (STT m s s) where
    (>>=) = flip ibind
    return = ireturn