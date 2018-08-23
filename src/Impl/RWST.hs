{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}
module Impl.RWST where

import           Challenges.MCPrelude
import           Challenges.UsingMonad
import           Impl.Typeclasses

-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩
newtype RWST r w s m a = RWST {runRWST :: r->s-> m (a,w,s) }

instance (Monoid w, Monad m)=> Monad (RWST r w s m) where
    -- return :: a -> 
    return va = RWST $ \r s -> return (va,mempty,s)

    -- 
    m >>= f = RWST $ \r s -> do
        (va,w1,s') <- runRWST m r s
        (vb,w2,s'') <- runRWST (f va) r s'
        return (vb, w1 `mappend` w2 , s'')
