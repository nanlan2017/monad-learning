{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}
module Impl.MaybeT where

import           Challenges.MCPrelude
import           Challenges.UsingMonad
import           Impl.Typeclasses
-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance (Monad m)=> Monad (MaybeT m) where
    -- return :: a -> MaybeT m a
    return = MaybeT. return .Just

    -- bind :: MaybeT m a -> (a->MaybeT m b)-> MaybeT m b
    m >>= f = MaybeT $ do
                maybeV <- runMaybeT m
                case maybeV of
                    Nothing -> return Nothing
                    Just va -> runMaybeT $ f va   -- 实现bind时，只要能让下一个动作所需的a 得到上一步的a 就可以了！！

instance MonadTrans MaybeT where
    -- lift :: m a -> MaybeT m a   
    -- lift m = MaybeT $ m >>= (return.Just)
    lift = MaybeT. liftM Just


instance (Monad m)=> MonadPlus (MaybeT m) where
    -- mzero :: MaybeT m a
    mzero = MaybeT $ return Nothing
    -- mplus :: m (Maybe a) -> m (Maybe a)-> m (Maybe a)
    m `mplus` k = MaybeT $ do
        maybeV1 <- runMaybeT m
        case maybeV1 of
            Nothing -> runMaybeT k
            Just _ -> return maybeV1






