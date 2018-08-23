{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}
module Impl.ExceptT where

import           Challenges.MCPrelude
import           Challenges.UsingMonad
import           Impl.Typeclasses
-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩
instance Monad (Either e) where
    return = Right
    -- bind :: Either e a -> (a-> Either e b) -> Either e b
    m >>= f = case m of
        Left err -> Left err
        Right va -> f va



newtype ExceptT e m a = ExceptT {runExceptT :: m (Either e a)}


instance Monad m => Monad (ExceptT e m) where
    -- return :: 
    return va = ExceptT $ return $ Right va
    -- bind :: m (Either e a) ->   (a-> m (Either e b)) ->  m (Either e b)
    m >>= f = ExceptT $ do
        ev1 <- runExceptT m
        case ev1 of
            Left err -> return $ Left err
            Right va -> runExceptT $ f va





