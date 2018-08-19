{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Impl.Monad where

class IFunctor f where
    fmap :: (a->b) -> f a -> f b

class IApplicative m where
    pure :: a -> m a
    (<*>):: m (a->b) -> m a -> m b

class IMonad m where
    return :: a -> m a
    (>>=) :: m a -> (a-> m b)-> m b
    (>>):: m a -> m b -> m b
-- **********************************************************************************
class IMonadReader r m | m-> r where
    ask :: m r
    local :: (r->r) -> m a -> m a
    reader :: (r->a) -> m a

asks :: (IMonadReader r m) => (r -> a) -> m a
asks f = undefined
