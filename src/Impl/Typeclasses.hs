{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}


module Impl.Typeclasses where

import           Challenges.MCPrelude
import           Challenges.UsingMonad
import           Prelude                        ( IO )
-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩
class Functor f where
    (<$>) :: (a->b) -> f a -> f b

class Applicative m where
    pure :: a -> m a
    (<*>):: m (a->b) -> m a -> m b

class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a
    -- ? some / many
    some :: f a -> f [a]
    many :: f a -> f [a]


class Monoid a where
    mempty :: a
    mappend :: a -> a -> a
-- **********************************************************************************
class MonadReader r m | m-> r where
    ask :: m r
    local :: (r->r) -> m a -> m a
    reader :: (r->a) -> m a

asks :: (MonadReader r m) => (r -> a) -> m a
asks f = undefined

class (Monad m)=> MonadWriter w m | m -> w where
    tell :: w -> m ()

class (Monad m)=> MonadState s m | m -> s where
    get :: m s
    put :: s -> m ()
-- **********************************************************************************
class (Monad m)=> MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

-- guard::     

class (Monad m)=> MonadIO m where
    liftIO :: IO a -> m a

-- **********************************************************************************
class MonadTrans t where
    lift :: Monad m => m a -> t m a
