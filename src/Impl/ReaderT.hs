{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}
module Impl.ReaderT where

import           Challenges.MCPrelude
import           Challenges.UsingMonad
-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩
{-
好简单啊，就相当于 Context 中有个隐匿的 只读资源在actions 链中 传递。
-}
newtype Reader r a = Reader {runReader :: r->a}

instance Monad (Reader r) where
    -- 
    return va = Reader $ \r-> va
    -- bind :: (r->a) -> (a-> r->b) -> (r->b)
    m >>= f = Reader $ \r ->
        let va = runReader m r
        in runReader (f va) r



-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩

newtype ReaderT r m a = ReaderT {runReaderT :: r-> m a}

instance Monad m => Monad (ReaderT r m) where
    return va = ReaderT $ \r -> return va

    m >>= f = ReaderT $ \r-> do
        va <- runReaderT m r
        runReaderT (f va) r
