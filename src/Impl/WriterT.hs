{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}
module Impl.WriterT where

import           Challenges.MCPrelude
import           Challenges.UsingMonad
import           Impl.Typeclasses
-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩
{-
好简单啊，就相当于 Context 中有本 notebook， 在传递
每个 action 都可以选择 在上面 写东西 （但是不能从上面去东西）
-}

newtype Writer w a = Writer {runWriter :: (a,w)}

-- | 特殊：w 必须是 Monoid ，则 Writer w 才是 Monad
instance (Monoid w)=> Monad (Writer w) where
    return va = Writer $ (va,mempty)
    -- 
    m >>= f = Writer $
     let
        (va,w1) = runWriter m
        (vb,w2) = runWriter (f va)
     in
        (vb,w1 `mappend` w2)




-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩
newtype WriterT w m a = WriterT {runWriterT:: m (a,w)}

instance (Monoid w,Monad m )=> Monad (WriterT w m) where
    return va = WriterT $ return (va, mempty)
    -- m (a,w) ->  (a-> m (b,w)) -> m (b,w)
    m >>= f = WriterT $ do
        (va,w1) <- runWriterT m
        (vb,w2) <- runWriterT (f va)
        return (vb,w1 `mappend` w2)
