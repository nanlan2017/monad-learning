{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}
--
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}


module Impl.StateT where

import           Challenges.MCPrelude
import           Challenges.UsingMonad
import           Impl.Typeclasses
-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩
newtype State s a = State {runState :: s->(a,s)}

instance Monad (State s) where
    return va = State $ \s -> (va,s)
    --
    m >>= f = State $ \s ->
        let (va,s') = runState m s
        in runState (f va) s'

-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩
newtype StateT s m a = StateT {runStateT :: s-> m (a,s)}


-- | StateT s m 作为 Monad
instance (Monad m)=> Monad (StateT s m) where
    return va = StateT $ \s -> return (va,s)

    -- bind:: 
    m >>= f = StateT $ \s -> do
            (va,s') <- runStateT m s
            runStateT (f va) s'   -- 后面的 f::a->m b  ， 我只要把 前面的m a 中的a 值拆出来、 则 f a 就是后面的这个完整的action 了啊！
            -- 而 结果的 m b 就是 第二个action 的执行结果。

-- | StateT s m 作为 MonadState
instance (Monad m)=> MonadState s (StateT s m) where
    -- get :: StateT s m s
    get = StateT $ \s-> return (s,s)
    -- put :: s -> StateT s m ()
    -- put s = StateT $ \s-> return ((),s)
    put s = StateT $ \ _ -> return ((),s)   -- ??

-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩
-- | StateT s m 继承 m的 Monad特性
instance (MonadPlus m)=> MonadPlus (StateT s m) where
    mzero = StateT $ \_-> mzero  -- ?
    -- mplus :: 
    (StateT m) `mplus` (StateT k) = StateT $ \s ->  m s `mplus` k s
