{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}
module Impl.ContT where

import           Challenges.MCPrelude
import           Challenges.UsingMonad
import           Impl.Typeclasses
-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩
-- | 上面我们连续调用了3次 f_cps :: a -> ((a -> r) -> r)      , 看能不能chain 起来                                     
chainCPS :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> (b -> r) -> r
--  ▇▇▇▇▇▇▇▇▇▇▇▇▇其实非 CPS 版本就是:
--                  a        ->     (a->b)               ->   b
--                 ($ a)       ->   a -> ($ b)           ->   b
--               挂起的a计算   ->  （a-> 挂起的b计算)        ->  挂起的b计算
--       （假设Context 已存在a值） ->................->  （能将 Context 中的值计算成b 值）

-- 怎么串两个函数：  1. 把一个函数当做另一个函数的参数   2. 提供一个初始值，完成链式计算

-- | Context 中的隐式状态是什么 ：  一个step 的中间结果 （传入Continuation 前就可以算是得到了一个结果）
-- | (a->r)->r     passAto       这是一个高阶函数！相当于提供了一个a 值、后面跟【一个消费a 的函数】（无其他要求）
-- | a->(b->r)->r  eatApassBto   这个先消费上一步的计算结果a、计算出一个b 值 +  继续向后面的函数传递
-- | b->r          eatB          【一个消费 B 的函数】
chainCPS passAto eatApassBto eatB = passAto $ \va -> eatApassBto va $ eatB


-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩

--  (a->r) -> r   会对应什么样的运算
newtype Cont r a = Cont {runCont :: (a->r)->r }

instance Monad (Cont r) where
    -- return :: a -> Cont r a
    return va = Cont ($ va)
    -- ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
    -- ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
    -- ▇▇▇▇▇▇▇  Cont r a   其实是抽象了一个运算中的两个部分：  
     --                   ① a:  这个运算的 本 step 的运算成果
     --                   ③ m :: (a->r)->r     要对它进行运算，需要  用 m 来传入一个 a->r 的函数 作为参数、对m 进行调用！！。
     --                                                           如 \va -> Cont r #   :: 类型 a->r
     --                                             即    Cont r a $ \va -> Cont r #    -----> 最后给一个 #->r（消费#类型参数的函数，如print）、 得到结果r
     --                   ② r:  而最后通过 Continuation 运算的最终结果  (即 Continuation 的结果)
    -- ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
    -- ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
    -- ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
    -- bind :: Cont r a  -> (a-> Cont r b) -> Cont r b
    -- bind :: { (a->r) ->r } -> { a-> (b->r) ->r } -> { (b->r) ->r }
    --         { (a->r) ->r } $ { a-> (b->r) ->r } -> { (b->r) ->r }
    -- 运算中每一个 $ 后面都是跟第三个参数： Continuation (即要将运算结果传入的)
    m >>= f = Cont $ \after -> runCont m $ \va -> runCont (f va) after


newtype ContT r m a = ContT {runContT :: (a->m r)-> m r}  -- 这个 Continuation 消耗a 值、最终产生结果是r （但会经过m 副作用）


instance Monad (ContT r m) where
    return va = ContT ($ va)    -- ($ va) :: (a->b)->b 
    m >>= k  = ContT $ \c -> runContT m (\x -> runContT (k x) c)

instance MonadTrans (ContT r) where
    lift m = ContT (m >>=)

instance (MonadIO m) => MonadIO (ContT r m) where
    liftIO = lift . liftIO
