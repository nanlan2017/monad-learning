{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}
-- {-# LANGUAGE TypeSynonymInstances  #-} -- 允许用类型别名synonym 来注册instance
-- {-# LANGUAGE LiberalTypeSynonyms  #-}

module Challenges.Set4 where

import           Challenges.MCPrelude
import           Challenges.Set1
import           Challenges.Set2
-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩
{-—————————————————————————————1.Generalizing State and Maybe———————————————————————————————————————————————————-}
{-  <Set1 >
generalA :: (a -> b) -> Gen a -> Gen b
generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
-}
-- 对比 Set1 (随机数 State) 和 Set2 (携带失败可能性的 Maybe) 中的类型签名
-- 发现都存在  liftM2 :: (a -> b -> c) -> m a -> m b -> m c   即 bind ::Maybe a -> (a -> Maybe b) -> Maybe b

-- generalB2 :: 能够将两个前后承接的 action 串起、并对其结果进行 纯性的 组合
generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB2 f gena genb =
    gena `genTwo` \ra -> genb `genTwo` \rb -> mkGen $ f ra rb   -- 理解：这个mkGen 这样用 合理吗 ？？
-- |▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
-- |▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
-- |▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇  bind 是 C++ 版的 一连串的顺序的计算。
-- |▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
-- |▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
            -- bind 是用来串连 有副作用的 计算actions (computation) 用的
-- 当一连串的computation 都是没有副作用的时，全部顺起来用函数组合就行了、其他的combinator 为 where/let 里的子运算
-- 而bind 就是带副作用的 action 的先后发生，这种action 就和 命令式里的 “一段计算” 很像了！

-- ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ 本计算的实际语义 ： 串联一连串computation actions，并收集它们的结果。
repRandom'' :: [Gen a] -> Gen [a]
repRandom'' [] = mkGen []
repRandom'' (g : gs) =   -- 取出 r 和 后续动作的 Gen [rs] 结果进行合并
    g `genTwo` \r -> repRandom'' gs `genTwo` \rs -> mkGen $ r : rs
    -- ▇▇▇▇▇▇▇▇▇▇▇▇▇注意：代码里已经不会出现seed 了，（其实seed 是“隐式状态”） ---> 已经放在action 的副作用环境中进行传递

-- ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇  当你知道这个类型 代表的 实际顺序的行为后， 你就可以 随意实现它。 这就是Monad 作为actions 的 Combinator 吧！！ 
-- generalA :: (a->b)-> Gen a -> Gen b     ： 含义为： 给我一个由环境计算出a 值+ 对a 转成b  ---> 我能 从环境得到一个b 值   
-- generalA f :: 能对 一个 Action-s (▇▇ 必须是承接连在一起的一串 ) 的结果 + 直接进行 纯处理
repRandom''' :: [Gen a] -> Gen [a]
repRandom''' (g : gs) = generalB2 (:) g (repRandom''' gs)
{-—————————————————————————————2. A missed generalization ———————————————————————————————————————————-}



{-——————————————————————————————3. Formalizing the pattern —————————————————————————————————————————————-}
class Monad m where
    bind :: m a -> (a-> m b)-> m b
    return :: a -> m a

newtype Generator a = Generator { runGenerator ::Seed -> (a,Seed)}

-- genTwo / link (>=>)
instance Monad Generator where
    return x = Generator $ \seed -> (x,seed)
    -- bind :: Generator a -> (a->Generator b) -> Generator b
    bind genA f = Generator $ \seed ->
        let
            (ra,seed') = runGenerator genA seed
        in
            runGenerator (f ra) seed'

instance Monad Maybe where
    return = Just
    -- bind :: 
    bind Nothing _ = Nothing
    bind (Just x) f = f x

instance Monad [] where
    return x = [x]
    bind = flip concatMap

{-————————————————————————————4. Creating instances ———————————————————————————————————————————————-}
-- ▇▇▇▇▇▇▇ Gen a 是一个动作action， 但这个动作并不是能直接执行得到结果值 （如同 IO），而是执行时需要有 预置状态s的动作！    
evalGenerator :: Generator a -> Seed -> a
evalGenerator gen seed = fst $ runGenerator gen seed

{-———————————————————————————————5. Revisiting other generic functions———————————————————————————————————————————————-}
                 -- this time do everything in terms of return and bind.
-- Set1 : repRandom
sequence :: (Monad m) => [m a] -> m [a]
sequence (act1 : actions) =
    act1 `bind` \r1 -> sequence actions `bind` \rs -> return $ r1 : rs

-- Set1 : generalB
liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f actA actB = actA `bind` \ra -> actB `bind` \rb -> return $ f ra rb

-- Set2 : >=>
chain :: Maybe a -> (a -> Maybe b) -> Maybe b
chain = bind -- flip bind

-- Set2 : combine
join :: Maybe (Maybe a) -> Maybe a
join m = m `bind` id

-- Set3 : allCombs  -- liftM2

-- Set3 : allCombs3
liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f actA actB actC =
    actA `bind` \ra -> actB `bind` \rb -> actC `bind` \rc -> return $ f ra rb rc
-- Set3 : combStep
-- | ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ 这地方有点乱： action 的承接、先后关系？？ 对副作用状态的 连贯影响 ？？
-- | ▇▇▇▇▇▇▇ 可以用 monad 的 return /bind 实现 Applicative 的 ap 吗？
ap :: [a -> b] -> [a] -> [b]  -- m (a->b ) -> m a -> m b  
-- ▇▇▇▇▇▇▇▇▇▇▇▇▇▇ 理解该计算的语义：两个独立的动作，一个动作会得到一个函数、一个动作得到一个值 ---> “先后化- 让它们发生 ”这两个动作
-- ▇▇▇▇▇▇▇▇▇▇▇▇▇▇  注意： 这时候只是“安排它们发生”，结果部分先后！   action1 >>= \r1 -> action2 ...   还是  action2 >>= \r2 -> action1 ...
ap fl al = fl `bind` \f -> al `bind` \ss -> return $ f ss


{-———————————————————————————————6. Using the abstraction——————————————————————————————————————————————-}
