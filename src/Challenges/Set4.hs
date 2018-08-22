{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}
-- {-# LANGUAGE TypeSynonymInstances  #-} -- 允许用类型别名synonym 来注册instance
-- {-# LANGUAGE LiberalTypeSynonyms  #-}

module Challenges.Set4 where

import           Challenges.MCPrelude
import           Challenges.Set1
import           Challenges.Set2
import           Challenges.UsingMonad

-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩
{-—————————————————————————————1.Generalizing State and Maybe———————————————————————————————————————————————————-}
{-  <Set1 >
generalA :: (a -> b) -> Gen a -> Gen b
generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
-}
-- 对比 Set1 (随机数 State) 和 Set2 (携带失败可能性的 Maybe) 中的类型签名
-- 发现都存在  liftM2 :: (a -> b -> c) -> m a -> m b -> m c   即 bind ::Maybe a -> (a -> Maybe b) -> Maybe b

-- generalB2 :: 能够将两个前后承接的 action 串起、并对其结果进行 纯性的 组合
-- |▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ 为什么这里最后必须 return？ 而不能是 直接 Gen a -> Gen b->Gen c 
-- |▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
-- |▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
-- 这里的本质是  动作的捆绑，既然得到结果的 过程是有 副作用的，那么最终的结果必然也是 带副作用的。不会出现 “下降”:   m  a ->  a
-- generalB2' :: (a -> b -> c) -> Gen a -> Gen b -> c
-- generalB2' f gena genb =
--     gena `genTwo` \ra -> genb `genTwo` \rb -> f ra rb   <---- 这是无法编译通过的，因为 Action 链的 每个环都是action ，包括最终得到的结果。
-- |▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
-- |▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB2 f gena genb =
    gena `genTwo` \ra -> genb `genTwo` \rb -> mkGen $ f ra rb   -- 理解：这个mkGen 这样用 合理吗 ？？
{-   
-- |▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
--                            bind : “捆绑” 两个 承接的 actions ---> 成为 一个大的 Action
                                    解决的是： a function that does one step of two generators and the necessary state threading

    -- readDB/writeLog 在执行时都会修改全局的状态。             readDB s1  >>=  writeLog   >> writeLove
                                                             M ra     >>=  ra -> M rb 
                                 m a -> (a-> m b) -> m b
                        m 里只 囊括了 S,  但是上一步的运算结果 a 也是可以使用的 状态资源啊！！！！   
          ▇▇ ▇▇▇ ▇▇  ▇ ▇▇▇ ▇▇▇▇ ▇▇▇▇ ▇▇▇▇ ▇ ▇▇ ▇▇▇ ▇▇     （比如：State 中只涵盖了5个全局变量，  那么某个action 的结果 a 成了一个新的 可见变量，但并不包括在State 中！！）
                （当然了，虽然我们最后只取了 整个流程的结果。但是执行过的所有actions 其副作用已发生：全局状态都已改变！）
                        ▇ ▇▇▇ ▇▇▇▇ ▇▇▇▇ ▇▇▇▇ ▇ ▇▇ ▇▇▇ ▇▇ a 是 新出来的那部分state  (可供后续 actions 使用)      
【C++】                        
    -- readDB/writeLog 在执行时都会修改全局的状态。             readDB s1 >>= writeLog >> writeLove                        
    String str = "update TABLE"
    ra = readDB (s1); 
    {rb =}   writeLog (ra)); 
    {rc = }  writeLove "love it"

【Haskell- do】 
    let str =  "update TABLE"
    ra <-  readDB s1
    writeLog ra
    writeLove "love it" 


--                         ▇▇▇▇ ▇ ▇▇ ▇▇▇ ▇▇  (then)  >>  : 刚才这个 action 的结果值 在后续动作中 用不着 ！
【C++】
    -- readDB/writeLog 在执行时都会修改全局的状态。             readDB s1  >> writeLog
    String str = "update TABLE"
    ra = readDB (s1); 
    rb = writeLog "done";


【C++】
    -- readDB/writeLog 在执行时都会修改全局的状态。             { readDB s1   >>= \r1 -> }  writeLove  >>  writeLog r1
                                                                    M ra                  M rb           M rc
    String str = "update TABLE"
    ra = readDB (s1); 
    rb = writeLove "love it";  
    rc = writeLog (ra);  



-- |▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
-- |▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇  bind 是 C++ 版的 一连串的顺序的计算。

--         pure 的computations(连续运算：即每一步是对上一步的运算结果进行运算)    data -> calc1 -> calc2 -> calc3  = result 

--         而如果 其中的 每一步calc1/ calc2/ calc3 是有副作用的，  即 a->s -> (b,s) ， 那么要对data 进行连续运算就要 同时传递 s 了
--        \\ \ \\ \ \       bind 的 作用就是让你能在 calc1/calc2/calc3 都有副作用时，仍让能写 data -> calc1 >>= calc2 >>= calc2 = result
-- |▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
-- |▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
            -- bind 是用来串连 有副作用的 计算actions (computation) 用的
-- 当一连串的computation 都是没有副作用的时，全部顺起来用函数组合就行了、其他的combinator 为 where/let 里的子运算
-- 而bind 就是带副作用的 action 的先后发生，这种action 就和 命令式里的 “一段计算” 很像了！
-}
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

{-————————————————————————————4. Creating instances ———————————————————————————————————————————————-}

{-———————————————————————————————5. Revisiting other generic functions———————————————————————————————-}

{-———————————————————————————————6. Using the abstraction——————————————————————————————————————————————-}
