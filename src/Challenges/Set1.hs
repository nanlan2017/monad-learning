{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}
module Challenges.Set1 where

import           Challenges.MCPrelude
-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩
{- ———————————————————————————— 1. Random number generation——————————————————————————————————————————— -}
fiveRands :: [Integer]
fiveRands =
    let (r1, s1) = rand $ mkSeed 1
        (r2, s2) = rand s1
        (r3, s3) = rand s2
        (r4, s4) = rand s3
        (r5, s5) = rand s4
    in  [r1, r2, r3, r4, r5]

v00 = product fiveRands
-- v01 = Just 4  -- ▇▇▇▇▇编译选项具有传递性：由于 MCPrelude 中指定了编译选项“NoImplicitPrelude"，则依赖 MCPrelude 的文件中也不导入默认的 Prelude
{- ————————————————————————————2. Random character generation ———————————————————————————————————————— -}
randLetter :: Seed -> (Char, Seed)
randLetter seed = let (r, s) = rand seed in (toLetter r, s)

randString3 :: String
randString3 =
    let (c1, s1) = randLetter $ mkSeed 1
        (c2, s2) = randLetter s1
        (c3, s3) = randLetter s2
    in  [c1, c2, c3]
{- ———————————————————————————— 3. More generators ———————————————————————————————————————— -}
type Gen a = Seed -> (a, Seed)

randEven :: Gen Integer
randEven = (\(r, seed) -> (r * 2, seed)) . rand

randOdd :: Gen Integer
randOdd = (\(r, seed) -> (r + 1, seed)) . randEven

randTen :: Gen Integer
randTen = (\(r, seed) -> (r * 10, seed)) . rand

-- ▇▇▇▇▇▇ 这时候可以去抽象出来了： 这就是”链式处理“的bind： 即对上一步的结果r 部分进行继续处理，而不影响 Context 中的 Seed

generalA :: (a -> b) -> Gen a -> Gen b
-- generalA f gen seed = (f ra, seed') where (ra, seed') = gen seed
generalA f gen = \seed -> let (ra, seed') = gen seed in (f ra, seed')


randEven' = generalA (* 2) rand
randOdd' = generalA (+ 1) randEven'
randTen' = generalA (* 10) rand

-- ◯◯◯◯◯◯◯◯◯ test ◯◯◯◯◯◯◯◯◯◯ --
seed0 = mkSeed 1
v01 = product $map (fst . ($ seed0)) [randEven', randOdd', randTen']
-- ◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯
{- ———————————————————————————— 4. Generalizing random pairs—————————————————————————————————————————————————— -}
randPair :: Gen (Char, Integer)
randPair seed = ((c, x), seed'')
  where
    (c, seed' ) = randLetter seed
    (x, seed'') = rand seed'


-- |用 IO 来想象的话极其自然啊！▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
-- 有个 IO动作，能得到一个结果a、还有个 IO action,能得到结果b，那么给我这两个动作，我自然可以得到一个（a,b)    
generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair ga gb seed =
    let (xa, s' ) = ga seed
        (xb, s'') = gb s'
    in  ((xa, xb), s'')

randPair' = generalPair randLetter rand

-- | Instead of always constructing pairs, you should be able to have a generalization that can construct anything. 
-- All you need to do is pass in a function that does the constructing with two inputs. 
generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f ga gb seed =
    let (ra, s' ) = ga seed
        (rb, s'') = gb s'
    in  (f ra rb, s'')

generalPair2 = generalB (,)
{- —————————————————————————————— 5. Generalizing lists of generators ——————————————————————————————————————————————————————— -}
-- | ...rather painful... You have to thread the output seed from one rand call to the input of the next call. This is tedious and error prone,
-- 计算过程： gen1 seed = (r1, seed1) , gen2 seed1 = (r2, seed2) ... 所以ACC栈里是（r1++r2..,  newestSeed)
repRandom :: [Gen a] -> Gen [a]
repRandom gens initSeed = foldl
    (\(rs, lastSeed) gen -> let (r, s) = gen lastSeed in (rs ++ [r], s))
    ([], initSeed)
    gens

-- 方式2：使用递归
repRandom' :: [Gen a] -> Gen [a]
repRandom' []       seed = ([], seed)
repRandom' (g : gs) seed = (r : rs, snew)
  where
    (r , s   ) = g seed
    (rs, snew) = repRandom' gs s

-- ◯◯◯◯◯◯◯◯◯ test ◯◯◯◯◯◯◯◯◯◯ --
gens = replicate 5 rand
-- ◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯
{- ————————————————————————————6. Threading the random number state ————————————————————————————————————————————— -}
-- A simpler idea is to have a function that does one step of two generators and the necessary state threading. 
-- Its first argument will be a generator. 
-- Its second argument will be a function that takes the result of the first generator and returns a second generator. 

{-❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑
-- ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ 《 一段命令式运算过程的抽象：理解 Action a -> (a->Action b) -> Action b 》
    想像一段 C++程序： 
                首先存在全局的状态变量 s0，
                action1 执行时会修改这些环境变量 : 在 s0 => (a,s1)中  就体现着状态s 被修改，并action1也会计算出结果a
                接下来，action2同样依赖/修改这些环境变量，（所以是 s1 => (b,s2) 。 而且其还可以使用上一步action1的计算结果a

    整个系统的状态总和变化 ： s0  ---(act1)--->  a, s1 ---(act2)---> b, s2
    就是说：结合 act1 和act2后，得到了一个从 s0 -> (b, s2) 的合成的action

   ▇▇▇▇▇▇▇▇▇ a -> Action b 的意思理解为： 在 Action发生前，除了系统状态 S外还有一个代表之前的局部结果 a  , 而 接下来后方的 Action-s 会用到这个 a 
                                                                                        （顺序式的计算过程肯定是 在前面的局部基础上继续啊！）



    -- Action-s 执行前存在着全局状态 s                                                                                        
    -- Action-s 执行前存在着前面的运算结果 a  
            a -> s -> (b,s)                                                                                      
    -- Action-s 执行后会产生一个结果： b
    -- Action-s 执行后还会有副作用：s'                                                                                    
❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑❑-}
genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo ga f = \initSeed -> let (ra, seed') = ga initSeed in (f ra) seed'

-- | repRandom' [] seed = ([],seed)   
-- 本质上是凭空构造出一个 Gen a
mkGen :: a -> Gen a
mkGen x seed = (x, seed)   -- 其语义： mkGen x = \state -> (x,state) ,即 mkGen x 使得 x 成为了（一个假设出来的 Action 的） 结果。
