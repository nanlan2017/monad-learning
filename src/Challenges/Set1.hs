{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}
module Challenges.Set1 where

import           Challenges.MCPrelude
import           Challenges.UsingMonad
-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩

{- ▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃ 1. Random number generation -}
-- 本质：安排5个action，搜集它们的结果 （以纯函数的方式进行 状态传递）
fiveRands :: [Integer]
fiveRands =
    let (r1, s1) = rand $ mkSeed 1
        (r2, s2) = rand s1
        (r3, s3) = rand s2
        (r4, s4) = rand s3
        (r5, s5) = rand s4
    in  [r1, r2, r3, r4, r5]

-- ◯◯◯◯◯◯◯◯◯◯◯◯ test ◯◯◯◯◯◯◯◯◯◯◯◯◯ --
-- v00 = product fiveRands
-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩

fiveRands_M :: [Integer]
fiveRands_M = evalGen (sequence (replicate 5 $ WrapGen rand)) $ mkSeed 1

fiveRands_M' :: [Integer]
fiveRands_M' =
    let gen  = WrapGen rand
        gen5 = sequence $ replicate 5 gen
    in  evalGen gen5 (mkSeed 1)

fiveRands_D :: [Integer]
fiveRands_D = evalGen gen5 (mkSeed 1)
  where
    gen  = WrapGen rand
    gen5 = do
        r1 <- gen
        r2 <- gen
        r3 <- gen
        r4 <- gen
        r5 <- gen
        return [r1, r2, r3, r4, r5]
{- ▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃ 2. Random character generation -}
-- 本质：一个 action ： 得到值 + 对结果值 pure computation    ::  写成pointerless :   toLetter =<< rand
randLetter :: Seed -> (Char, Seed)
randLetter seed = let (r, s) = rand seed in (toLetter r, s)

randString3 :: String
randString3 =
    let (c1, s1) = randLetter $ mkSeed 1
        (c2, s2) = randLetter s1
        (c3, s3) = randLetter s2
    in  [c1, c2, c3]
-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩
randLetter_M :: Seed -> (Char, Seed)
randLetter_M = runGen $ toLetter <$> WrapGen rand

randString3_M :: String
randString3_M =
    evalGen (sequence $ replicate 3 (WrapGen randLetter_M)) $ mkSeed 1
{- ▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃ 3. More generators -}
type G a = Seed -> (a, Seed)

randEven :: G Integer
randEven = (\(r, seed) -> (r * 2, seed)) . rand

randOdd :: G Integer
randOdd = (\(r, seed) -> (r + 1, seed)) . randEven

randTen :: G Integer
randTen = (\(r, seed) -> (r * 10, seed)) . rand

-- ▇▇▇▇▇▇ 这时候可以去抽象出来了： 这就是”链式处理“的bind： 即对上一步的结果r 部分进行继续处理
-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩
randEven_M :: Gen Integer  -- randEven_M 是 囊括seed的状态 在内的一个计算动作。
randEven_M = fmap (* 2) $ WrapGen rand


randOdd_M = fmap (+ 1) randEven_M



generalA :: (a -> b) -> G a -> G b
-- 注意：可供组装的参数里 只发生了一次action
generalA f gen = \seed -> let (ra, seed') = gen seed in (f ra, seed')


randEven' = generalA (* 2) rand
randOdd' = generalA (+ 1) randEven'
randTen' = generalA (* 10) rand

-- ◯◯◯◯◯◯◯◯◯ test ◯◯◯◯◯◯◯◯◯◯ --
seed0 = mkSeed 1
v01 = product $map (fst . ($ seed0)) [randEven', randOdd', randTen']
-- ◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯
{- ▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃ 4. Generalizing random pairs -}
-- 本质： 多个action 的 串接
randPair :: G (Char, Integer)
randPair seed = ((c, x), seed'')
  where
    (c, seed' ) = randLetter seed
    (x, seed'') = rand seed'

randPair_M = liftM2 (,) (WrapGen randLetter_M) (WrapGen rand)
-- |用 IO 来想象的话极其自然啊！▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
-- 有个 IO动作，能得到一个结果a、还有个 IO action,能得到结果b，那么给我这两个动作，我自然可以得到一个（a,b)    
-- ▇▇▇▇▇▇▇▇▇▇ 注意： 这两个action 是连续发生的，（后一个action 是基于前一个发生后的状态），所以 是可以 打包成 一个action
generalPair :: G a -> G b -> G (a, b)
generalPair ga gb seed =
    let (xa, s' ) = ga seed
        (xb, s'') = gb s'
    in  ((xa, xb), s'')

randPair' = generalPair randLetter rand


generalB :: (a -> b -> c) -> G a -> G b -> G c
generalB f ga gb seed =
    let (ra, s' ) = ga seed
        (rb, s'') = gb s'
    in  (f ra rb, s'')

generalPair2 = generalB (,)
{-▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃ 5. Generalizing lists of generators  -}
-- | 本质： 多个action 的串联+ 结果合并
-- 计算过程： gen1 seed = (r1, seed1) , gen2 seed1 = (r2, seed2) ... 所以ACC栈里是（r1++r2..,  newestSeed)
repRandom :: [G a] -> G [a]
repRandom gens initSeed = foldl
    (\(rs, lastSeed) gen -> let (r, s) = gen lastSeed in (rs ++ [r], s))
    ([], initSeed)
    gens

-- 方式2：使用递归
repRandom' :: [G a] -> G [a]
repRandom' []       seed = ([], seed)
repRandom' (g : gs) seed = (r : rs, snew)
  where
    (r , s   ) = g seed
    (rs, snew) = repRandom' gs s

-- ◯◯◯◯◯◯◯◯◯ test ◯◯◯◯◯◯◯◯◯◯ --
gens = replicate 5 rand
-- ◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯
{- ▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃ 6. Threading the random number state  -}
-- A simpler idea is to have a function that does one step of two generators and the necessary state threading. 
-- Its first argument will be a generator. 
-- Its second argument will be a function that takes the result of the first generator and returns a second generator. 

genTwo :: G a -> (a -> G b) -> G b
-- (Seed -> (a,Seed))          ->     (a-> (Seed -> (b,Seed)))        ->     Seed        ->     (b,Seed)
-- (initSeed -> (ra,seed'))    ->     (ra-> Seed' -> (rb,seed'')))    ->     【 initSeed    ->     (rb,seed'') 】
genTwo ga f initSeed = let (ra, seed') = ga initSeed in f ra seed'

-- | repRandom' [] seed = ([],seed)   
-- 本质上是凭空构造出一个 Gen a
mkGen :: a -> G a
mkGen x seed = (x, seed)   -- 其语义： mkGen x = \state -> (x,state) ,即 mkGen x 使得 x 成为了（一个假设出来的 Action 的） 结果。
