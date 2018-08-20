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
