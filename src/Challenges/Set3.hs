{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}
module Challenges.Set3 where

import           Challenges.MCPrelude
import           Challenges.UsingMonad
-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩
{-———————————————————————————1. Generating combinations  ———————————————————————————————————————————-}
-- allPairs [1,2] [3,4] == [(1,3),(1,4),(2,3),(2,4)]
allPairs :: [a] -> [b] -> [(a, b)]
allPairs []       _  = []
allPairs _        [] = []
allPairs (x : xs) ys = map ((,) x) ys ++ allPairs xs ys

{-———————————————————————————————————2. Poker hands————————————————————————————————————————————————-}
data Card = Card Int String

instance Show Card where
    show (Card i s) = show i ++ s

allCards :: [Int] -> [String] -> [Card]  -- 函数组合得到的函数、函数调用 $ 得到的可能是函数/值
allCards is ss = map (\(i, s) -> Card i s) $ allPairs is ss

-- 不使用allPairs 实现
allCards'' :: [Int] -> [String] -> [Card]
allCards'' (i : is) ss = map (Card i) ss ++ allCards' is ss

{-————————————————————————3. Generalizing pairs and cards —————————————————————————————————————————————-}
-- | 运算抽象：有两个列表，分别从各自中取出一个元素、进行结合
-- 2个列表
allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs _ []        _  = []
allCombs _ _         [] = []  -- 这个模式有没有必要？
allCombs f (xa : xs) ys = map (f xa) ys ++ allCombs f xs ys


allPairs' = allCombs (,)
allCards' = allCombs Card

-- ◯◯◯◯◯◯◯◯◯ test ◯◯◯◯◯◯◯◯◯◯ --
v01 = show (allCards cardRanks cardSuits)
v02 = show (allCards' cardRanks cardSuits)
-- ◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯
{-————————————————————————————4. Combinations of three things ———————————————————————————————————————————-}
-- 已经利用了两个 List 的mix 函数
allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f as bs = allCombs id (allCombs f as bs)
    -- where mixByCall f c = f c

-- | 乃至扩展到 4个列表...
allCombs4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
allCombs4 f as bs cs = allCombs id (allCombs3 f as bs cs)

-- ◯◯◯◯◯◯◯◯◯ test ◯◯◯◯◯◯◯◯◯◯ --
v04 = allCombs3 (,,) [1, 2] ['a', 'b'] [True, False]
v05 = allCombs4 (,,,) [1, 2] ['a', 'b'] [True, False] [100, 999]
-- ◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯
{-——————————————————————————————5.Combinations of more things —————————————————————————————————————————-}
combStep :: [a -> b] -> [a] -> [b]  -- 就是 Applicative 嘛！
combStep []       _  = []
combStep _        [] = []
combStep (f : fs) xs = map f xs ++ combStep fs xs

-- | 改写升级版
allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs' f xs ys = map f xs `combStep` ys
allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' f xs ys zs = map f xs `combStep` ys `combStep` zs
allCombs4' :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
allCombs4' f xs ys zs os = map f xs `combStep` ys `combStep` zs `combStep` os
