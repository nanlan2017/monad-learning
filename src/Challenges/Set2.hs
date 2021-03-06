{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}
module Challenges.Set2 where

import           Challenges.MCPrelude
import           Challenges.UsingMonad
-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩
{-———————————————————————————————1. The Maybe type —————————————————————————————————————-}

{-————————————————————————2.Build a library of things that can fail —————————————————————————————————————-}

headMay :: [a] -> Maybe a
headMay []      = Nothing
headMay (x : _) = Just x

tailMay :: [a] -> Maybe [a]
tailMay []       = Nothing
tailMay (x : xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ []            = Nothing
lookupMay k ((x, y) : xs) = if k == x then Just y else lookupMay k xs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay x y = Just $ x / y

maxMay :: Ord a => [a] -> Maybe a
maxMay []       = Nothing
maxMay (x : xs) = case maxMay xs of
    Nothing -> Just x
    Just y  -> if x > y then Just x else Just y

minMay :: Ord a => [a] -> Maybe a
minMay []       = Nothing
minMay (x : xs) = case minMay xs of
    Nothing -> Just x
    Just y  -> if x > y then Just y else Just x
{-————————————————————————————3. Chains of failing computations ———————————————————————————————————————————-}
queryGreek :: GreekData -> String -> Maybe Double
queryGreek []               _   = Nothing
queryGreek ((k, list) : xs) key = if k == key
    then case tailMay list of
        Nothing  -> Nothing
        Just tai -> case maxMay tai of
            Nothing      -> Nothing
            Just tailmax -> case headMay list of
                Nothing -> Nothing
                Just hea ->
                    case fromInteger tailmax `divMay` fromInteger hea of
                        Nothing  -> Nothing
                        Just res -> Just res
    else queryGreek xs key

-- ◯◯◯◯◯◯◯◯◯ test ◯◯◯◯◯◯◯◯◯◯ --
v01 = map (queryGreek greekDataA) ["alpha", "belta", "gamma", "delta", "zeta"]
v02 = map (queryGreek greekDataB) ["rho", "phi", "chi", "psi", "omega"]
-- ◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯
{-——————————————————————————————4. Generalizing chains of failures ————————————————————————————————————————-}
-- chain
chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain _ Nothing  = Nothing
chain f (Just x) = f x

-- link
-- infixl 1 >=>
(>=>) :: Maybe a -> (a -> Maybe b) -> Maybe b
(>=>) Nothing   _ = Nothing
(>=>) (Just va) f = f va

-- 这个是一块”承接“的 action，结果为maxtail
-- 这是另一个action （与前面的不承接）， 结果为 hea
-- 不承接。可以取前面的actions 的结果。
queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 []               _   = Nothing
queryGreek2 ((k, list) : xs) key = if k == key
    then tailMay list >=> maxMay >=> \maxtail -> headMay list
        >=> \hea -> divMay (fromInteger maxtail) (fromInteger hea)
    else queryGreek2 xs key

queryGreek2' :: GreekData -> String -> Maybe Double
queryGreek2' []               _   = Nothing
queryGreek2' ((k, list) : xs) key = if k == key
    then
        let
            m = tailMay list >=> maxMay
            k = headMay list
        in
            m >=> \maxtail ->
                k >=> \hea -> divMay (fromInteger maxtail) (fromInteger hea)
    else queryGreek2' xs key



-- ◯◯◯◯◯◯◯◯◯ test ◯◯◯◯◯◯◯◯◯◯ --
v05 = map (queryGreek2 greekDataA) ["alpha", "belta", "gamma", "delta", "zeta"]
v06 = map (queryGreek2 greekDataB) ["rho", "phi", "chi", "psi", "omega"]
r3 = show v01 == show v05
r4 = show v02 == show v06

gd = [("fuck", [1])]
v07 = queryGreek gd "fuck"
-- v07' = queryGreek' gd "fuck"
v07'' = queryGreek2 gd "fuck"

-- 报错：Non-exhaustive patterns in lambda
t00 = (\(Just maxtail) (Just hea) -> divMay 3.0 1.0) Nothing Nothing
-- ◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯    
{-————————————————————————————————5 .Chaining variations ———————————————————————————————————————————-}
lookup :: [(String, Integer)] -> String -> Maybe Integer
lookup []            _   = Nothing
lookup ((k, v) : xs) key = if key == k then Just v else lookup xs key

-- You give this function a data structure with salary info and the names of two people and it returns the sum of their salaries
addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries alist k1 k2 = case lookup alist k1 of
    Nothing -> Nothing
    Just x  -> case lookup alist k2 of
        Nothing -> Nothing
        Just y  -> mkMaybe $ x + y

addSalaries' :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries' alist k1 k2 = ylink (+) (lookup alist k1) (lookup alist k2)

{-
-- | 抽象出的运算模式： 只有两个 Action 的结果r1,r2都成功返回了，才对他们的结果进行合并处理；否则整体结果为 Nothing
-}

ylink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
ylink f m k = m >=> \x -> k >=> \y -> mkMaybe $ f x y

-- ◯◯◯◯◯◯◯◯◯ test ◯◯◯◯◯◯◯◯◯◯ --
v09 = addSalaries' salaries "alice" "bob"
v10 = addSalaries salaries "alice" "wjh"
-- ◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯

mkMaybe :: a -> Maybe a
mkMaybe = Just
{-—————————————————————————————————6. Tail-product ——————————————————————————————————————————————————-}
-- 要求：使用 tailMay + product 实现 tailProd
tailProd :: Num a => [a] -> Maybe a
tailProd = transMaybe product . tailMay

tailSum :: Num a => [a] -> Maybe a
tailSum = transMaybe sum . tailMay

-- | 抽象出上述两个计算的共同模式： See if you can abstract out the commonality. 
-- To do this, write another function called transMaybe that is the generalized version of both of these    
-- ▇▇▇▇▇▇▇▇ 从多个运算过程的 类型签名中 抽取共通的函数类型、数据类型===> 得到 “运算模式” ！！！

-- f :: ([a] -> Maybe [a]) -> ([a] -> a) -> Maybe a
--            tailMay          prod或sum     
-- 进一步抽象：第一个函数可泛化成  a->Maybe b ： 从一个纯值得到一个 Maybe action 的值
--           第二步的逻辑是：如果上一步 Nothing,则整体 Noting，否则对上一步结果进行运算。且运算类型为 b -> Maybe c
transMaybe :: (a -> b) -> Maybe a -> Maybe b --就是fmap
transMaybe f Nothing  = Nothing
transMaybe f (Just x) = Just $ f x

tailMax x = tailMay x >=> maxMay -- maxMay :: [a]-> Maybe a
tailMin x = tailMay x >=> minMay

-- 要求使用transMaybe (lift) 实现tailMax
tailMax' :: (Ord a) => [a] -> Maybe (Maybe a)
tailMax' = transMaybe maxMay . tailMay --反正先tail,再 max

combine :: Maybe (Maybe a) -> Maybe a
combine Nothing         = Nothing
combine (Just Nothing ) = Nothing
combine (Just (Just v)) = Just v

tailMax'' :: (Ord a) => [a] -> Maybe a  -- 这个类型签名才是符合逻辑的
tailMax'' = combine . transMaybe maxMay . tailMay
