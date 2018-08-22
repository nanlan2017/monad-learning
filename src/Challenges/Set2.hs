{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}
module Challenges.Set2 where

import           Challenges.MCPrelude
import           Challenges.UsingMonad
-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩
{-———————————————————————————————1. The Maybe type —————————————————————————————————————-}

{-————————————————————————2.Build a library of things that can fail —————————————————————————————————————-}
{-   换一种思路看 m a -> (a->m b)

▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
                    a -> m b    实际上还是 a->b ，但这个结果过程 是带副作用的，  m b =  b + sideEffect 
▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ 
▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ 
▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇                     

pure 函数的串联 就是简单的     f :: a-> b    g :: b-> c    h:: c->d
             pure 计算的承接（对结果持续操作）：      h . g. f           类型为 a -> d
▇▇ ▇▇ ▇▇ ▇▇ ▇▇▇▇ ▇▇▇▇ 而 计算过程 带副作用的 函数 的形式是 ：  f :: a -> m b        g :: b-> m c   h:: c -> m d
             带副作用的 计算的承接  ：               h =<< g  =<< f     类型为  a -> m d 

所以：  标准的串联 va -> f -> g -> h  的形式是   f va >>= g >>= h
                            对比 Pure 版本：   h . g . f va



  ▇▇ ▇▇ ▇▇ ▇▇ ▇▇▇▇ ▇▇▇▇ 如果是 一条龙 全部 一步一步基于运算的话， 就一串 >>= 就可以了   （这时候用do 写 反而很麻烦，

   而 do 写法，  默认是 每一行的action 的 结果是 后方不用的 。（除非自己 <- 取出来 ）
   

   如果 有的action 的结果要后方使用、有的不要，则 可以do + 某些action 的结果用<- 取出
     ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ 实际上就是  >>=  和 >> 的混合（>>就是>>=来实现的） (以及通过 Lambda 绑定结果变量)

   如果 action1的结果不是 由action2使用的话， （而是由后面的action5, 5)
   那么   
     action1 >>= \r1 -> action2 >>= action3 ...
     
    翻译成do


-}
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

-- \ \ \ \\ \\ \ \\ \ \ \\ \\\ \ \\ \ \ \ \ \ \\ \\ \ \\ \ \ \\ \\\ \ \\ \ \ \ \ \ \\ \\ \ \\ \ \ \\ \\\ \ \\ \ \
{-
lift :: (a -> b) -> Maybe a -> Maybe b
lift _ Nothing  = Nothing
lift f (Just x) = Just $ f x

lift2 :: (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c  --写错了！！
lift2 f Nothing  _        = Nothing
lift2 f _        Nothing  = Nothing
lift2 f (Just x) (Just y) = f x y

queryGreek' :: GreekData -> String -> Maybe Double
queryGreek' []               _   = Nothing
queryGreek' ((k, list) : xs) key = if k == key
    then lift2 divMay
               (lift fromInteger $ tailMay list >=> maxMay)
               (lift fromInteger $ headMay list)
    else queryGreek' xs key

-- ◯◯◯◯◯◯◯◯◯ test ◯◯◯◯◯◯◯◯◯◯ --
v03 = map (queryGreek' greekDataA) ["alpha", "belta", "gamma", "delta", "zeta"]
v04 = map (queryGreek' greekDataB) ["rho", "phi", "chi", "psi", "omega"]
r1 = show v01 == show v03
r2 = show v02 == show v04
-- ◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯
-}
-- \ \ \ \\ \\ \ \\ \ \ \\ \\\ \ \\ \ \ \ \ \ \\ \\ \ \\ \ \ \\ \\\ \ \\ \ \ \ \ \ \\ \\ \ \\ \ \ \\ \\\ \ \\ \ \ 
{-
queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 []               _   = Nothing
queryGreek2 ((k, list) : xs) key = if k == key
    then   -- ▇▇▇▇▇ 这是错误写法！！ bind 是可以做到 两个不”承接“的action 的连接的 ！！
        (\(Just maxtail) (Just hea) ->
                divMay (fromInteger maxtail) (fromInteger hea)
            )
            (tailMay list >=> maxMay)
            (headMay list)
    else queryGreek xs key
-}
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

{-
do      (do 里面写承接的倒是很麻烦！！))
    tail    <- tailMay list
    maxtail <- maxMay tail     -- 本来前两行只要写   maxtail <- maxMay =<< tailMay list
    hea     <- headMay list
    divMay (fromInteger maxtail) (fromInteger hea)


▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ 
【★☆★☆★☆★☆★☆★☆  结论：  只要是 基于同一个 m 的 Actions，都可以 写到 一块 ！】（只要它们确实发生的 过程是 那样的。）
▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ ▇▇▇▇▇▇▇▇▇▇
▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ ▇▇▇▇▇
    结论： 前后承接的 多个actions 间 使用 bind >>=  将其 捆绑成一个大 Action
      而若  f  g  h 是 承接的
      但    j  k  l  也是承接、但不和上述 fgh 承接，
      那么现在要 启动 两个大 Action，【！！不要叫”串联“，先后顺序完全看你的业务需求】
      ___ 这时候不是用  >> then  ( >> 的语义是 刚才这块大的action 的结果不要了 )
      fgh >>= \r1 ->  jkl >>= \r2 -> {后续的Actions,可见r1,r2}。。。
      fgh    >>       jkl >>= \r2 ->    则 fgh 的结果就不使用了

▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ ▇▇▇▇▇▇
▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ ▇▇▇▇▇▇▇▇▇▇
【★☆★☆★☆★☆★☆★☆所以：什么时候用do :  do 默认就是 存在多个非承接的 Action 块！ -->  如果只有 一段 承接的几个actions，那用不着 do! 】
[所谓承接：  Action b 是在 Action a 的 产生结果 a 上继续运算， 则说 这两个 Action 是承接的。]
▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇            
-}

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
ylink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
ylink _ _        Nothing  = Nothing
ylink _ Nothing  _        = Nothing
ylink f (Just x) (Just y) = mkMaybe $ f x y
-}
-- |||  使用 action 的 bind 意义实现 lift ,当然bind+ lambda 可实现do 写法
--  ▇▇▇▇▇▇▇▇▇▇▇ 关键是理解它所描述的运算逻辑 。 其实都是非常简单的顺序几

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
