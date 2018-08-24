{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}
module Impl.ContT where

import           Challenges.MCPrelude
import           Challenges.UsingMonad
import           Impl.Typeclasses
-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩
add :: Int -> Int -> Int
add x y = x + y

square :: Int -> Int
square x = x * x

pythagoras :: Int -> Int -> Int
pythagoras x y = add (square x) (square y)

-- | 
-- 定义使用了CPS的加法和平方函数,
-- （add_cps 和 square_cps 实际上并不是严格的CPS函数
-- 他们只是类型正确罢了）

add_cps :: Int -> Int -> ((Int -> r) -> r)
add_cps x y = \k -> k (add x y)
-- add_cps  x y  = \fNext -> fNext (add x y)
-- add_cps  x y  fNext  =  fNext $ add x y

square_cps :: Int -> ((Int -> r) -> r)
square_cps x = \k -> k (square x)
-- square_cps x fNext = fNext $ square x

pythagoras_cps :: Int -> Int -> ((Int -> r) -> r)
pythagoras_cps x y = \k ->  --                   3 $ (*2)   ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ 通用经验：用Lambda 来 对应类型签名中的函数。
                           square_cps x $ \x_squared ->  -- 这里 square_cps x 就是 (a->r)->r 啊  , 而 (\x_squared-> ...) 就是 【a-> ...】类型
    square_cps y $ \y_squared ->  --       -- 而 square_cps y  就是下一个 (b->r)->r 
                                 add_cps x_squared y_squared $ k

{-                                
-- | pythagoras x y fNext = fNext $        -- 只要确保 【fNext 所要求的参数】 与 【此函数的计算结果类型】 一致就可以了      
                                            --   step1 p1a p1b  $\r1->     -- 这里step1是 CPS 风格的，才能这样串联
                                                    (后续会使用r1的steps)    就可以了啊！  
                                            ---  所以：最外层的 \k-> (.....) $ k  请无视，这是为了让最后一个计算后仍能把结果往 其他函数传                  
-- pythagoras x y fNext = 
        square x $ \xr->
        square y $ \yr ->
        add xr yr $ fNext
-}
-- v0 = 3 $ (+2)
v0 = map ($ 2) [(2 *), (4 *), (8 *)]
-- v1 = 2 $ (2 *)
-- v2 = \k -> 2 $ k
-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩
-- ▇▇▇▇▇▇▇▇▇▇▇▇▇ 将一个函数 变换为 CPS 形式 ---高阶函数如何变换：
-- ▇▇▇▇▇▇▇▇▇▇▇▇▇        参数中的 f:: a->b 也要变为 f_cps :: a -> ((b -> r) -> r)
-- ▇▇▇▇▇▇▇▇▇▇▇▇▇                f a :: b   变为   f_cps a :: (b -> r) -> r
--                 其中  在f 的最后添加了一个参数，其参数类型b与原函数 的结果类型b 一致
--                      (b -> r) -> r 为 suspended computation                  (Context 中已有一个b 型值，只要给我一个要运算它的函数、我就可以运算)
--                      b->r  成为 Continuation : 指的是
-- | 这是一个高阶函数（high-order): 即 其将函数作为参数  （类型签名中：在前方有括号的都是高阶函数）
thrice :: (a -> a) -> a -> a
thrice f x = f (f (f x))
-- ◯◯◯◯◯◯◯◯◯◯◯◯ test ◯◯◯◯◯◯◯◯◯◯◯◯◯ --
-- *ghci> thrice tail "foobar"
-- "bar"
-- ◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯

thrice_cps :: (a -> ((a -> r) -> r)) -> a -> ((a -> r) -> r)
thrice_cps f_cps x fnext =
    f_cps x $ \fx ->   --
                     f_cps fx $ \ffx ->    --
                                        f_cps ffx $ fnext   --

-- | 上面我们连续调用了3次 f_cps :: a -> ((a -> r) -> r)      , 看能不能chain 起来                                     
chainCPS :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
--  ▇▇▇▇▇▇▇▇▇▇▇▇▇其实非 CPS 版本就是:
--                  a        ->     (a->b)               ->   b
--               挂起的a计算   ->  （a-> 挂起的b计算)        ->  挂起的b计算
--       （假设Context 已存在a值） ->................->  （能将 Context 中的值计算成b 值）

-- 怎么串两个函数：  1. 把一个函数当做另一个函数的参数   2. 提供一个初始值，完成链式计算
chainCPS m f fNext = m $ \x -> f x $ fNext   -- | ▇▇▇▇▇▇▇注意观察各元素类型！！！


-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩


--  (a->r) -> r   会对应什么样的运算
newtype Cont r a = Cont {runCont :: (a->r)->r }

instance Monad (Cont r) where
    -- return :: a -> Cont r a
    -- return va  
    -- return va = Cont $ let f = \va -> r in \f -> r


newtype ContT r m a = ContT {runContT :: (a->m r)-> m r}
