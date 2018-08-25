{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}
module ContDemo.ContSimple where

import           Challenges.MCPrelude
import           Challenges.UsingMonad
import           Impl.ContT                     ( Cont
                                                , chainCPS
                                                )
import           Prelude                        ( print
                                                , IO
                                                )
-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩
add :: Int -> Int -> Int
add x y = x + y

square :: Int -> Int
square x = x * x

pythagoras :: Int -> Int -> Int
pythagoras x y = add (square x) (square y)
{-▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃-}
-- ◯◯◯◯◯◯◯◯◯◯◯◯ test ◯◯◯◯◯◯◯◯◯◯◯◯◯ --

v0 :: IO ()
v0 = add_cps 2 3 square_cps print

v1 :: IO ()
v1 = add_cps 2 3 (\xr -> add_cps 5 6 (\yr -> add_cps xr yr print))

v10 = add_cps 2 3 `chainCPS` square_cps

v11 :: Int -> (Int -> r) -> r
v11 = add_cps 2
{-▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃-}
{-▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃-}
{-▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃-}
f_cps :: Int -> String -> (String -> r) -> r
f_cps i str k = k $ show i ++ str

ff_cps :: String -> Int -> (String -> r) -> r
ff_cps str i k = k $ show i ++ str

-- |▇▇▇▇▇▇▇▇▇▇▇▇▇▇ 重大发现： 不用lambda 的话，前面的值会自动成为后面的参数！！！
-- v12 = ($ print) $ add_cps 2 3 $ f_cps "abc"  -- f_cps "abc"不是一个正确的调用-- 把 $ 后面看成一个整体 ： f_cps 的第一个参数需为 Int

-- ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
--  非CPS 风格的函数   Int -> String            
--  CPS 风格的函数     Int -> (String->r)->r
--      ▇★▇▇★▇▇★▇▇★▇ 两者的业务功能都是 ： 将 Int 计算得一个 String 值。 不同的是 这个 String 值是“怎么放的”
--      两者都可以作为continuation:      非 CPS 的作为continuation 时，会得到一个 value
--                                     CPS 作为continuation 时，会得到一个 suspended value |  一个

v12a :: (String -> r) -> r  -- ▇▇▇▇▇▇▇  suspended Int ---> to suspended String   (▇▇ CPS consumer)
                            --                       continuation 必然是 ::  Int -> (String->r)->r   （一个cps 风格的计算: 缺一个 Int 参数，运算出一个 String)
-- | (Int->r)->r   $   Int->(String->r)->r    ::   (String->r)->r     
-- 抽象为  (a->r)->r  $  a->(b->r)->r     ::  (b->r)->r          ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
--                                                              ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
--                                                              ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇        
v12a = add_cps 2 3 $ ff_cps "abc"
                -- 第一个suspended 的其实是一个 Int 值，后面的continuation肯定是 Int-> .... （这里是 ff_cps "abc" ::  Int->(String -> r) -> r )
                --                                                                      其 吸收a 值，并返回一个suspended computation 
                -- 
v12a1 :: IO ()  -- suspended Int + print （▇▇ Non-cps consumer)
v12a1 = ($ print) $ add_cps 2 3 $ ff_cps "abc"

v12a2 :: Int    -- suspended Int + (add 3)（▇▇ Non-cps consumer)
v12a2 = add_cps 2 3 $ add 3
{-▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃-}
{-▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃-}
{-▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃-}
-- |▇▇▇▇▇▇▇▇▇▇▇▇▇▇ 你想将xr,yr 以有名字的方式往后传，就必须使用 Lambda
v2 :: (Int -> r) -> r
v2 = add_cps 2 3 `chainCPS` \xr -> add_cps 5 6 `chainCPS` \yr -> add_cps xr yr
v2equal :: (Integer -> b) -> b
v2equal = ($ (2 + 3) + (5 + 6))   --- 一个suspended = ($ ..value...)
-- {- ^ ^ ^ ^^ ^  ^ ^ ^^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^^ ^ ^ ^ ^ ^ ^^ ^ ^ ^  -}

v2'' = v2 print
v2equal'' = v2equal print

v2' :: IO ()
v2' = ($ print) $ add_cps 2 3 `chainCPS` \xr ->
        add_cps 5 6 `chainCPS` \yr -> add_cps xr yr -- chainCPS 只会算出 suspended computation

v3 = add_cps 2 3 (\xr -> add_cps 5 6 (\yr -> add_cps xr yr))
v3' = add_cps 2 3 (add_cps 5 6 . add_cps)
-- ◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯


add_cps :: Int -> Int -> ((Int -> r) -> r)
add_cps x y k = k (add x y)
-- add_cps  x y  = \fNext -> fNext (add x y)
-- add_cps  x y  fNext  =  fNext $ add x y

square_cps :: Int -> ((Int -> r) -> r)
square_cps x k = k (square x)
-- square_cps x fNext = fNext $ square x


{-
add_cps 2 3  :: (Int->r)->r
        【▇▇▇▇▇▇就是一个已经‘部分求值’的高阶函数！ 其部分应用已求出部分值】（cps 风格的函数都是 高阶函数）
   是一个 suspended computation  (相当于“已经算出来以备作参数一个值”，但还缺一个使用此值的函数)

   

-}
pythagoras_cps :: Int -> Int -> ((Int -> r) -> r)
pythagoras_cps x y k = square_cps x $ \x_squared ->
        square_cps y $ \y_squared -> add_cps x_squared y_squared k

        --      所有的continuation 都是 T ->r (即后面的每一个lambda 都是 _::T ->r)
        --      这里 square_cps x 就是 (a->r)->r 啊  , 而 (\x_squared-> ...) 就是 【a-> ...】类型
        --      而 square_cps y  就是下一个 (b->r)->r 
        --      最后一行为 suspended + continuation = r        
{-▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃-}

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
{-▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃-}
add_cont :: Int -> Int -> Cont r Int
add_cont x y = return (add x y)

square_cont :: Int -> Cont r Int
square_cont x = return (square x)

pythagoras_cont :: Int -> Int -> Cont r Int
pythagoras_cont x y = do
        x_squared <- square_cont x
        y_squared <- square_cont y
        add_cont x_squared y_squared

{-
  square x >>= \xr -> square y >>= \yr -> add xr yr
-}
