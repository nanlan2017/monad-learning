{-# LANGUAGE FlexibleContexts #-}
module ReaderDemo.MakeSentence where
-- **********************************************************    
import           Control.Monad.Reader
{- ▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃ -}

-- | 用ask 把 Reader 的 r域的值放到 a 域，然后在前面拼接step 这个字符串后，再return
myName :: MonadReader String m => String -> m String  -- m 与 String组成一个 Reader Monad
myName step = do
    name <- ask
    return (step ++ ", I am " ++ name)
    -- 是怎么deduce 出这个do 块对应的是 MonadReader String m 这个东西的？  关键就是ask  知道处于 MonadReader + 最后return String

myName_do :: MonadReader String m => String -> m String
myName_do step = reader $ \str -> str ++ ", I am " ++ step


-- | 最后得到的 Reader 这个monadic value 仍然内部wrap 的是一个 e -> result 的函数，提供一个初始环境方可得到结果。    
localExample :: Reader String (String, String, String)
localExample = do  -- 首先确定：这个do 中的是 哪个 Monad Action ? 
    --是 Reader String 这个 Monad (就是 ReaderT String Identity)
    a <- myName "First"  -- a = "First, I am Fred"
    -- | local f m   :  先用f 修改一下 Context，再把此 Context 串后一个 monadic computation     m1 >> m2 
    b <- local (++ "dy") (myName "Second")  -- b = "Second" ++ ", I am " ++ "Fred"++"dy"   
    c <- myName "Third" -- c= "Third"  ++ ", I am" ++ "Fred"
    return (a, b, c)


{- ▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃ -}

-- | 注意理解： 上面的 Reader String 这个 Monad 里，String 就对应的是 人名（Fred /  Freddy/  Fred)
-- | MyName ::  String -> m String,  就是可以链式的运算嘛！而且就 Reader来讲，其运算都不需要上一步结果 ，就是 e-> m a 这样的运算，运算出3个a 值
v00 = runReader localExample "Fred"
-- v00 = ("First, I am Fred","Second, I am Freddy","Third, I am Fred")  


--  ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
-- | 想一想：我的do 写法 
--  1. 就是获取了     <- 和 return     这两个能够 方便提取/ 放入monadic value<——> value 的能力嘛！
--  2. 第一行构建了一个 Context！  后面每一个action 都是基于此 Context 的连续运算！！  
--     就像  monadic value >> f  >> g 这里不传a，但会传递 Context!!
--  ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒

