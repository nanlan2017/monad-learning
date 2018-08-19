module Do.Does1 where

f0 = putStr "Hello" >> putStr " " >> putStr "world!" >> putStr "\n"

-- | 一个action 就是一个只依赖于 Context 的值
-- IO String
-- State s Int  ==  \s-> (Int,s)
-- Reader r Int ==  r-> Int
-- Writer w Int ==  (Int,w)
f0' = do  -- 每一行都是一个 action   (m 为IO )
    putStr "Hello"
    putStr " "
    putStr "world!"
    putStr "\n"
{-❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖
在do block 中，
    每一行都是一个 m action，其类型为 L1:: m a , L2:: m b ...
    而do 的方式是把这些action 用 >> 连接， 即后面的计算并不需要使用上一行的结果（>>=)
                要用的话，就在那一行预先用 <- 取出来a/b        



想一想  putStrLn  "aha"  这是一个 IO ()  的action
而它的a 部分为void， 意思是执行这个action 的结果是 void
▇▇▇▇▇▇▇▇▇▇ m a  称为monadic value， 一个带有副作用的计算 会产生一个monadic value             


*ghci> Nothing >> Just 3    // Nothing >>= \_ -> Just 3
Nothing
*ghci> Just 2 >> Just 3
Just 3

instance  Monad Maybe  where
    --  Maybe a   >>=  (a->Maybe b)  ::  Maybe b
    (Just x) >>= k      = k x
    Nothing  >>= _      = Nothing

❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖-}
