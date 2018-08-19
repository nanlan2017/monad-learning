module Do.Does2 where

{-
do { x1 <- action1
   ; x2 <- action2
   ; mk_action3 x1 x2 }

意思是：mk_action 中要同时使用 action1, action2的结果值   
-}

f0 = do
    line1 <- getLine
    line2 <- getLine
    putStrLn $ line1 ++ line2

f0' = getLine >>= \line1 -> do  -- 后续作为一个大 Actions！ 使得line1能为后方所用，而不仅是只在下一个小的action 可见
    line2 <- getLine
    putStrLn $ line1 ++ line2

f0'' = getLine >>= \line1 ->
    (getLine >>= \line2 -> do
        putStrLn $ line1 ++ line2
    )

f0''' =
    getLine >>= \line1 -> (getLine >>= \line2 -> (putStrLn $ line1 ++ line2))
{-
                把 m a  理解成 monadic value <---- 这是侧重结果
                        理解成 有副作用的 action <--- 强调这是一个动作。并且a 部分就是这个动作的直接结果。
▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ 既然 do-block 就是m a, 显然do 是动词，所以把 m a 想成action 更合适！！！



▇▇▇ bind/ do 中的<- 的本质就是： 
    让第一个action (getLine)的结果能    以line1这个名称     被后续的action (actions/do-block整个就是一个大action) 所使用
    即： action1 >>= \line1 -> ACTIONS

                        
而 m a    >>=    (a -> m b)   >>=     (b -> m c)
▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ 把 a -> m b 理解为：  m b 是一个action， 不是说： “有了a，才得到一个action”，
                                                      而是说： “m b 这个action 在 ‘动作’过程中会用到一个 a 类型‘纯值’ （这个动作会计算得到b、并有副作用）”
                                                                ————>  “ 提供了a 以后，这个 m b 的action 才是可以执行的 ”
-}


