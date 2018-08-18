module StateDemo.SimpleState where
-- *******************************************************************************

type SimpleState s a = s -> (a, s)
-- | monad的类型构造器是 SimpleState s :: *->* ,而不是单独的 SimpleState
-- 此时： SimpleState s 就是对应的 Monad 中的 m
type StringState a = SimpleState String a  --  String-> (a,String)  -- 注意用的type 别名，而不是newtype 化名

{-
" SimpleState s " 是一个 Monad
StringState 是一个 Monad

return a :: a -> StringState a
-}
-- *******************************************************************************
returnSt :: a -> SimpleState s a
returnSt a s = (a, s)

bindSt :: SimpleState s a -> (a -> SimpleState s b) -> SimpleState s b
-- bindSt :: 【s ->(a,s)】 -> 【a-> s-> (b,s) 】 -> 【s->(b,s)】
bindSt m k = \s -> let (ra, s') = m s in k ra s'

-- | 变量名 易读版
bindSt' :: SimpleState s a -> (a -> SimpleState s b) -> SimpleState s b
bindSt' step1 makeStep2 initState =
    let (result1, state1) = step1 initState in makeStep2 result1 state1
-- *******************************************************************************
-- | 提供接口： 读取/修改 Context 状态
-- "In getSt, result happens to be state itself. "
getSt :: SimpleState s s
getSt = \state -> (state, state)  -- 让 m a 的 a 域等于 m 中的s
--  The getSt function simply takes the current state and returns it as the result, 
--  while putSt ignores the current state and replaces it with a new state.
putSt :: s -> SimpleState s ()
putSt state = \_ -> ((), state)   -- 让 m a 的 m 域等于你这个状态
