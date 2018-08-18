module WriterDemo.Logger where

import           Control.Monad
-- ********************************************************************************************
-- ********************************************************************************************
-- ********************************************************************************************
type Log = [String]
newtype Logger a = WrapLogger { unwrapLogger:: (a,Log)}  -- Logger 就是那个m 了
    deriving (Show)

instance Functor Logger where
    -- fmap
    fmap f (WrapLogger (va,logs)) = WrapLogger (f va ,logs)

instance Applicative Logger where
    -- pure :: a -> m a
    pure = return

    -- <*> :: m (a->b) -> m a -> m b
    WrapLogger (fab,llogs) <*> WrapLogger (va,rlogs) = WrapLogger (fab va, llogs++rlogs)


instance Monad Logger where
    -- return :: a -> Logger a
    return va = WrapLogger (va,[])
    -- bind :: Logger a -> (a-> Logger b) -> Logger b
    WrapLogger (r1,oldLogs) >>= f = WrapLogger (r2,newLogs)
            where
                (r2, newLog) = unwrapLogger $ f r1
                newLogs = oldLogs ++ newLog

    -- >>  :: m a -> m b -> m b
    {-
      -- ▒▒▒▒▒▒▒▒▒▒▒▒ 默认实现：   Va >> Vb    =    Va >>= \_ -> Vb     -- ★ 忽略左侧的a 值，但其 Context 域要照常进行更新运算。
      
      WrapLogger (ra,aLogs) >> WrapLogger (rb, bLogs) 
      = WrapLogger (ra,aLogs) >>= \_ -> WrapLogger (rb, bLogs) 
      = 计算：
                (r2, newLog) = (rb, bLogs)
                newLogs = aLogs ++ newLog
            故结果为 WrapLogger (r2, aLogs++bLogs)
    -}
    WrapLogger (ra,aLogs) >> WrapLogger (rb, bLogs) = WrapLogger (rb,aLogs++ bLogs)
-------------------------------------------------------------------------------
-- |  record log 是一个 无返回值的 Logger Action （如同putStrLn "..."是一个 IO Action）
-- () 就相当于 void
-- ▒▒▒▒▒▒▒▒▒▒▒▒ record 本质： 用来更新 Context 域（不影响a 值） ： record xxx >> m a 就能更新m 域，又不影响a
record :: String -> Logger ()
record log = WrapLogger ((), [log])

v00 = unwrapLogger $ record "hi mom!" >> return 3.1337
-- WrapLogger ((),["hi mom!"])   >>  WrapLogger (3.1337, [])  = WrapperLogger (3.1337,["hi mom!"])
-- (3.1337,["hi mom!"])
