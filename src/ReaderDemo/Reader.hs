module ReaderDemo.Reader
    ( Fuck(..)
    )
where


import           Control.Monad                  ( Monad )
-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩
-- | 我操，这个模块导出 Fuck 的话，那 Fuck 究竟指这个类型，还是这个 Fuck 构造函数？ 
-- 当然是类型！  否则是 Fuck (Fuck)
newtype Fuck = Fuck {getInt::Int}


