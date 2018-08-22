module Lib
    ( someFunc
    )
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩
-- | 我操，这个模块导出 Fuck 的话，那 Fuck 究竟指这个类型，还是这个 Fuck 构造函数？ 
-- 当然是类型！  否则是 Fuck (Fuck)


-- ▇▇▇▇▇编译选项具有传递性：由于 MCPrelude 中指定了编译选项“NoImplicitPrelude"，则依赖 MCPrelude 的文件中也不导入默认的 Prelude
-- 还有一种方式： import Prelude ()
