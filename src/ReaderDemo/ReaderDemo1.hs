{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module ReaderDemo.ReaderDemo1 where
import           Control.Monad                  ( replicateM_ )

import           Control.Monad.Reader           ( MonadReader
                                                , MonadIO
                                                , ask
                                                , liftIO
                                                , asks
                                                )
-- ***********************************************************
{- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
                        Module 说明
https://hackernoon.com/the-reader-monad-part-1-1e4d947983a8
https://medium.com/@jonathangfischoff/monad-reader-part-2-d812dda1d03e

先是原始版本的 loadAll
然后是使用 MonadReader 进行重构的版本 loadAll'
最后是 进一步通过 Has ,更进一步重构的版本 loadAll''


////////////////////////////////////////////////////////////////////////////////// -}
type Config = FilePath

load :: Config -> String -> IO String
load config x = readFile (config ++ x)

loadRevision :: Config -> Int -> IO String
loadRevision config x = load config ("history" ++ show x ++ ".txt")

loadAll :: Config -> Int -> String -> IO (String, String)
loadAll config x y = do
    a <- load config y
    b <- loadRevision config x
    return (a, b)
-- ***********************************************************
-- Config 作为Reader 的资源， 在 Reader 环境内 进行 IO：  
-- 下面的m 应该就是对应 IO
load' :: (MonadReader Config m, MonadIO m) => String -> m String  -- 这个类型签名：是对m（内部 Monad 进行了限制）
load' x = do
    config <- ask
    liftIO $ readFile (config ++ x)

loadRevision' :: (MonadReader Config m, MonadIO m) => Int -> m String
loadRevision' x = load' ("history" ++ show x ++ ".txt")

loadAll'
    :: (MonadReader Config m, MonadIO m) => Int -> String -> m (String, String)
loadAll' x y = do
    a <- load' y
    b <- loadRevision' x
    return (a, b)
-- ***********************************************************
pure :: a -> e -> a
-- pure x = \_ -> x 
pure = const -- 很明显：只能实现为：返回第一个参数
-- ***********************************************************
class HasFilePath a where
  hasFilePath :: a -> FilePath

-- | a 是可以从中 getFilePath 的类型
-- m 是基于a 类型的 Reader
-- m 是涉及 IO 的 Monad (MonadIO)
load'' :: (HasFilePath a, MonadReader a m, MonadIO m) => String -> m String
load'' file = do
    folder <- asks hasFilePath --单纯的ask 会直接导出环境r，但asks 会通过作用于r 的函数 从而导出结果
    liftIO $ readFile $ folder ++ file

loadRevision'' :: (HasFilePath a, MonadReader a m, MonadIO m) => Int -> m String
loadRevision'' x = load'' $ "history" ++ show x ++ ".txt"

loadAll''
    :: (HasFilePath a, MonadReader a m, MonadIO m)
    => Int
    -> String
    -> m (String, String)
loadAll'' x y = do
    a <- loadRevision'' x
    b <- load'' y
    return (a, b)

-- ***********************************************************
class HasPrintCount a where
  hasPrintCount :: a -> Int

awesomeMessage :: (HasPrintCount a, MonadReader a m, MonadIO m) => m ()
awesomeMessage = do
    repeatCount <- asks hasPrintCount
    replicateM_ repeatCount $ liftIO $ putStrLn "hello!"


veryAwesomeLoader
    :: (HasPrintCount a, HasFilePath a, MonadReader a m, MonadIO m)
    => String
    -> IO String
veryAwesomeLoader filePath = undefined  -- // TODO
