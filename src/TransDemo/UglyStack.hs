module TransDemo.UglyStack where


import           System.Directory
import           System.FilePath
import           Control.Monad.Reader
import           Control.Monad.State
-- ***********************************************************

-- 遍历目录： 最深深度设置 Reader
data AppConfig = AppConfig
  { cfgMaxDepth :: Int
  } deriving (Show)

-- 遍历目录：达到最深深度的次数 State
data AppState = AppState
  { stDeepestReached :: Int
  } deriving (Show)

type App = ReaderT AppConfig (StateT AppState IO)

runApp :: App a -> Int -> IO (a, AppState)
runApp k maxDepth =
    let config = AppConfig maxDepth
        state  = AppState 0
    in  runStateT (runReaderT k config) state

-- ***********************************************************

constrainedCount :: Int -> FilePath -> App [(FilePath, Int)]
constrainedCount curDepth path = do
    contents <- liftIO . listDirectory $ path
    cfg      <- ask
    rest     <- forM contents $ \name -> do
        let newPath = path </> name
        isDir <- liftIO $ doesDirectoryExist newPath
        if isDir && curDepth < cfgMaxDepth cfg
            then do
                let newDepth = curDepth + 1
                st <- get
                when (stDeepestReached st < newDepth)
                    $ put st { stDeepestReached = newDepth }
                constrainedCount newDepth newPath
            else return []
    return $ (path, length contents) : concat rest
