module WriterDemo.CountEntries
    ( listDirectory
    , countEntriesTrad
    )
where

import           System.Directory               ( doesDirectoryExist
                                                , getDirectoryContents
                                                )
import           System.FilePath                ( (</>) )
import           Control.Monad                  ( forM
                                                , liftM
                                                )
{- ▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃ -}
-- | 在介绍monad变换器之前，先看看以下函数，其中使用的都是之前接触过的技术。
-- 这个函数递归地访问目录树，并返回一个列表，列表中包含树的每层的实体个数
listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
    where notDots p = p /= "." && p /= ".."

countEntriesTrad :: FilePath -> IO [(FilePath, Int)]
countEntriesTrad path = do
    contents <- listDirectory path
    -- 并同时对其下面的sub 子目录也各自进行统计
    rest     <- forM contents $ \name -> do
        let newName = path </> name
        isDir <- doesDirectoryExist newName
        if isDir then countEntriesTrad newName else return []
    return $ (path, length contents) : concat rest
