module TransDemo.WikiTransformer where
import           Control.Monad
import           Control.Monad.Trans.Maybe      ( MaybeT )
import           Control.Monad.Trans.Class      ( MonadTrans
                                                , lift
                                                )
import           Data.Functor.Identity          ( Identity )
{- ▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃ -}
getPassphrase :: IO (Maybe String)
getPassphrase = do
    s <- getLine
    if isValid s then return $ Just s else return Nothing

-- 我们可以要求密码满足任何我们想要的条件
isValid :: String -> Bool
isValid s = length s >= 8
            -- && any isAlpha s
            -- && any isNumber s
            -- && any isPunctuation s

askPassphrase :: IO ()
askPassphrase = do
    putStrLn "输入新密码:"
    maybe_value <- getPassphrase
    case maybe_value of   -- 此处要进行模式匹配
        (Just psd) -> putStrLn "储存中..." -- 假装存在数据库操作
        Nothing    -> putStrLn "密码无效"
{--- *****************************************************************-}
{-
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a)}


____▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒____ 运算时的关键： 就是把这个嵌套的盒子 ：外面什么盒子|里面那种盒子 、如何取值、装值 给想清楚，很简单！
instance (Monad m)=> Monad (MaybeT m) where     // MaybeT m 仍然是一个 Monad
    return :: a -> MaybeT m a
    return x = MaybeT $ m.return (Just x),  即 return = MaybeT. return .Just
    -- 首先, 我们先用 Just 将值装入最内层的 Maybe 中, 然后用 return 将前述 Maybe 装入 m monad里, 最后再用 MaybeT 将整个值包装起来.



    bind :: MaybeT m a -> (a-> MaybeT m b) -> MaybeT m b         // 是m 盒子里，装着一个 Maybe盒子，里面有个a 值
    m >>= k = MaybeT $ do
                    maybe_value <- runMaybeT m   // ①Unwrap:暴露这个嵌套的双层盒子值  ② <- ：取出 m (Maybe a)中的 Maybe a
                    case maybe_value of           
                        Nothing -> return Nothing      //___▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒__ 这个do 对应的是 m 这个外层的盒子，所以 <- | return 也是只能用于 m 的？？   
                        Just va -> runMaybeT $ k va      
                        
                        
    bind :: MaybeT m a -> (a-> MaybeT m b) -> MaybeT m b         // 是m 盒子里，装着一个 Maybe盒子，里面有个a 值
    m >>= k = MaybeT $ do
                    maybe_value <- runMaybeT m   // ①Unwrap:暴露这个嵌套的双层盒子值  ② <- ：取出 m (Maybe a)中的 Maybe a
                    res = do                      // ___▒▒▒▒▒▒▒▒  可以吗？ 在 m 的do 内嵌套一个 Maybe 的do
                        va <- maybe_value
                        runMaybeT $ k va        
-}
getValidPassphrase :: MaybeT IO String
getValidPassphrase = do -- 该do 块对应的monad 为 MaybeT IO  (其实是 IO (Maybe a)))
    s <- lift getLine -- 我们使用了 lift 函数以在 MaybeT IO monad 中使用 getLine 和 putStrLn  
    guard (isValid s) -- MonadPlus 类型类使我们能够使用 guard.
                        -- guard 可以为我们检查代码的合法性. 在密码不合法时其将返回 mzero (即 IO Nothing)
    return s

askPassphrase' :: MaybeT IO ()
askPassphrase' = do
    lift $ putStrLn "输入新密码:"
    psd <- getValidPassphrase
    lift $ putStrLn "储存中..."

-- ***************************************8
-- | 实现 IdentityT
newtype IdentityT m a = IdentityT { runIdentityT :: m a}

instance (Monad m)=> Functor (IdentityT m) where
instance (Monad m)=> Applicative (IdentityT m) where

instance (Monad m)=> Monad (IdentityT m) where
    -- return :: a -> IdentityT m a  // m a
    return x = IdentityT $ return x
    -- bind :: IdentityT m a -> (a-> IdentityT m b) -> IdentityT m b
    m >>= k = do
            va <- m
            k va

instance MonadTrans IdentityT where
    -- lift :: m a -> Identity m a
    lift = IdentityT


