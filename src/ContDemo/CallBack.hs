-- http://www.haskellforall.com/2012/12/the-continuation-monad.html
module ContDemo.CallBack where
import           Control.Monad.Cont             ( ContT(..) )
import           Control.Monad.Trans.Cont       ( Cont
                                                , cont
                                                )
import           Control.Monad                  ( forever )
-- ******************************************************************************************************
-- | 这属于 callback 回调
-- Cont r  a    ： a 是suspended value 的类型， 它限制的是 continuation函数的 “参数“的类型
--                 r 是 对应的作为continuation函数的 ”结果“类型
--                 所以，最直接的 Continuation 就是 a->r 这样的函数，那么能得到r 值
--                      还有一种 就是 a->(b->r)->r 这样的，那么只会得到 suspended value (b)
onInput :: (String -> IO ()) -> IO ()
        -- i.e. Cont (IO ()) String
onInput f = forever $ do
        str <- getLine
        f str
-- *******************************"Complete me Later"****************************************************
type Target = String
target = "Lily"
swingAxeBack = print
sayUhOh = putStrLn "uh-oh !"
isTargetValid :: Target -> IO Bool
isTargetValid _ = return True


-- | 待填充的业务逻辑
unitAttack :: Target -> IO ()
unitAttack target = do
        swingAxeBack 60
        valid <- isTargetValid target
        if valid
                then undefined -- then ??? target  -- 待填充的业务逻辑  Target-> IO ()
                else sayUhOh
{-
接下来的 函数其实是 CPS 风格的，即结果的是一个 suspended value
那么为什么 不能直接写成 非 CPS 风格的呢？

unitA :: Target -> IO Target
unitA target = do
        swingAxeBack 60
        valid <- isTargetValid target
        if valid then todo target else sayUhOh               
-}
unitAttack' :: Target -> Cont (IO ()) Target -- 得到一个suspended Target 值。。
unitAttack' target = cont $ \todo -> do   -- Rule 在函数实现体中出现的 大写字母开头的标识符 都是 ”值构造子“
        swingAxeBack 60
        valid <- isTargetValid target
        if valid then todo target else sayUhOh

unitAttack'' :: Target -> ContT () IO Target
unitAttack'' target = ContT $ \todo -> do
        swingAxeBack 60
        valid <- isTargetValid target
        if valid then todo target else sayUhOh


-- ◯◯◯◯◯◯◯◯◯◯◯◯ test ◯◯◯◯◯◯◯◯◯◯◯◯◯ --
damageTarget :: Target -> IO ()
damageTarget _ = putStrLn "damaged!"

r0 :: IO ()
r0 = runContT (unitAttack'' target) damageTarget
-- ◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯
-- ******************************************************************************************************
-- | 有2个，乃至多个todo 的原始写法
unitAttack2 :: Target -> (Int -> IO ()) -> (Target -> IO ()) -> IO ()
unitAttack2 target todo1 todo2 = do
        todo1 60
        valid <- isTargetValid target
        if valid then todo2 target else sayUhOh


-- --------------  改进 -------------------
-- | contin ation 的函数要能处理 多种类型的值，而其结果类型已确定为 IO（）
-- 方式1：多态函数（利用typeclass)- 子类型多态
-- 方式2：参数化多态  + 用值构造子的模式来区分不同类型的参数       
data Hole = Swing Int | Attack Target

unitAttack2' :: Target -> ContT () IO Hole
unitAttack2' target = ContT $ \todo -> do
        todo (Swing 60)
        valid <- isTargetValid target
        if valid then todo (Attack target) else sayUhOh

-- ◯◯◯◯◯◯◯◯◯◯◯◯ test ◯◯◯◯◯◯◯◯◯◯◯◯◯ --
swingBack :: Int -> IO ()
swingBack _ = putStrLn "swing!"

continue :: Hole -> IO ()
continue (Swing  n) = swingBack n
continue (Attack t) = damageTarget t

r1 :: IO ()
r1 = runContT (unitAttack2' target) continue
-- ◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯◯
