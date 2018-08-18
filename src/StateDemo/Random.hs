module StateDemo.Random where

import           System.Random
import           Control.Monad.State
-- *******************************************************************************

-- | rand 函数会读取和修改 IO monad中内置的全局随机数生成器StdGen
rand :: IO Int
rand = getStdRandom (randomR (0, maxBound))

-- | 错误示范： random gen 根据随机种子取一次随机数后，未更新gen，则下次再取时会取到相同值
twoBadRandoms :: RandomGen g => g -> (Int, Int)
twoBadRandoms gen = (fst $ random gen, fst $ random gen)

-- | 改正版
twoGoodRandoms :: RandomGen g => g -> ((Int, Int), g)
twoGoodRandoms gen =
    let (a, gen' ) = random gen
        (b, gen'') = random gen'
    in  ((a, b), gen'')
-- ****************************State Monad 改进版 ***********************************************
--         思路： State Gen 作为 Context,  RandomState 就是对应m
type RandomState a = State StdGen a  -- 注意是type 别名方式

{-
-- State 不是 Monad， 
-- 你自己定义的 State s 才对应为 Monad的m ， 
-- ▒▒▒▒▒▒▒▒▒▒▒▒ State s a 已经默认实现了诸多计算接口：  runState, returnState, bindState, get, put , ▒▒ evalState, execState
  ▒▒▒▒▒▒▒▒▒▒▒▒ MonadState ▒▒▒▒▒▒▒▒▒▒▒▒▒  State s 有没有默认已注册为 Monad （既然bindState是已实现的，那注册为 Monad 的bind 也可以吧？？
-- 若要让 State s 成为 Monad，要自己去实现 instance Monad XxState 中的 bind 行为 ???
-}

getRandom :: Random a => RandomState a -- State StdGen a ------ (包装的) StdGen -> (a,StdGen)
-- m a 就是一步的运算结果嘛！！
-- 这个运算的主调函数就是random :: StdGen -> (a, StdGen)
-- 
-- ▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 【 put gen' >> return val 】  是典型的构造一个m a 值的手法 ： 即分别填写 Context 值、a 值
getRandom =
    get >>= \gen -> let (val, gen') = random gen in put gen' >> return val  --- ▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 这 get + put ,妙啊！
    ------  ▒▒▒▒▒▒▒▒ ？？？ 这里更新的 StdGen ，是getStdGen 能获取到的那个 StdGen 吗 ？


getTwoRandoms :: Random a => RandomState (a, a)
getTwoRandoms = liftM2 (,) getRandom getRandom
-- ******************************** 测试  *****************************************************************
--  State 的 值构造器未 导出，所以是无法自己去构造 State 值的、也无法在实现中对其值进行模式匹配
checkResult :: (Show a) => RandomState a -> IO a
checkResult s = do
    gen <- getStdGen
    let (result, gen') = runState s gen
    putStrLn $ show result
    return result
{-
*Random> twoBadRandoms  <$>  getStdGen
(-332025931888261705,-332025931888261705)



*Random> twoGoodRandoms <$> getStdGen
((-332025931888261705,-1279278131693181570),2092493162 1780294415)

*Random> :t it
it :: ((Int, Int), StdGen)
-}
v00 :: Random a => IO a  -- 测试发现：多次运行，产生的随机数一直是同一个
v00 = do
    gen <- getStdGen
    return (fst $ runState getRandom gen)
-- ******************************** 测试  *****************************************************************
-- |  练习题： 用do 重写getRandom
getRandom' :: Random a => RandomState a
getRandom' = do
    gen <- get
    let (val, gen') = random gen
    put gen'
    return val


doGetRandom :: Random a => RandomState a
doGetRandom = do
    (val, gen') <- random `liftM` get
    state (\s -> (val, gen'))

-- *************************************************************************************************
runTwoRandoms :: IO (Int, Int)
runTwoRandoms = do
    oldState <- getStdGen
    let (result, newState) = runState getTwoRandoms oldState
    setStdGen newState  --IO 这个 Monad Context 里有很多资源，比如 StdGen
    return result
