{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}
module Challenges.UsingMonad where

import           Challenges.MCPrelude
-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩
data Maybe a = Nothing | Just a

data Either e a = Left e | Right a

instance Show a=> Show (Maybe a) where
    show Nothing = "Nothing"
    show (Just x)= "Just "++ show x

newtype Gen a = WrapGen { runGen ::Seed -> (a,Seed)}
{-———————————————————————————————————————————————————————————————————————————————————————————————————-}

class Monad m where
    (>>=) :: m a -> (a-> m b)-> m b
    return :: a -> m a

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

instance Monad Gen where
    return x = WrapGen $ \seed -> (x,seed)

    genA >>= f = WrapGen $ \seed ->
        let (ra,seed') = runGen genA seed
        in runGen (f ra) seed'

instance Monad Maybe where
    return = Just

    (>>=) Nothing _ = Nothing
    (>>=) (Just x) f = f x


instance Monad [] where
    return x = [x]

    (>>=) = flip concatMap
{-———————————————————————————————————————————————————————————————————————————————————————————————————-}
-- ▇▇▇▇▇▇▇ Gen a 是一个动作action， 但这个动作并不是能直接执行得到结果值 （如同 IO），而是执行时需要有 预置状态s的动作！    
evalGen :: Gen a -> Seed -> a
evalGen gen seed = fst $ runGen gen seed

-- Set1 : repRandom
sequence :: (Monad m) => [m a] -> m [a]
sequence [] = return []
sequence (act1 : actions) =
    act1 >>= \r1 -> sequence actions >>= \rs -> return $ r1 : rs

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = m >>= (return . f)

fmap :: Monad m => (a -> b) -> m a -> m b
fmap = liftM

(<$>) :: Monad m => (a -> b) -> m a -> m b
(<$>) = fmap

-- Set1 : generalB
liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f actA actB = actA >>= \ra -> actB >>= \rb -> return $ f ra rb

-- Set3 : allCombs3
liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f actA actB actC =
    actA >>= \ra -> actB >>= \rb -> actC >>= \rc -> return $ f ra rb rc

-- Set3 : combStep
-- | ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ 这地方有点乱： action 的承接、先后关系？？ 对副作用状态的 连贯影响 ？？
-- | ▇▇▇▇▇▇▇ 可以用 monad 的 return /bind 实现 Applicative 的 ap 吗？
ap :: Monad m => m (a -> b) -> m a -> m b  -- m (a->b ) -> m a -> m b  
-- ▇▇▇▇▇▇▇▇▇▇▇▇▇▇ 理解该计算的语义：两个独立的动作，一个动作会得到一个函数、一个动作得到一个值 ---> “先后化- 让它们发生 ”这两个动作
-- ▇▇▇▇▇▇▇▇▇▇▇▇▇▇  注意： 这时候只是“安排它们发生”，结果部分先后！   action1 >>= \r1 -> action2 ...   还是  action2 >>= \r2 -> action1 ...
ap mf ma = mf >>= \f -> ma >>= \a -> return $ f a


(<*>) :: Monad m => m (a -> b) -> m a -> m b
(<*>) = ap

-- Set2 : combine
join :: Maybe (Maybe a) -> Maybe a
join m = m >>= id
