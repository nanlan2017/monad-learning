module Do.Does3 where

{-
do { Just x1 <- action1     --   <- :其实是对a 值进行模式匹配
   ; x2      <- action2
   ; mk_action3 x1 x2 }


action1 >>= f                 -- f 是一个接受一个 Maybe 值（模式类型）， 此模式值作为参数，进入一个do-block 的大 Action (代表后续的actions 整体)
where f (Just x1) = do { x2 <- action2
                       ; mk_action3 x1 x2 }
      f _         = fail     <——————————  ▇▇▇▇▇▇▇ 这就是为什么实现 Monad 时可以自定义其 fail,

———————————————————————————————————————— 分析与理解 ——————————————————————————————————————————
x1 <- action1 时， 可以通过这种形式    
                        ①
                        action1 >>= \x1 -> do {...剩下来的actions...}      
                        ②
                        其实相当于     action1  >>= f   (给这个lambda函数一个名字)
                                         where f x1 = do {...剩下来的actions...}

(Just x1) <- action1
而当 这地方是一个 pattern 时(比如 Just x1 /Nothing )  ，
                     ①
                     由于 lambda 表达式的参数部分只能匹配一种模式 ，比如：若为 \(Just x1)  ->  则此lambda无法匹配 Nothing)
                            ————>  那么，此lambda 写法也是正确的 ：它和原来的do 写法一样，也是匹配不到就会报错。
                     ②
                     所以，也能写成‘具名函数’f :
                            action1 >>= f 
                                where f (Just x1) = do {...剩下来的actions...}
                                      f  Nothing = fail "..." // 编译器提示
———————————————————————————————————————— ▇▇▇▇▇▇▇ Monad 的fail ——————————————————————————————————————————   
何时被调用：
    在do-block 中，当模式匹配失败时。                                   
 
What fail actually does depends on the monad instance.
（ fail 的实际行为取决于具体的 Monad 类型）

 Though it will often rethrow the pattern matching error, 
（ 虽然通常都是采用 Monad接口中的默认实现：抛出一个“模式匹配失败”异常）

 monads that incorporate some sort of error handling may deal with the failure in their own specific ways. 
（但那些带有错误处理语义的 Monad 通常会自定义fail）

 For instance, Maybe has fail _ = Nothing; analogously, for the list monad fail _ = []
（ 例如，Maybe 的fail _ = Nothing,  List 的fail _ = [] ）

The fail method is an artifact of do notation. 
（Monad 的fail 方法是do notation 自动展开成一般bind形式后(解语法糖后) 用到的道具）

Rather than calling fail directly,
（在代码中我们一般不会直接显式调用fail， 还是依赖于该 Monad 自动处理 链式调用时 出现的模式匹配失败）
 you should rely on automatic handling of pattern match failures whenever you are sure that fail will do something sensible for the monad you are using.    
-}
