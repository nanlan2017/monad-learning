-- https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus
module Wiki.Alternative where
import           Control.Monad                  ( guard )
import           Control.Applicative            ( (<|>) )
{-❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

___________________________ Maybe 作为 Alternative ______________________________
instance Alternative Maybe where
  empty               = Nothing

  -- Note that this could have been written more compactly.
  Nothing <|> Nothing = Nothing -- 0 results + 0 results = 0 results
  Just x  <|> Nothing = Just x  -- 1 result  + 0 results = 1 result  ▇▇▇▇▇▇▇▇▇ “合并两个monadic value"：只要有一个不是 Nothing,则有结果
  Nothing <|> Just x  = Just x  -- 0 results + 1 result  = 1 result
  Just x  <|> Just y  = Just x  -- 1 result  + 1 result  = 1 result:
                                -- Maybe can only hold up to one result, so we discard the second one.

其语义为：
    ▇▇▇▇▇▇▇▇▇ 对于两个 Maybe 结果，可以用 <|> 对它们进行“合并” 


___________________________ Maybe 作为 Alternative ______________________________
instance Alternative [] where
  empty = []
  (<|>) = (++) -- length xs + length ys = length (xs ++ ys)

❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖❖-}
-- | consume-digit :: 从 String中parse Int 指定的数字，看是否匹配 （失败则返回 Nothing)   
digit :: Int -> String -> Maybe Int
digit _ [] = Nothing
digit i (c : _) | i > 9 || i < 0 = Nothing
                | otherwise      = if [c] == show i then Just i else Nothing


binChar :: String -> Maybe Int
binChar s = digit 0 s <|> digit 1 s
{-
Now, (<|>) can be used to run two parsers in parallel. 
That is, 
    we use the result of the first one if it succeeds, 
    and otherwise, we use the result of the second.
    If both fail, then the combined parser returns Nothing. 

We can use digit with (<|>) to, for instance, parse strings of binary digits:

▇▇▇▇▇▇▇▇ This usage pattern can be described in terms of "choice". 
For instance, if we want to give binChar a string that will be successfully parsed, 
    we have two choices: either to begin the string with '0' or with '1'.
-}
{-▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃-}

pythags_0 =
    [ (x, y, z)
    | z <- [1 ..]
    , x <- [1 .. z]
    , y <- [x .. z]
    , x ^ 2 + y ^ 2 == z ^ 2
    ]

pythags :: [(Integer, Integer, Integer)]
pythags = do
    z <- [1 ..]
    x <- [1 .. z]
    y <- [x .. z]
    guard (x ^ 2 + y ^ 2 == z ^ 2)
    return (x, y, z)
{-
pythags' :: [(Integer, Integer, Integer)]
pythags' =
  [1..] >>= \z ->
  [1..z] >>= \x ->
  [x..z] >>= \y ->
  guard (x^2 + y^2 == z^2) >>= \_ ->
  return (x, y, z)    
-}
pythags' :: [(Integer, Integer, Integer)]
pythags' =
    let ret x y z = [(x, y, z)]
        -- guard True = [()], guard False = []
        -- gd :: Integer -> Integer -> Integer -> [(Integer, Integer, Integer)]
        gd z x y = concatMap (\_ -> ret x y z)
                             ((guard $ x ^ 2 + y ^ 2 == z ^ 2) :: [()])
        doY z x = concatMap (gd z x) [x .. z]
        doX z = concatMap (doY z) [1 .. z]
        doZ = concatMap doX [1 ..]
    in  doZ
{-
   start
   |_________________________...
   |    |         |
z  1    2         3
   |    |____     |____________
   |    |    |    |       |    |
x  1    1    2    1       2    3
   |    |_   |    |___    |_   |
   |    | |  |    | | |   | |  |
y  1    1 2  2    1 2 3   2 3  3


Each combination of z, x and y represents a route through the tree. 
-}
{-▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃-}
char :: Char -> String -> Maybe (Char, String)
char c s = do
    let (c' : s') = s
    if c == c' then Just (c, s') else Nothing

hexChar :: Char -> String -> Maybe (Char, String)
hexChar c s = do
    let (c' : s') = s
    if ishex c && c == c' then Just (c, s') else Nothing

ishex :: Char -> Bool
ishex c = True



t0 = map digit [0 .. 9]
