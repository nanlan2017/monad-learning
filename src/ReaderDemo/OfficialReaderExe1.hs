module ReaderDemo.OfficialReaderExe1 where

import           Control.Monad.Reader           ( Reader
                                                , ask
                                                , asks
                                                , runReader
                                                )
import           Data.Map                      as Map
{- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
                        Module 说明
Example 1: Simple Reader Usage

In this example 
the Reader monad provides access to variable bindings (<- Environment). 
Bindings are a Map of integer variables. 

The variable count contains number of variables in the bindings. 
(这个 Map 中有个key: count,其值为整个map 的size)

You can see :
    1. how to run a Reader monad and retrieve data from it with runReader, 
    2. how to access the Reader data with ask and asks.


////////////////////////////////////////////////////////////////////////////////// -}
{-▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃-}


type Bindings = Map String Int;

-- Returns True if the "count" variable contains correct bindings size.
isCountCorrect :: Bindings -> Bool
isCountCorrect bindings = runReader calc_isCountCorrect bindings

-- The Reader monad, which implements this complicated check.
calc_isCountCorrect :: Reader Bindings Bool
calc_isCountCorrect = do
    count    <- asks (lookupVar "count")
    bindings <- ask
    return (count == (Map.size bindings))

-- The selector function to  use with 'asks'.
-- Returns value of the variable with specified name.
lookupVar :: String -> Bindings -> Int
lookupVar name bindings = maybe 0 id (Map.lookup name bindings)



{-▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃-}
sampleBindings = Map.fromList [("count", 3), ("1", 1), ("b", 2)]

main = do
    putStr $ "Count is correct for bindings " ++ show sampleBindings ++ ": "
    print (isCountCorrect sampleBindings)
