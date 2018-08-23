module TransDemo.MaybeIO where

import           Control.Monad
import           Control.Monad.Trans.Maybe      ( MaybeT
                                                , runMaybeT
                                                )
import           Control.Monad.Trans.Class      ( lift )
-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩

main :: IO ()
main = do
    password <- runMaybeT getPassword
    case password of
        Just p  -> putStrLn "valid password!"
        Nothing -> putStrLn "invalid password!"

isValid :: String -> Bool
isValid = (>= 10) . length

getPassword :: MaybeT IO String
getPassword = do
    password <- lift getLine
    guard (isValid password)
    return password
