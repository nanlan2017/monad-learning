module Wiki.IOApplicative where

import           Text.Read                      ( readMaybe )
import           Control.Applicative            ( liftA2
                                                , (<*>)
                                                )

interactiveDoubling = do
    putStrLn "Choose a number:"
    s <- getLine
    let mx = readMaybe s :: Maybe Double
    case mx of
        Just x  -> putStrLn ("The double of your number is " ++ show (2 * x))
        Nothing -> do
            putStrLn "This is not a valid number. Retrying..."
            interactiveDoubling


interactiveDoubling' = do
    putStrLn "Choose a number:"
    s <- getLine
    let mx = readMaybe s :: Maybe Double
    case fmap (2 *) mx of
        Just d  -> putStrLn ("The double of your number is " ++ show d)
        Nothing -> do
            putStrLn "This is not a valid number. Retrying..."
            interactiveDoubling'
{-***************************************************************************************************-}
sumInput = do
    s1 <- getLine
    let x1 = readMaybe s1 :: Maybe Double
    s2 <- getLine
    let x2 = readMaybe s2 :: Maybe Double
    case liftA2 (+) x1 x2 of
        Just d  -> putStrLn $ "result is " ++ show d
        Nothing -> do
            putStrLn "Nothing"
            sumInput

sumInput' = do
    s1 <- getLine
    let x1 = readMaybe s1 :: Maybe Double
    s2 <- getLine
    let x2 = readMaybe s2 :: Maybe Double
    case (+) <$> x1 <*> x2 of
        Just d  -> putStrLn $ "result is " ++ show d
        Nothing -> do
            putStrLn "Nothing"
            sumInput'

interactiveConcatenating :: IO ()
interactiveConcatenating = do
    sz <- putStrLn "Choose two strings:" *> ((++) <$> getLine <*> getLine)
    putStrLn "Let's concatenate them:" *> putStrLn sz
