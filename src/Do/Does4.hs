module Do.Does4 where

nameDo :: IO ()
nameDo = do
    putStr "What is your first name? "
    first <- getLine
    putStr "And your last name? "
    last <- getLine
    let full = first ++ " " ++ last
    putStrLn ("Pleased to meet you, " ++ full ++ "!")

nameLambda :: IO ()
nameLambda = putStr "What is your first name? " >> getLine >>= \first ->
    putStr "And your last name? " >> getLine >>= \last ->
        let full = first ++ " " ++ last
        in  putStrLn ("Pleased to meet you, " ++ full ++ "!")



-- ********************************************************  
nameReturn :: IO String
nameReturn = do
    putStr "What is your first name? "
    first <- getLine
    putStr "And your last name? "
    last <- getLine
    let full = first ++ " " ++ last
    putStrLn ("Pleased to meet you, " ++ full ++ "!")
    return full

greetAndSeeYou :: IO ()
greetAndSeeYou =
    nameReturn >>= (\name -> putStrLn ("See you, " ++ name ++ "!"))

printSeeYou :: String -> IO ()
printSeeYou name = putStrLn ("See you, " ++ name ++ "!")
greetAndSeeYou' :: IO ()
greetAndSeeYou' = nameReturn >>= printSeeYou


seeYou :: String -> String
seeYou name = "See you, " ++ name ++ "!"
-- Reminder: fmap f m  ==  m >>= (return . f)  ==  liftM f m
greetAndSeeYou'' :: IO ()
greetAndSeeYou'' = fmap seeYou nameReturn >>= putStrLn
-- ********************************************************  


