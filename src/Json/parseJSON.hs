module Json.ParseJSON where

import           Text.Parsec                    ( (<|>)
                                                , spaces
                                                , char
                                                , sepBy
                                                , string
                                                , many1
                                                , noneOf
                                                , many
                                                , digit
                                                , oneOf
                                                , parseTest
                                                )
import           Text.Parsec.String             ( Parser )
-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩

type Name = String -- This is sort of the label of the object / sub-object

data JsonPair = Label Name JSON -- A JSON object has a label (Name) which is followed by colon + curly braces and data
  deriving Show

data Numbers = Fraction Float | Integer Int | Negative Int | Exponential String
  deriving Show

data JSON = Object [JsonPair] -- One whole object, as well as potential sub-objects
          | Boolean Bool
          | Str String
          | Number Numbers
          | Array [JSON] -- JSON Arrays repeat any of the other types / itself
          | None -- In case there is a nullpointer in the file
  deriving Show

json :: Parser JSON
json = object <|> boolean <|> str <|> number <|> array <|> none

object :: Parser JSON
object = do
    spaces
    char '{'
    spaces
    kind <- parseJSONData `sepBy` (spaces >> char ',' >> spaces) -- kind is the entire line, e.g. "test-attribute" : value,
    spaces
    char '}'
    spaces
    return (Object kind)


parseJSONData :: Parser JsonPair
parseJSONData = do
    name  <- getName
    value <- (object <|> array <|> boolean <|> none <|> str <|> number)
    return (Label name value)

getName :: Parser Name
getName = do
    spaces
    name <- parseStr
    spaces
    char ':'
    spaces
    return name

boolean :: Parser JSON
boolean = do
    spaces
    boolean <- string "true" <|> string "false"
    spaces
    if boolean == "true" then return (Boolean True) else return (Boolean False)

str :: Parser JSON
str = do
    spaces
    string <- parseStr
    spaces
    return (Str string)

parseStr = do
    char '\"'
    str <- many1 (noneOf "\"")
    char '\"'
    return str

number :: Parser JSON
number = do
    spaces
    fullNumber <- many (digit <|> oneOf ".-+eE") -- Number can be int, which is only digits, or a floating point number of a negative number
    spaces
    if (charFound (head "e") fullNumber)
        then return (Number (Exponential (fullNumber))) -- Note: I am only returning exponentials as strings
        else if (charFound (head "E") fullNumber)
            then return (Number (Exponential (fullNumber)))
            else
                if (charFound (head ".") fullNumber)
                    && (charFound (head "-") fullNumber)
                then
                    return (Number (Fraction (read fullNumber))) -- Negative float
                else
                    if (charFound (head ".") fullNumber)
                        then return (Number (Fraction (read fullNumber)))
                        else if (charFound (head "-") fullNumber)
                            then return (Number (Negative (read fullNumber)))
                            else return (Number (Integer (read fullNumber)))

charFound :: Char -> String -> Bool
charFound _ [] = False
charFound c (x : xs) | c == x    = True
                     | otherwise = charFound c xs

array :: Parser JSON
array = do
    spaces
    char '['
    spaces
    arr <-
        (object <|> array <|> boolean <|> none <|> str <|> number)
            `sepBy` (spaces >> char ',' >> spaces)
    spaces
    char ']'
    spaces
    return (Array arr)

none :: Parser JSON
none = do
    spaces
    string "null"
    spaces
    return None
-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩

runTest1 = do
    test <- readFile "test1.json"
    parseTest json test

runTest2 = do
    test <- readFile "test2.json"
    parseTest json test
