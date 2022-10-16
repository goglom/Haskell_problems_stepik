module Problem_4'4'9 where

import Data.List (elemIndex, find)
import Data.Char (isLetter, isSpace)

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show


wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

splitOnLines :: String -> [String]
splitOnLines = wordsWhen (=='\n')

trimString :: String -> String
trimString = f . f
      where f = reverse . dropWhile isSpace 

parseLine :: String -> Either Error (String, String)
parseLine line = case break (== '=') line of
      (_, [])    -> Left ParsingError
      (_, "=")   -> Left ParsingError
      (p1, _:p2) -> Right (trimString p1, trimString p2)

findIntegerField :: String -> [(String, String)] -> Either Error Int
findIntegerField fieldName fields = case find (\(key, value) -> key == fieldName) fields  of
      Nothing            -> Left IncompleteDataError
      Just (_, valueStr) ->  case reads valueStr :: [(Int, String)] of
                                  [(fieldVlue,"")] -> Right fieldVlue
                                  _                -> Left (IncorrectDataError valueStr)

findStrField :: String -> [(String, String)] -> Either Error String
findStrField fieldName fields = case find (\(key, value) -> key == fieldName) fields  of
      Nothing            -> Left IncompleteDataError
      Just (_, valueStr) -> Right valueStr


makePerson :: [(String, String)] -> Either Error Person
makePerson xs = case findStrField "firstName" xs of
      (Left err)        -> Left err
      (Right firstName) -> case findStrField "lastName" xs of
            Left err       -> Left err
            Right lastName -> case findIntegerField "age" xs of
                  Left err  -> Left err
                  Right age -> Right (Person firstName lastName age)

safetyMap :: (a -> Either e b) -> [a] -> Either e [b]
safetyMap func [] = Right []
safetyMap func (x:xs) = case safetyMap func xs of
      Left err -> Left err
      Right ys -> case func x of
            Left err -> Left err
            Right y  -> Right (y:ys)

parsePerson :: String -> Either Error Person
parsePerson input = let  lines = map trimString (splitOnLines input) in  
      case safetyMap parseLine lines of
            Left err     -> Left err
            Right fields -> makePerson fields


test1 = parsePerson "firstName = John\nlastName = Connor\nage = 30"
--Right (Person {firstName = "John", lastName = "Connor", age = 30})

test2 = parsePerson "firstName = John Smith\nlastName = Connor\nage = 30\nasde=as11"
--Right (Person {firstName = "John Smith", lastName = "Connor", age = 30})

test3 = parsePerson "firstName=Barbarian\nlastName=Conn On\nage=30"
--Right (Person {firstName = "Barbarian", lastName = "Conn On", age = 30})