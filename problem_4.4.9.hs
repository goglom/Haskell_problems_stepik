module Problem_4'4'9 where

import Data.List (elemIndex, find)
import Data.Char (isLetter, isSpace)

data Error = ParsingError String | IncompleteDataError | IncorrectDataError String deriving Show

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
      (_, [])    -> Left $ ParsingError $ "No '=' sign in line: '" ++ line ++ "'"
      (_, "=")   -> Left $ ParsingError "empty part after '='"
      (p1, _:p2) -> Right (trimString p1, trimString p2)


splitOn3Lines :: String -> Either Error [String]
splitOn3Lines str = case map trimString . splitOnLines $ str of
      lines | length lines == 3 -> Right lines
            | otherwise         -> Left $ ParsingError "number of lines not equals to 3"


findIntegerField :: String -> [(String, String)] -> Either Error Int
findIntegerField fieldName fields = case find (\(key, value) -> key == fieldName) fields  of
      Nothing            -> Left IncompleteDataError
      Just (_, valueStr) ->  case reads valueStr :: [(Int, String)] of
                                  [(fieldVlue,"")] -> Right fieldVlue
                                  _                -> Left (IncorrectDataError valueStr)

findStrField :: String -> [(String, String)] -> Either Error String
findStrField fieldName fields = case find (\(key, value) -> key == fieldName) fields  of
      Nothing                                    -> Left IncompleteDataError
      Just (_, valueStr) | all isLetter valueStr -> Right valueStr
                         | otherwise             -> Left (IncorrectDataError valueStr)


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
parsePerson input = case splitOn3Lines input of
      Left err    -> Left err
      Right lines -> case safetyMap parseLine lines of
            Left err     -> Left err
            Right fields -> makePerson fields


example = "firstName = John\nlastName = Connor\nage = 30"