module Task_4'3'3 where
  
import Data.Time.Clock
import Data.Time.Format
--import System.Locale

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving Show

data LogEntry = LogEntry {timestamp :: UTCTime, logLevel :: LogLevel, message :: String}

logLevelToString :: LogLevel -> String
logLevelToString = show  

logEntryToString :: LogEntry -> String
logEntryToString x =  timeToString (timestamp  x) ++ ": " ++ show (logLevel x) ++ ": " ++ message x