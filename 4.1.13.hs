module Task_4'1'13 where

data Result = Fail | Success
data SomeData  = A|B|BB

doSomeWork :: SomeData -> (Result,Int)
doSomeWork x = (Fail, 4)

processData :: SomeData -> String
processData x = case doSomeWork x of
    (Success, 0) -> "Success"
    (Fail, n)    -> "Fail: " ++ show n
    _            -> error "bad result"