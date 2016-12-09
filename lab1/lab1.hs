fact               :: Int -> Int
fact i             = if i <= 1 then 1 else i * fact(i-1)

doFact = do
    x <- getLine
    print(show (fact $ read x))