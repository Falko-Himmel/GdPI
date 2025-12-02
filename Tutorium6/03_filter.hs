{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}


-- filter sind eine in haskell eingebaute methode um Listen auf bestimmte Kriterien zu überprüfen und nur die Elemente zurückzugeben die dieses kriterium erfüllen.
-- dabei nimmt filter eine Funktion die ein Bool zurückgibt (True/False) und eine Liste von Elementen
-- und gibt eine neue Liste zurück die nur die Elemente enthält für die die Funktion True zurückgibt.

-- beispiel: Wir wollen aus einer Liste von Zahlen nur die geraden Zahlen herausfiltern
filterEvens :: [Int] -> [Int]
filterEvens xs = filter (\x -> even x) xs

-- beispiel: Wir wollen aus einer Liste von Zahlen nur die Zahlen größer als 5 herausfiltern
filterGreaterThanFive :: [Int] -> [Int]
filterGreaterThanFive xs = filter (\x -> x > 5) xs
