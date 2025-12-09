{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}


-- altklausur 2024 2
-- im letzten Tutorium haben wir die Funktionen fold, map und filter näher kennengelernt.
-- weil diese Funktionen für die Klausur essentiell sind, wollen wir sie hier noch weiter üben.


-- Aufgabe 3b)

-- Implementieren Sie ein Funktion count, die die Zahl der Elemente zurückgibt, die das übergebene
-- Prädikat erfüllen. (3 Punkte)

-- Beispiele:
-- • count (\x -> 'A' <= x && x <= 'Z') "Hello World!" ≡ 2
-- • count odd [1,2,3,2,2,3,4,3,4,4] ≡ 4


count :: (a -> Bool) -> [a] -> Int
count = undefined
























-- Lösung mit filter:
--        Prädikatsfunk. lst.   result
count' :: (a -> Bool) -> [a] -> Int
count' p xs = length (filter p xs)


-- Lösung mit fold:
count'' :: (a -> Bool) -> [a] -> Int
count'' p xs = foldr (\x acc -> if p x then acc + 1 else acc) 0 xs

-- rekursive Lösung:
count''' :: (a -> Bool) -> [a] -> Int
count''' _ [] = 0
count''' p (x:xs) = if p x then 1 + count''' p xs else count''' p xs


-- =====================================================================================================================

-- Aufgabe 3a) (4 Punkte)

-- Schreibe eine Funktion eitherFilter, welche alle Elemente, auf die das Prädikat zutrifft, in
-- Right und alle anderen in Left speichert.
-- Beispiel:
-- • eitherFilter even [1,42,7] ≡ [Left 1, Right 42, Left 7]


eitherFilter :: (a -> Bool) -> [a] -> [Either a a]
eitherFilter = undefined



















-- Rekursive Lösung:
eitherFilter' :: (a -> Bool) -> [a] -> [Either a a]
eitherFilter' _ [] = []
eitherFilter' p (x:xs)
    | p x = Right x : eitherFilter' p xs
    | otherwise = Left x : eitherFilter' p xs

-- Lösung mit map:
eitherFilter'' :: (a -> Bool) -> [a] -> [Either a a]
eitherFilter'' p xs = map (\x -> if p x then Right x else Left x) xs