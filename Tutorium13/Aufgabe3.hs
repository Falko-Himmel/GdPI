-- Schreibe eine Funktion eitherFilter, welche alle Elemente, auf die das Prädikat zutrifft, in
-- Right und alle anderen in Left speichert.
-- Beispiel:
-- • eitherFilter even [1,42,7] ≡ [Left 1, Right 42, Left 7]
-- eitherFilter :: (a -> Bool) -> [a] -> [Either a a]

-- es wird nicht gefordert, dass wir das mit einem fold machen müssen => rekursiv (falls ihr schneller mit höherer Funktion könnt ist das natürlich super)
{- HLINT ignore "Use second" -}


eitherFilter :: (a -> Bool) -> [a] -> [Either a a]
eitherFilter = undefined




























eitherFilter' :: (a -> Bool) -> [a] -> [Either a a]
eitherFilter' _ [] = []
eitherFilter' p (x:xs) = (if p x then Right x else Left x) : eitherFilter' p xs


-- oder schneller mit map

eitherFilter'' :: (a -> Bool) -> [a] -> [Either a a]
eitherFilter'' p = map (\x -> if p x then Right x else Left x)



-- Implementieren Sie eine Funktion list2multiset, welche in einer Liste die Häufigkeit der
-- vorkommenden Elemente zählt.
-- Hinweis: Die Reihenfolge der Elemente muss in der Ergebnisliste nicht berücktsichtigt werden.
-- Beispiele:
-- • list2multiset [] ≡ []
-- • list2multiset [1,2,3,2,2,3,4,3,4,4] ≡ [(1,1),(2,3),(3,3),(4,3)]
-- • list2multiset [5,1,3,1,5,2,3] ≡ [(5,2),(1,2),(3,2),(2,1)]
-- list2multiset :: Eq a => [a] -> [(a, Int)]

count :: Eq a => [a] -> Int
count = foldr (\ x -> (+) 1) 0


















list2multiset' :: Eq a => [a] -> [(a, Int)]
list2multiset' [] = []
list2multiset' (x:xs) = (x, 1 + count (filter (== x) xs)) : list2multiset' (filter (/= x) xs)







-- Implementieren Sie eine Funktion selects, die ein Prädikat und eine Liste erhält und eine Liste
-- zurückgibt, die die Elemente die das Prädikat erfüllen und jeweils die Eingabeliste, ohne das
-- entsprechende Element, enthält.
-- Beispiele:
-- • selects ((== 4) . length) ["Matt", "Pike", "Al", "Cisneros"]
-- ≡ [("Matt",["Pike","Al","Cisneros"]),("Pike",["Matt","Al","Cisneros"])]
-- • selects odd [2,4..10] ≡ []
-- selects :: (a -> Bool) -> [a] -> [(a, [a])]






















selects' :: (a -> Bool) -> [a] -> [(a, [a])]
selects' _ [] = []
selects' p xs = selects'' [] xs 
    where
        selects'' _ [] = []
        selects'' ys (x:xs) 
            | p x = (x, ys ++ xs) : selects'' (ys ++ [x]) xs
            | otherwise = selects'' (ys ++ [x]) xs