-- Listen

-- data [a] = [] | a : [a]
-- • [] beschreibt die leere Liste
-- • a : [a] beschreibt eine Liste, an die ein Element vom Typ a von Vorn
-- angefügt wurde. In einem Ausdruck x : xs nennen wir x den Kopf (Head)
-- und xs den Rest (Tail) der Liste.

-- Eine Liste ist "quasi" wie ein Aufzälungsdatentyp
-- Eine Liste [a] kann dabei dann immer entweder die leere Liste sein oder ein Element mit wieder einer Restliste
-- => hier kommt die Rekursion ins Spiel (da dann wieder leere Liste oder Element ...)

-- Bsp.: Wir wollen eine Liste [1, 2, 3, 4] erstellen
-- wir haben die Elemente 1, 2, 3, 4 (alle vom Typ Num => a ist also Num)

-- Anfang: wir nehmen das Element 1 und packen dann Rekursiv die Restliste dran
-- 1 : "[Num]"
-- Dann nehmen wir das Element 2 und auch wieder dran
-- 1 : 2 : "[Num]"
-- ...
-- 1 : 2 : 3 : 4 : [] = [1, 2, 3, 4] -- Die Schreibweise mit eckigen Klammern ist nur eine Abkürzung (Listenliteral)

-- Wichtige Funktionen auf Listen
-- Wir wollen den Kopf einer übergebenen Liste erhalten
head' :: [a] -> a
head' (x:xs) = x -- 1 : [2, 3, 4]

-- hier spalten wir das erste Element der Liste ab (durch x:xs, also x ist der Kopf und xs die restliche Liste)
-- bei ausführung von 
test :: Num a => a
test = head' [1, 2, 3, 4] -- erhalten wir 1, da x:xs = 1:[2,3,4]


-- Weil wir es oben schon von Literalen hatten:
-- eine Liste aus Chars [Char] ist ein String
-- ['H', 'a', 'l', 'l', 'o'] == "Hallo"

-- Aufgabe: Wir suchen ein Element x und wollen das erste passende Element zurückgeben
findFirst :: (Num a, Eq a) => a -> [a] -> a
findFirst _ [] = -1 -- leere Liste => -1 als Fehlerwert
findFirst p (x:xs)
  | p == x =  x
  | otherwise = findFirst p xs 

-- Aufgabe: den n-ten Wert aus einer Liste zurückgeben
nthElement :: Int -> [a] -> a
nthElement 0 (x:_)  = x -- wenn n 0 ist, geben
nthElement _ []     = error "Index out of bounds" -- wenn die Liste leer ist, Fehler
nthElement n (_:xs) = nthElement (n-1) xs -- ansonsten rekursiv weitersuchen




-- Aufgabe: Wir wollen bei einer übergebenen Listen mit positiven Zahlen den ersten Wert zurückgeben der größer ist als der übergebene Parameter
firstGreaterThan :: (Ord a, Num a) => a -> [a] -> a
firstGreaterThan _ [] = -1 -- hier behandeln wir den Fall, dass die Liste leer ist (-1 ist Fehlerwert)
firstGreaterThan n (x:xs)
    | x > n     = x
    | otherwise = firstGreaterThan n xs

-- beispielhafe Ausführung:
test2 :: (Num a, Ord a) => a
test2 = firstGreaterThan 3 [1, 2, 3, 4, 5] -- hier erhalten wir 4

-- weil: firstGreaterThan 3 (1:[2,3,4,5])
-- x = 1, xs = [2,3,4,5]
-- 1 > 3 ? no => otherwise => firstGreaterThan 3 [2,3,4,5]
-- x = 2, xs = [3,4,5]
-- 2 > 3 ? no => otherwise => firstGreaterThan 3 [3,4,5]
-- x = 3, xs = [4,5]    
-- 3 > 3 ? no => otherwise => firstGreaterThan 3 [4,5]
-- x = 4, xs = [5]
-- 4 > 3 ? yes => return 4


-- nächste Aufgabe: Wir wollen ein Element ans Ende einer Liste anhängen
cons :: [a] -> a -> [a]
cons [] el     = [el]        -- wenn die Liste leer ist, fügen wir das Element in die leere Liste ein (Basisfall)
cons (x:xs) el = x : cons xs el -- ansonsten fügen wir das Element rekursiv ans Ende der Liste

-- weil: cons [1, 2, 3] 4
-- x = 1, xs = [2,3]
-- return 1 : cons [2,3] 4
-- x = 2, xs = [3]
-- return 2 : cons [3] 4
-- x = 3, xs = []
-- return 3 : cons [] 4
-- xs = [] => Basisfall => return [4]
-- also insgesamt: [1, 2, 3, 4]

-- nächste Aufgabe zum einfügen eines Elements: wir wollen einen index übergeben ab diesem das Element eingefügt werden soll. 
-- Ist der index zu groß so soll das Element am Ende der Liste angefügt werden.
insertAt :: a -> Int -> [a] -> [a]
insertAt el 0 xs     = el : xs -- wenn index 0 ist, fügen wir das Element vorne an (Basisfall)
insertAt el _ []     = [el]    -- wenn die Liste leer ist, fügen wir das Element in die leere Liste ein (Basiisfall)
insertAt el n (x:xs) = x : insertAt el (n-1) xs -- ansonsten fügen wir das Element rekursiv ein

-- beispielhafte Ausführung:
test3 :: [Int]
test3 = insertAt 99 2 [1, 2, 3, 4, 5] -- hier erhalten wir [1, 2, 99, 3, 4, 5]

-- weil: insertAt 99 2 (1:[2,3,4,5])
-- x = 1, xs = [2,3,4,5]
-- return 1 : insertAt 99 1 [2,3,4,5]
-- x = 2, xs = [3,4,5]
-- return 2 : insertAt 99 0 [3,4,5]
-- hier treffen wir auf den Basisfall (index 0)
-- return 99 : [3,4,5]
-- also insgesamt: [1, 2, 99, 3, 4, 5]

-- Letzte Aufgabe: Wir wollen alle 2er aus einer Liste entfernen
removeTwos :: (Eq a, Num a) => [a] -> [a]
removeTwos [] = [] -- Basisfall: leere Liste bleibt leer
removeTwos (x:xs)
    | x == 2    = removeTwos xs      -- wenn x 2 ist, überspringen wir es
    | otherwise = x : removeTwos xs  -- sonst fügen wir x zur Ergebnisliste hinzu

-- beispielhafte Ausführung:
testRemoveTwos :: [Int]
testRemoveTwos = removeTwos [1, 2, 3, 2, 4, 2, 5] -- hier erhalten wir [1, 3, 4, 5]
-- weil: removeTwos (1:[2,3,2,4,2,5])
-- x = 1, xs = [2,3,2,4,2,5]
-- 1 == 2 ? no => return 1 : removeTwos [2,3,2,4,2,5]
-- x = 2, xs = [3,2,4,2,5]
-- 2 == 2 ? yes => removeTwos [3,2,4,2,5]
-- x = 3, xs = [2,4,2,5]
-- 3 == 2 ? no => return 3 : removeTwos [2,4,2,5]
-- x = 2, xs = [4,2,5]
-- 2 == 2 ? yes => removeTwos [4,2,5]
-- x = 4, xs = [2,5]
-- 4 == 2 ? no => return 4 : removeTwos [2,5]
-- x = 2, xs = [5]
-- 2 == 2 ? yes => removeTwos [5]
-- x = 5, xs = []
-- 5 == 2 ? no => return 5 : removeTwos []
-- xs = [] => Basisfall => return []
-- also insgesamt: [1, 3, 4, 5]

-- Konkatenation von Listen einfach über den Operator (++)
-- Beispiel:
concatLists :: [a] -> [a] -> [a]
concatLists xs ys = xs ++ ys    

-- beispielhafte Ausführung:
test4 :: [Int]
test4 = concatLists [1, 2, 3] [4, 5, 6] -- hier erhalten wir [1, 2, 3, 4, 5, 6]





