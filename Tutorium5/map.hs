{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}


-- Bei maps geht es darum eine Liste an Elementen zu nehmen und auf jedes Element eine Funktion anzuwenden um eine neue Liste zu erzeugen.
-- Dabei wird bei dem map jedes Element der Liste auf genau ein Element der neuen Liste abgebildet (daher der Name map)

-- würden wir normal probieren eine Liste von Zahlen alle zu quadrieren, dann könnte das so aussehen:
squareList :: Num a => [a] -> [a]
squareList []     = [] -- Basisfall: leere Liste bleibt leer
squareList (x:xs) = (x^2) : squareList xs -- rekursiver Fall: Quadrat von x + quadratListe von xs

-- beispielhafte Ausführung:
test1 :: [Int]
test1 = squareList [1, 2, 3] -- hier erhalten wir [1, 4, 9]
-- weil: squareList (1:[2,3])
-- x = 1, xs = [2,3]
-- return (1^2) : squareList [2,3]
-- x = 2, xs = [3]
-- return (2^2) : squareList [3]
-- x = 3, xs = []
-- return (3^2) : squareList []
-- xs = [] => Basisfall => return []
-- also insgesamt: [1, 4, 9]

-- das funktioniert zwar alles schon ganz gut, aber Haskell bietet uns mit der Funktion map eine eingebaute Funktion die genau das macht.
-- das gleiche mit einem map sieht dann so aus:
squareListMap :: Num a => [a] -> [a]
squareListMap xs = map (\x -> x^2) xs

-- hier verwenden wir eine Lamda-Funktion (\x -> x^2), die jedes Element x der Liste nimmt und es quadriert.
-- Dabei "gibt" das map bei jedem Element der Liste das Element der Lambda-Funktion
-- dieses dann in das x "reinschreibt" und dann nach dem -> auf x^2 abgebildet wird.

-- weiteres beispiel: Wir wollen alle geraden Zahlen in einer Liste verdoppeln
doubleEvens ::  [Int] -> [Int]
doubleEvens xs = map (\x -> if even x then x * 2 else x) xs

-- hier "stecken" wir auch wieder jedes Element der Liste nacheinander in das x, schauen nach dem -> ob es gerade
-- ist oder nicht und verdopppelt es dann wenn gerade
-- das map führt dann auch hier wieder dieses lamda für alle Elemente der Liste aus und bildet 
-- dann eine neue Liste wobei die Anzahl der Elemente und die Reihenfolge der dann abgebildeten Elemente gleich bleibt.




