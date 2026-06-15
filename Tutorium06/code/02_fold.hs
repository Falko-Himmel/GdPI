{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use sum" #-}

-- foldr und foldl sind Funktionen, die eine binäre Funktion, einen Anfangswert (Akkumulator) und eine Liste nehmen und die Funktion wiederholt anwenden, um einen einzigen Wert zu erzeugen.
-- Der Unterschied zwischen foldr (fold right) und foldl (fold left) liegt in der Richtung, in der die Liste verarbeitet wird.

-- foldr wendet die Funktion von rechts nach links an, foldl von links nach rechts.
-- foldr bildet einen rekursiven Term von rechts nach links, während foldl die Schritte "einzeln" von links nach rechts macht.


-- WICHTIG ZUM MERKEN: map reduziert die Anzahl der Elemente in der Liste NICHT, sondern bildet jedes Element auf genau ein Element ab.
-- Fold hingegen reduziert die Liste auf einen einzigen Wert. (Element kann aber auch eine Liste sein)


-- foldl und foldr verhält sich manchmal gleich:

--  bsp.: summe berechnen:
foldSumr :: (Foldable t, Num b) => t b -> b
foldSumr xs = foldr (+) 0 xs
-- foldSumr [1,2,3,4,5] = 1+(2+(3+(4+(5+0)))) = 15
-- 1:(2:(3:[])) = obere
foldSuml :: (Foldable t, Num b) => t b -> b
foldSuml xs = foldl (+) 0 xs
-- foldSuml [1,2,3,4,5] = ((((0+1)+2)+3)+4)+5 = 15


-- Unterschied erkennbar bei division: (da die division nicht kommutativ ist)

foldDivr :: (Foldable t, Fractional b) => t b -> b
foldDivr xs = foldr (/) 1 xs
-- foldDivr [2,4,8] = 2/(4/(8/1)) = 4

foldDivl :: (Foldable t, Fractional b) => t b -> b
foldDivl xs = foldl (/) 1 xs
-- foldDivl [2,4,8] = (((1/2)/4)/8) = 1/64


-- Fold kann mit beliebigen Funktionen verwendet werden, die man auch selbst definieren kann:

countEvens ::  [Int] -> Int
countEvens xs = foldr (\x acc -> if even x then acc + 1 else acc) 0 xs
-- countEvens [1,2,3,4,5,6] = 3


-- Auf Fold basieren viele Listenfunktionen:

lengthWithFoldr :: (Foldable t) => t a -> Int
lengthWithFoldr xs = foldr (\_ acc -> acc + 1) 0 xs
-- lengthWithFoldr [1,2,3,4,5] = 5

mapWithFoldr :: (Foldable t) => (a -> b) -> t a -> [b]
mapWithFoldr f xs = foldr (\x acc -> f x : acc) [] xs
-- mapWithFoldr (*2) [1,2,3] = [2,4,6]