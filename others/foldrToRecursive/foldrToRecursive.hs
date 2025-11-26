-- bsp foldr zu rekursiv übersetzten

foldrSum :: Num a => [a] -> a
foldrSum xs = foldr(\x acc -> x + acc) 0 xs

foldrSum' :: Num a => [a] -> a
foldrSum' xs = foldr(+) 0 xs



-- Note how foldr essentially replaces the (:) constructor with some other function (in this case (+)).
-- Das heißt, wir können ja Listen mit dem : Operator kontruieren:

-- [1, 2] === 1:(2:[])
-- [1,2,3] === 1:(2:(3:[]))

-- Wenn wir jetzt die Summe einer Liste berechnen wollen, ersetzen wir den (:) Operator mit (+) und die leere Liste ([]) mit 0 (basecase):
-- foldrSum [1,2,3] === 1+(2+(3+0)) = 6



-- Umbauen von foldrSum zu einer rekursiven Funktion:
-- Definition D7: foldr
-- Akkumuliert das Funktionsergebnis des aktuellen Elements und dem bisherigen Akkumulator:

--               +          acc   lst
--          ____________    ___   ___  
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr _ c [] = c
-- foldr f c (x:xs) = x `f` foldr f c xs


foldrSum'' :: Num a => [a] -> a
foldrSum'' xs = foldr(+) 0 xs

-- alles ersetzen:

recursiveSum :: Num a => a -> [a] -> a
recursiveSum c [] = c
recursiveSum c (x:xs) = x + recursiveSum c xs

-- mit akkumulator = c = 0 für addition:
recursiveSum' :: Num a => [a] -> a
recursiveSum' xs = recursiveSum 0 xs
-- oder direkt:
recursiveSum'' :: Num a => [a] -> a
recursiveSum'' [] = 0
recursiveSum'' (x:xs) = x + recursiveSum'' xs