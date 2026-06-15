-- funktionen in haskell sind ein bisschen vergleichbar mit mathematischen funktionen
-- funktion besteht aus einem Bezeichner, Parametern und einem Ausdruck

-- z.b.
add x y = x + y




-- zustäzlich kann man optional eine Typsignatur hinzufügen

func' :: Integer -> Integer -> String -> Integer
func' x y z = x + y

-- Dabei Dateiname getrennt mit zwei Punkten und dann die Typen der Parameter in der richtigen Reihenfolge, der letzte Typ ist der Rückgabetyp


-- Aufgabe: Wir wollen die Zahlen 1 - 3 als Wort ausgeben, sonst ein Fehler


foo :: Integer -> Integer -> String
foo 1 _= "eins"
foo 2 2 = "zwei"
foo 3 2= "drei"
foo _ _= "fehler"







-- bisschen vorgegriffen eventuell, aber so können wir auch rekursion machen
-- bei einem rekursiven algorithmus brauchen wir dabei immer einen Basisfall (mit diesem "bricht" die Rekursion ab)
-- beispiel Fakultät: z.b. 6! = 6 * 5 * 4 * 3 * 2 * 1
-- 5! = 5 * 4! = 5 * ( 4 * 3!) = 5 * ( 4 * (3 * (2!)))



fak :: Integer -> Integer
fak 0 = 1
fak x = x * fak (x-1)

-- oder alternativ 

fak' :: (Eq t, Num t) => t -> t -> t
fak' 0 acc = acc
fak' x acc = fak' (x-1) (x * acc)



-- Frage was ist was? Welche Rekursionsart?

foobar :: String -> String
foobar x = x


-- foob :: (Integer, Integer, Integer) -> (Integer, Integer, Integer) -> (Integer, Integer, Integer)

-- alsdjhf
-- aksdjfhkads


-- kasdjhfa

-- kasdjhk



