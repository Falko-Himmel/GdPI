-- funktionen in haskell sind ein bisschen vergleichbar mit mathematischen funktionen
-- funktion besteht aus einem Bezeichner, Parametern und einem Ausdruck

-- z.b.
func x y = x + y


-- zustäzlich kann man optional eine Typsignatur hinzufügen

func' :: Integer -> Integer -> Integer
func' x y = x + y

-- Dabei Dateiname getrennt mit zwei Punkten und dann die Typen der Parameter in der richtigen Reihenfolge, der letzte Typ ist der Rückgabetyp


-- Aufgabe: Wir wollen die Zahlen 1 - 5 als Wort ausgeben, sonst ein Fehler










-- bisschen vorgegriffen eventuell, aber so können wir auch rekursion machen
-- bei einem rekursiven algorithmus brauchen wir dabei immer einen Basisfall (mit diesem "bricht" die Rekursion ab)
-- beispiel Fakultät: z.b. 6! = 6 * 5 * 4 * 3 * 2 * 1


fak :: Integer -> Integer
fak 0 = 1
fak x = x * fak (x-1)

-- oder alternativ 

fak' 0 acc = acc
fak' x acc = fak' (x-1) (x * acc)

-- Frage was ist was? Welche Rekursionsart?

