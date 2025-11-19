-- wir hatten vorhin im Beispiel den algorithmus

numberToString :: Integer -> String
numberToString 0 = "null"
numberToString 1 = "eins"
numberToString 2 = "zwei"
numberToString 3 = "drei"
numberToString _ = "fehler"


greaterFive :: Integer -> Bool
greaterFive x = x > 5

-- wollen wir jetzt aber nicht nur Ganzzahlen, sondern auch Fließkommazahlen vergleichen so müssten wir Signatur ändern

numberToString' :: Double -> String
numberToString' 0 = "null"
numberToString' 1 = "eins"
numberToString' 2 = "zwei"
numberToString' 3 = "drei"
numberToString' _ = "fehler"


greaterFive' :: Double -> Bool
greaterFive' x = x > 5

-- das ist aber blöd, weil wir so den "gleichen" Code öfters haben, obwohl wir das eigntlich gar nicht brauchen
-- Lösung: Typklassen

-- hier verwenden wir Num, diese enthält dann alle Typen wir Integer, Double, Float und weitere
-- zusätzlich brauchen wir auch noch Eq, da wir ja Zahlen vergleichen wollen

numberToString'' :: (Eq a, Num a) => a -> String
numberToString'' 0 = "null"
numberToString'' 1 = "eins"
numberToString'' 2 = "zwei"
numberToString'' 3 = "drei"
numberToString'' _ = "fehler"

-- mit dem anderen Beispiel
-- hier brauchen wir auch wieder Num und akzeptieren damit Typen wie Integer, Double, Float und weitere
-- zusätzlich aber auch noch Ord, da wir ja unseren Parameter auf > vergleichen. Heißt der Typ muss eine Ordnung besitzen

greaterFive'' :: (Ord a, Num a) => a -> Bool
greaterFive'' x = x > 5



-- wichtig dabei zu sagen ist noch, dass nicht Eq gleich Ord
-- vgl komplexe Zahlen, kann man nicht "sortieren", aber vergleichen