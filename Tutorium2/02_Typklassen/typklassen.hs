-- wir hatten vorhin im Beispiel den algorithmus

signum :: Integer -> String
signum x
  | x < 0 = "negativ"
  | x == 0 = "null"
  | otherwise = "positiv"


-- wollen wir jetzt aber nicht nur Ganzzahlen, sondern auch Fließkommazahlen vergleichen so müssten wir Signatur ändern

signum' :: Double -> String
signum' x
  | x < 0 = "negativ"
  | x == 0 = "null"
  | otherwise = "positiv"

-- das ist aber blöd, weil wir so den "gleichen" Code öfters haben, obwohl wir das eigntlich gar nicht brauchen
-- Lösung: Typklassen

-- hier verwenden wir Num, diese enthält dann alle Typen wir Integer, Double, Float und weitere
-- zusätzlich brauchen wir aber auch noch die Typklasse Ord, um unsere Eingabe vergleichen zu können, ob <, > oder gleich
-- würden wir nur auf Gleichheit vergleichen würde Eq reichen, dies ist aber schon im Ord quasi drin

signum'' :: (Ord a, Num a) => a -> String
signum'' x
  | x < 0 = "negativ"
  | x == 0 = "null"
  | otherwise = "positiv"

compare :: (Num a, Eq a) => a -> String
compare x = if x == 2 then "Eingabe ist 2" else "Eingabe ist ungleich 2"

-- vgl komplexe Zahlen, kann man nicht "sortieren", aber vergleichen