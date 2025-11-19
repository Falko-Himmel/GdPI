--  in haskell gibt es verschiedene möglichkeiten verzweigungen zu machen
--  eine Möglichkeit ist if then else (ähnlich wie in python, java, c++, ...)
--  unser beispielalgorithmus soll zu einer zahl sagen, ob die zahl positiv, neutral oder negativ ist


signumIf :: Integer -> String
signumIf x =
  if x < 0
    then "negativ"
    else
      if x == 0
        then "null"
        else "positiv"

--  pattern matching mit guards (wächter) ist eine weitere möglichkeit verzweigungen zu machen

signumGuards :: Integer -> String
signumGuards x
  | x < 0 = "negativ"
  | x == 0 = "null"
  | otherwise = "positiv"

--  case of ist eine weitere möglichkeit verzweigungen zu machen
-- bei diesem beispiel bisschen schwieriger weil case of ähnlich wie switch case in python, java, ... und nicht direkt <, > ermöglicht, kann man hier hilfsmethode verwenden

signumCase :: Integer -> String
signumCase x = case compare x 0 of
  LT -> "negativ"
  EQ -> "null"
  GT -> "positiv"

--  compare x 0 gibt LT, EQ oder GT zurück je nachdem ob x kleiner, gleich oder größer als 0 ist, ist bereits eine eingebaute funktion in haskell

-- anderes Beispiel für case of
foo :: Integer -> String
foo x = case x of
  0 -> "null"
  1 -> "eins"
  _ -> "etwas anderes"

