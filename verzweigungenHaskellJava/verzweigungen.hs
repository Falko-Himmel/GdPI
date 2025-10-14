--  in haskell gibt es verschiedene möglichkeiten verzweigungen zu machen
--  die "einfachste" ist if then else

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

signumCase :: Integer -> String
signumCase x = case compare x 0 of
  LT -> "negativ"
  EQ -> "null"
  GT -> "positiv"

--  compare x 0 gibt LT, EQ oder GT zurück je nachdem ob x kleiner, gleich oder größer als 0 ist, ist bereits eine eingebaute funktion in haskell