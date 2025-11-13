-- mit Hilfe unserer primitiven Datentypen wie Int, Double, Float, Char, ... 
-- können wir jetzt auch mit algebraischen Datentypen komplexe Datentypen bauen

-- Aufzählungsdatentypen (vgl Java Enumerations)
-- bei diesem Typ "zählt" man eine Menge an Werten auf, die der Typ annehmen kann

data Wochentag = Montag | Dienstag | Mittwoch | Donnerstag | Freitag | Samstag | Sonntag
  deriving (Show, Eq) -- dadurch werden Eigenschaften für Ausgabe und Vergleich automatisch durch Haskell generiert
    



getWochentag :: Integer -> Wochentag
getWochentag 1 = Montag
getWochentag 2 = Dienstag
getWochentag 3 = Mittwoch
getWochentag 4 = Donnerstag
getWochentag 5 = Freitag
getWochentag 6 = Samstag
getWochentag 7 = Sonntag
getWochentag _ = error "Ungültiger Wochentag"

printWochentagZahl :: Wochentag -> Integer
printWochentagZahl Montag    = 1
printWochentagZahl Dienstag  = 2
printWochentagZahl Mittwoch  = 3
printWochentagZahl Donnerstag = 4
printWochentagZahl Freitag   = 5
printWochentagZahl Samstag   = 6
printWochentagZahl Sonntag   = 7


-- funktioniert nur mit dem deriving Eq
foo :: Wochentag -> String
foo x = if x == Montag then "ja" else "nein"


data Obst = Apfel Double| Banane Int| Orange | Birne
  deriving (Show, Eq)


-- Produktdatentypen (vgl Java Klassen)
-- hier werden verschiedene Werte zu einem neuen Typ kombiniert
data Person = Person String Integer 
  deriving (Show, Eq)

printName :: Person -> String
printName (Person name alter) = name

data Auto = Auto String String Integer -- Auto mit Marke, Modell und Baujahr
  deriving (Show, Eq)


-- noch ein paar Wichtige vordefinierte algebraische Datentypen in Haskell

-- data Maybe a = Nothing
--     | Just a
--   deriving (Show, Eq)

-- Aufgabe: Funktion die Zahlen zu Strings oder Nothing (statt Fehler)
numberToMaybeString :: (Eq a, Num a) => a -> Maybe String
numberToMaybeString 0 = Just "Null"
numberToMaybeString 1 = Just "Eins"
numberToMaybeString 2 = Just "Zwei"
numberToMaybeString 3 = Just "Drei"
numberToMaybeString 4 = Just "Vier"
numberToMaybeString 5 = Just "Fuenf"
numberToMaybeString _ = Nothing


-- dann gibts noch Either

-- data Either a b = Left a
--     | Right b
--   deriving (Show, Eq)

-- Aufgabe: Funktion die gerade Zahl in Left und ungerade in Right verpackt
evenOddEither :: Integer -> Either Integer Integer
evenOddEither x = if even x then Left x else Right x -- if then else hattet ihr noch nicht in der Vorlesung, kommt in der nächsten
