-- mit Hilfe unserer primitiven Datentypen wie Int, Double, Float, Char, ... 
-- können wir jetzt auch mit algebraischen Datentypen komplexe Datentypen bauen

-- Aufzählungsdatentypen (vgl Java Enumerations)
-- bei diesem Typ "zählt" man eine Menge an Werten auf, die der Typ annehmen kann

data Wochentag = Montag | Dienstag | Mittwoch | Donnerstag | Freitag | Samstag | Sonntag
  deriving (Show, Eq) -- dadurch werden Eigenschaften für Ausgabe und Vergleich automatisch durch Haskell generiert
    

data Obst = Apfel | Banane | Orange | Birne
  deriving (Show, Eq)


-- Produktdatentypen (vgl Java Klassen)
-- hier werden verschiedene Werte zu einem neuen Typ kombiniert
data Person = Person String Integer -- Person mit Name und Alter
  deriving (Show, Eq)

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

-- Aufgabe: Funktion die gerade Zahl in Left und gerade in Right verpackt
evenOddEither :: Integer -> Either Integer Integer
evenOddEither x = if even x then Left x else Right x -- if then else hattet ihr noch nicht in der Vorlesung, kommt in der nächsten
