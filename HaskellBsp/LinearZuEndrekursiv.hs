-- ============================================
-- BEWEIS: Jede linear rekursive Funktion kann 
-- in eine endrekursive Funktion umgewandelt werden
-- ============================================

-- BEISPIEL 1: Summe
-- ==================

-- Linear, NICHT endrekursiv:
sumLinear :: [Int] -> Int
sumLinear [] = 0
sumLinear (x:xs) = x + sumLinear xs
--                 ^^^ Operation NACH Rekursion

-- Umgewandelt: Linear UND endrekursiv:
sumTail :: [Int] -> Int
sumTail xs = go xs 0  -- Akkumulator startet bei 0
  where
    go [] acc = acc
    go (y:ys) acc = go ys (acc + y)  -- Addition VOR Rekursion!


-- BEISPIEL 2: Fakultät
-- =====================

-- Linear, NICHT endrekursiv:
facLinear :: Int -> Int
facLinear 0 = 1
facLinear n = n * facLinear (n-1)
--            ^^^ Multiplikation NACH Rekursion

-- Umgewandelt: Linear UND endrekursiv:
facTail :: Int -> Int
facTail n = go n 1  -- Akkumulator startet bei 1
  where
    go 0 acc = acc
    go n acc = go (n-1) (n * acc)  -- Multiplikation VOR Rekursion!


-- BEISPIEL 3: Länge einer Liste
-- ==============================

-- Linear, NICHT endrekursiv:
lengthLinear :: [a] -> Int
lengthLinear [] = 0
lengthLinear (_:xs) = 1 + lengthLinear xs
--                    ^^^ Addition NACH Rekursion

-- Umgewandelt: Linear UND endrekursiv:
lengthTail :: [a] -> Int
lengthTail xs = go xs 0
  where
    go [] acc = acc
    go (_:ys) acc = go ys (acc + 1)  -- Addition VOR Rekursion!


-- BEISPIEL 4: Liste umkehren
-- ===========================

-- Linear, NICHT endrekursiv:
reverseLinear :: [a] -> [a]
reverseLinear [] = []
reverseLinear (x:xs) = reverseLinear xs ++ [x]
--                     ^^^ Konkatenation NACH Rekursion

-- Umgewandelt: Linear UND endrekursiv:
reverseTail :: [a] -> [a]
reverseTail xs = go xs []  -- Akkumulator ist leere Liste
  where
    go [] acc = acc
    go (y:ys) acc = go ys (y:acc)  -- Konkatenation VOR Rekursion!


-- BEISPIEL 5: Maximum einer Liste
-- ================================

-- Linear, NICHT endrekursiv:
maxLinear :: Ord a => [a] -> a
maxLinear [x] = x
maxLinear (x:xs) = max x (maxLinear xs)
--                 ^^^ Vergleich NACH Rekursion

-- Umgewandelt: Linear UND endrekursiv:
maxTail :: Ord a => [a] -> a
maxTail (x:xs) = go xs x  -- Akkumulator startet mit erstem Element
  where
    go [] acc = acc
    go (y:ys) acc = go ys (max acc y)  -- Vergleich VOR Rekursion!


-- BEISPIEL 6: Filter
-- ===================

-- Linear, NICHT endrekursiv:
filterLinear :: (a -> Bool) -> [a] -> [a]
filterLinear _ [] = []
filterLinear p (x:xs)
  | p x       = x : filterLinear p xs
  | otherwise = filterLinear p xs
--              ^^^ Cons-Operation NACH Rekursion

-- Umgewandelt: Linear UND endrekursiv:
filterTail :: (a -> Bool) -> [a] -> [a]
filterTail p xs = go xs []
  where
    go [] acc = reverse acc  -- Muss am Ende umkehren!
    go (y:ys) acc
      | p y       = go ys (y:acc)  -- Cons VOR Rekursion
      | otherwise = go ys acc


-- ============================================
-- DAS ALLGEMEINE PATTERN
-- ============================================

{-
SCHRITT 1: Linear rekursive Funktion
-------------------------------------
f :: Input -> Output
f baseCase = baseValue
f input = operation (f recursiveInput)
           ^^^^^^^^^ Operation NACH Rekursion


SCHRITT 2: Zu endrekursiv umwandeln
------------------------------------
f :: Input -> Output
f input = go input initialAcc
  where
    go baseCase acc = acc (oder finalize acc)
    go input acc = go recursiveInput (operation acc currentValue)
                                      ^^^^^^^^^^^^ Operation VOR Rekursion!

Schlüssel:
- Füge Akkumulator hinzu
- Berechne Operation VOR dem rekursiven Aufruf
- Übergib Ergebnis als neuen Akkumulator
-}


-- ============================================
-- DETAILLIERTES BEISPIEL: Schritt für Schritt
-- ============================================

-- Gegeben: sumLinear
-- Gesucht: sumTail

-- Original:
-- sumLinear [] = 0
-- sumLinear (x:xs) = x + sumLinear xs

-- Schritt 1: Erkenne das Pattern
-- - Basisfall: 0
-- - Operation: (+)
-- - Rekursive Eingabe: xs

-- Schritt 2: Füge Hilfsfunktion mit Akkumulator hinzu
-- sumTail xs = go xs 0  -- 0 ist der Basisfall-Wert
--   where
--     go = ???

-- Schritt 3: Implementiere Basisfall
-- go [] acc = acc  -- Gib Akkumulator zurück

-- Schritt 4: Implementiere Rekursivfall
-- go (y:ys) acc = go ys (acc + y)  -- Operation VOR Rekursion!
--                       ^^^^^^^^ Neuer Akkumulator


-- ============================================
-- WARUM FUNKTIONIERT DAS IMMER?
-- ============================================

{-
Bei linearer Rekursion gibt es:
1. Genau EINEN rekursiven Aufruf
2. Eine Operation die das Ergebnis kombiniert

Der Trick:
- Statt die Operation NACH der Rekursion zu machen
- Machen wir sie VOR der Rekursion
- Und speichern das Zwischenergebnis im Akkumulator

Das ist mathematisch äquivalent, aber:
- Keine Stack-Frames nötig
- Compiler kann zu Schleife optimieren
- Konstanter Speicherverbrauch
-}


-- ============================================
-- FAZIT
-- ============================================

{-
✓ JA, jede linear rekursive Funktion kann in eine 
  endrekursive Funktion umgewandelt werden!

Methode: Akkumulator-Pattern
- Füge Akkumulator-Parameter hinzu
- Berechne Operation vor der Rekursion  
- Übergib Ergebnis als neuen Akkumulator

Vorteile:
✓ Konstanter Speicherverbrauch
✓ Keine Stack-Überläufe
✓ Tail Call Optimization möglich
✓ Oft bessere Performance

Nachteile:
✗ Manchmal weniger lesbar
✗ Manchmal muss am Ende reverse() gemacht werden
  (z.B. bei filterTail)
-}
