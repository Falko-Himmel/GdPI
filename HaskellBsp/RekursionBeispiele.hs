-- ============================================
-- BEISPIELE: Linear vs. Endrekursiv
-- ============================================

-- 1) LINEAR REKURSIV, aber NICHT ENDREKURSIV
-- ------------------------------------------
sumLinear :: [Int] -> Int
sumLinear [] = 0
sumLinear (x:xs) = x + sumLinear xs
-- Problem: Nach dem rekursiven Aufruf muss noch addiert werden!
-- sumLinear [1,2,3] = 1 + (2 + (3 + 0))  <- Addition NACH Rekursion

-- Ablauf:
-- sumLinear [1,2,3]
-- = 1 + sumLinear [2,3]
-- = 1 + (2 + sumLinear [3])
-- = 1 + (2 + (3 + sumLinear []))
-- = 1 + (2 + (3 + 0))
-- = 6  <- Erst jetzt wird berechnet!


-- 2) LINEAR REKURSIV UND ENDREKURSIV ✓
-- -------------------------------------
sumTail :: [Int] -> Int
sumTail xs = go xs 0
  where
    go [] acc = acc
    go (y:ys) acc = go ys (acc + y)  -- Letzter Schritt ist der rekursive Aufruf!

-- Ablauf:
-- sumTail [1,2,3]
-- = go [1,2,3] 0
-- = go [2,3] 1    <- Schon berechnet!
-- = go [3] 3      <- Schon berechnet!
-- = go [] 6       <- Schon berechnet!
-- = 6


-- 3) NICHT LINEAR (mehrere rekursive Aufrufe) und NICHT ENDREKURSIV
-- ------------------------------------------------------------------
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)  -- ZWEI Aufrufe!
-- Problem: Zwei rekursive Aufrufe -> NICHT linear
-- Problem: Addition nach Rekursion -> NICHT endrekursiv


-- 4) NICHT LINEAR, aber ANNÄHERND ENDREKURSIV (mit Akkumulatoren)
-- ----------------------------------------------------------------
fibonacciTail :: Int -> Int
fibonacciTail n = go n 0 1
  where
    go 0 a b = a
    go n a b = go (n-1) b (a+b)  -- Nur EIN Aufruf, endrekursiv!
-- Das ist LINEAR (ein Aufruf) UND ENDREKURSIV


-- ============================================
-- ZUSAMMENFASSUNG MIT BEISPIELEN
-- ============================================

-- Linear rekursiv, NICHT endrekursiv:
lengthLinear :: [a] -> Int
lengthLinear [] = 0
lengthLinear (_:xs) = 1 + lengthLinear xs  -- + NACH Rekursion

-- Linear rekursiv UND endrekursiv:
lengthTail :: [a] -> Int
lengthTail xs = go xs 0
  where
    go [] acc = acc
    go (_:ys) acc = go ys (acc + 1)  -- Rekursion ist letzter Schritt

-- NICHT linear (Baumrekursion), NICHT endrekursiv:
treeSum :: BinTree Int -> Int
treeSum Empty = 0
treeSum (Node x left right) = x + treeSum left + treeSum right  -- ZWEI Aufrufe!

data BinTree a = Empty | Node a (BinTree a) (BinTree a)

-- ============================================
-- GEGENBEISPIEL ZUR AUSSAGE
-- ============================================

-- Diese Funktion ist LINEAR REKURSIV (nur ein Aufruf)
-- aber NICHT ENDREKURSIV (Multiplikation nach Rekursion)
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)  -- Multiplikation NACH Rekursion!

-- Diese Funktion ist LINEAR REKURSIV UND ENDREKURSIV
factorialTail :: Int -> Int
factorialTail n = go n 1
  where
    go 0 acc = acc
    go n acc = go (n-1) (n * acc)  -- Rekursion ist letzter Schritt

-- ============================================
-- FAZIT:
-- ============================================
-- ✗ Jede endrekursive Funktion ist linear rekursiv: JA (stimmt!)
-- ✗ Jede lineare Funktion ist endrekursiv: NEIN! (siehe factorial)
-- 
-- Endrekursiv ⊂ Linear rekursiv
-- (Endrekursiv ist eine Teilmenge von linear rekursiv)
