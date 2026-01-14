-- ==================================================================================
--
--                               _-_ 
--                            /~~   ~~\
--                         /~~         ~~\
--                        {               }
--                         \  _-     -_  /
--                           ~  \\ //  ~
--                               ||
--                               ||
--                             ======
-- ==================================================================================





-- Gegeben sei der folgende algebraische Datentyp zur Modellierung eines Binärbaumes zur Speicherung
-- beliebiger Werte:

data BinTree a = Node a (BinTree a) (BinTree a)
    | Empty
    deriving (Eq, Show)

-- sowie eine dazugehörige fold-Funktion

foldTree :: (a -> b -> b -> b) -> b -> BinTree a -> b
foldTree fNode fEmpty = fold
    where
        fold (Node a l r) = fNode a (fold l) (fold r)
        fold Empty = fEmpty


-- Beispielbaum zur Veranschaulichung:
bsp :: BinTree Int
bsp = Node 1 (Node 2 Empty (Node 3 Empty Empty)) (
        Node 4 Empty Empty
    )   

--        1
--       / \
--      2   4
--       \
--        3 

-- Altklausuraufgabe 2024_2:

-- Aufgabe 4a)
-- Implementieren Sie eine Funktion countInnerNodes die die Anzahl der im Baum vorhandenen
-- inneren Knoten (d.h. solche mit mindestens einem Kindknoten ungleich Empty) zählt.
-- Beispiel:
-- • countInnerNodes bsp ≡ 2

countInnerNodes :: BinTree a -> Int
countInnerNodes Empty = 0
countInnerNodes (Node _ Empty Empty) = 0
countInnerNodes (Node _ l r) = 1 + countInnerNodes l + countInnerNodes r
















-- rekursive Lösung:
countInnerNodes' :: BinTree a -> Int
countInnerNodes' Empty = 0 -- Basisfall, ein leerer Baum hat keine inneren Knoten
countInnerNodes' (Node _ Empty Empty) = 0 -- ist an einem Knoten keine Kind, so ist dies ein Blatt und damit kein innerer Knoten => 0, da nur innere Knoten zählen
countInnerNodes' (Node _ l r) = 1 + countInnerNodes' l + countInnerNodes' r -- sonst für jeden inneren Knoten eins aufaddieren und rekursiv die linken und rechten Teilbäume untersuchen





-- Aufgabe 4b) ziemlich ähnlich wie die aufgabe davor:
-- Implementieren Sie die Funktion listInnerNodes die die Werte aller inneren Knoten im überge-
-- benen Baum auflistet.
-- Beispiele:
-- • listInnerNodes (Node 42 Empty Empty) → []
-- • listInnerNodes bsp ≡ [6,3]


listInnerNodes :: BinTree a -> [a]
listInnerNodes Empty = [] -- Basisfall, ein leerer Baum hat keine inneren Knoten
listInnerNodes (Node _ Empty Empty) = [] -- ist an einem Knoten keine Kind, so ist dies ein Blatt und damit kein innerer Knoten => 0, da nur innere Knoten zählen
listInnerNodes (Node a l r) = a : ( listInnerNodes l ++ listInnerNodes r)












-- die rekursive Lösung ist hier fast gleich wie oben:
listInnerNodes' :: BinTree a -> [a]
listInnerNodes' Empty = [] -- Basisfall, ein leerer Baum hat keine inneren Knoten
listInnerNodes' (Node _ Empty Empty) = [] -- ist an einem Knoten keine Kind, so ist dies ein Blatt und damit kein innerer Knoten => leere Liste
listInnerNodes' (Node a l r) = a : (listInnerNodes' l ++ listInnerNodes' r) -- sonst für jeden inneren Knoten den Wert auflisten und rekursiv die linken und rechten Teilbäume untersuchen






-- Aufgabe mit einem fold auf einem Baum
-- --  Aufgabe 4c)
-- Implementieren Sie, unter Verwendung der Funktion foldTree, eine Funktion productTree
-- welche das Produkt aller im Baum abgespeicherten Werte berechnet. Vervollständigen Sie auch
-- den Typen der Funktion!
-- Beispiel:
-- • productTree bsp ≡ 630


productTree :: Num a => BinTree a -> a
productTree = foldTree (\a l r -> a*l*r) 1


















-- rekursive Lösung:
productTree' :: Num a => BinTree a -> a
productTree' Empty = 1 -- Basisfall, das neutrale Element der Multiplikation
productTree' (Node a l r) = a * productTree' l * productTree' r -- Wert am Knoten mal Produkt der linken und rechten Teilbäume

-- wenn wir das vergleichen mit dem foldTree, dann sehen wir, dass:
-- fEmpty = 1
-- fNode a leftProd rightProd = a * leftProd * rightProd (multiplikation)

-- foldTree :: (a -> b -> b -> b) -> b -> BinTree a -> b
-- foldTree fNode fEmpty = fold
--     where
--         fold (Node a l r) = fNode a (fold l) (fold r)
--         fold Empty = fEmpty


-- daraus erhalten wir lösung mit foldTree durch einsetzen:
productTree'' :: Num a => BinTree a -> a
productTree'' = foldTree (\a leftProd rightProd -> a * leftProd * rightProd) 1