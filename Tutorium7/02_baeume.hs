--            &&& &&  & &&
--        && &\/&\|& ()|/ @, &&
--        &\/(/&/&||/& /_/)_&/_&
--     &() &\/&|()|/&\/ '%" & ()
--    &_\_&&_\ |& |&&/&__%_/_& &&
--  &&   && & &| &| /& & % ()& /&&
--   ()&_---()&\&\|&&-&&--%---()~
--       &&     \|||
--               |||
--               |||
--               |||
--           , -=-~-=- ,

-- ein baum ist in haskell definiert als:

data BinTree a = Nil | Node a (BinTree a) (BinTree a)
  deriving (Show, Eq)

-- wie man hier schon sieht ist das wieder eine rekursive datenstruktur.
-- ein baum kann leer sein (Nil) oder ein knoten (Node) mit einem wert und zwei kindern (linkes und rechtes subtree).
-- diese kinder können dann wieder bäume sein oder leer (dann ist der knoten wenn beide kinder leer sind ein blatt).

-- beispiel für einen baum aufbauen:
--       8
--     /   \
--    3     10
--   / \      \
--  1   6      14
--     / \     /
--    4   7   13

exampleTree :: BinTree Int
exampleTree =
  Node
    8
    ( Node
        3
        (Node 1 Nil Nil)
        ( Node
            6
            (Node 4 Nil Nil)
            (Node 7 Nil Nil)
        )
    )
    Nil

-- Jetzt können wir einen Baum auch durchlaufen und verschiedene funktionen darauf anwenden.
-- Zum beispiel eine Funktion, die alle Werte im Baum aufsummiert.

sumTree :: (Num a) => BinTree a -> a
sumTree Nil = 0 -- wenn ein baum "leer" ist (also Nil) dann ist die summe 0
sumTree (Node v left right) = v + sumTree left + sumTree right -- sonst rechne die summe auf von dem aktuellen knoten und der summe von rechtem und linken teilbaum

-- Oder z.b. können wir jetzt auch die Anzahl der Knoten in dem Baum zählen.
countNodes :: BinTree a -> Int
countNodes Nil = 0 -- ein "leerer" baum hat 0 knoten
countNodes (Node _ left right) = 1 + countNodes left + countNodes right -- sonst zähle 1 für den aktuellen knoten und die knoten in den teilbäumen


-- vielleicht habtb ihr gemerkt das die funktionen ziemlich ähnlich aufgebaut sind.
-- das hatten wir ja auch schon bei den listen, die immer aus einem basisfall (leere liste) und einem rekursiven fall bestehen.
-- da haben wir dann auch höhere ordnungsfunktionen wie foldr benutzt um solche funktionen allgemeiner zu schreiben.
-- das können wir auch bei bäumen machen!
-- dazu müssen wir eine fold funktion für bäume schreiben:

foldTree :: (a -> b -> b -> b) -> b -> BinTree a -> b
foldTree _ z Nil = z -- basisfall: wenn der baum leer ist, gib den basiswert z zurück. hier war oben ja der basiswert 0, weil ein leerer baum 0 knoten und die summe 0 hat
foldTree f z (Node v left right) = f v (foldTree f z left) (foldTree f z right) -- sonst wie wie wir oben gesehen haben: wende die funktion f an auf den wert v und die ergebnisse der rekursiven aufrufe auf die teilbäume


-- jetzt können wir die sumTree und countNodes funktionen mit foldTree schreiben:
sumTree' :: (Num a) => BinTree a -> a
sumTree' = foldTree (\v l r -> v + l + r) 0

countNodes' :: BinTree a -> Int
countNodes' = foldTree (\_ l r -> 1 + l + r) 0