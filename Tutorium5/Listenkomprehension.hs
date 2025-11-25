--  Wir haben bei der Listenkomprehension eine kompakte Schreibweise um Listen zu erzeugen.
-- Dabei besteht eine Listenkomprehension aus drei Teilen:
-- 1. Ein Ausdruck, der angibt, wie die Elemente der Liste aussehen sollen
-- 2. Eine oder mehrere Generatoren, die angeben, aus welchen Listen die Elemente stammen
-- 3. Optionale Filter, die angeben, welche Elemente ausgewählt werden sollen

-- Beispiel: Wir wollen eine Liste aller Quadratzahlen von 1 bis 10 erstellen
squares :: [Int]
squares = [x^2 | x <- [1..10]]
-- Hier ist der Ausdruck x^2, der Generator x <- [1..10] und es gibt keinen Filter

-- wollen wir jetzt alle geraden Quadratzahlen von 1 bis 10 erstellen
evenSquares :: [Int]
evenSquares = [x^2 | x <- [1..10], even x]
-- Hier ist der Ausdruck x^2, der Generator x <- [1..10] und der Filter even x
-- hier werden dann die Zahlen von 1..10 genommen, geprüft ob sie gerade sind und wenn ja wird das Quadrat in die Liste aufgenommen