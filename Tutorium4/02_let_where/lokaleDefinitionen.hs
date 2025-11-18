-- bei dem letzten übungsblatt hatten wir die aufgabe boolesche ausdrücke als haskellfunktion zu schreiben
-- z.b.Applicative

foo :: Bool -> Bool -> Bool -> Bool
foo x y z = (x || y) && (x || z ) && (y || z)

-- das können wir jetzt auslagern, dass übersichtlicher wird



-- aber so sind unsere minterme auch global sichtbar, was wir vielleicht nicht wollen
-- die lösung: lokale definitionen mit let oder where (machen eigentlich das gleiche)














-- angenommen wir haben die Aufgabe einen Algorithmus zu schreiben, der uns bei einer eingabe von 3 zahlen sagen soll gleich 2 ist
checkIfTwo :: Int -> Int -> Int -> String
checkIfTwo x y z =
  (if x == 2 then "x is 2 " else "x is not 2")
    ++ ", "
    ++ (if y == 2 then "y is 2 " else "y is not 2")
    ++ ", "
    ++ (if z == 2 then "z is 2 " else "z is not 2")

-- für die übersichtlichkeit wollen wir nun unsere Überprüfung auf eine eigene Methode auslagern
-- dafür machen wir uns eine eigene Methode isTwo

checkIfTwo' :: Int -> Int -> Int -> String
checkIfTwo' x y z = helper x ++ ", " ++ helper y ++ ", " ++ helper z

helper :: Int -> String
helper x = if x == 2 then "x is 2" else "x is not 2"

-- nun haben wir aber das Problem, dass die helper methode global sichtbar ist. Das wollen wir vielleicht nicht, weil wir diese ja nur für unsere CheckIfTwo Methode brachen
-- die Lösung: lokale Definitionen mit let oder where (machen eigentlich das gleiche)

checkIfTwo'' :: Int -> Int -> Int -> String
checkIfTwo'' x y z = let helper' x = if x == 2 then "x is 2" else "x is not 2" in
    helper' x ++ ", " ++ helper' y ++ ", " ++ helper' z


-- oder alternativ mit where (kommt nicht vor der verwendung sondern dannach)

checkIfTwo''' :: Int -> Int -> Int -> String
checkIfTwo''' x y z = helper' x ++ ", " ++ helper' y ++ ", " ++ helper' z 
    where helper' x = if x == 2 then "x is 2" else "x is not 2"
