-- Summe mit fold
foldSumExample :: Integer
foldSumExample = foldSum [1..5] -- [1..5] == [1, 2, 3, 4, 5]

foldSum :: Num a => [a] -> a
foldSum zs = foldr (\x xs -> x + xs) 0 zs

reducedFoldSum :: Num a => [a] -> a
reducedFoldSum = foldr (+) 0





-- MaybeFilter mit map

-- maybeFilter soll alle Daten auf die Prädikat zutrifft in Just speichern, sonst Nothing
-- bsp: 
-- maybeFilter even [1..5] soll dann [Nothing, Just 2, Nothing, Just 4, Nothing] liefern.

maybeFilterExample :: [Maybe Integer]
maybeFilterExample = maybeFilter even [1..10]

maybeFilter :: (a -> Bool) -> [a] -> [Maybe a]
maybeFilter p xs = map(\x -> if p x then Just x else Nothing) xs
