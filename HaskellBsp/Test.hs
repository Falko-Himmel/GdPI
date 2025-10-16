list = [1,2,3,4,5,6,7,8,9,0]

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

filterEven :: [Integer] -> [Integer]
filterEven [] = []
filterEven (x:xs) = filter (\x -> x `mod` 2 == 0) xs

myDivision :: Integer -> Integer -> Maybe Double
myDivision a 0 = Nothing
myDivision a b = Just (fromIntegral a / fromIntegral b)

data NestedList a = Elem a | List[NestedList a]

flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem x) = [x]
flatten (List (x:xs)) =  flatten x ++ flatten (List xs)

data BinTree a = Empty | Node a (BinTree a) (BinTree a)
    deriving Show

treeSearch :: Eq a => BinTree a -> a -> Bool
treeSearch Empty _ = False
treeSearch (Node x left right) a
    | x == a = True
    | otherwise = treeSearch left  a || treeSearch right a

smartAddMabe :: Ord a => Maybe (BinTree a) -> a -> Maybe (BinTree a)
smartAddMabe Nothing x = Just (Node x Empty Empty)
smartAddMabe (Just tree) y = treeSmartAdd tree y

treeSmartAdd :: Ord a => BinTree a -> a -> Maybe(BinTree a)
treeSmartAdd Empty x = Just (Node x Empty Empty)
treeSmartAdd (Node x left right) a 
    | a < x && isEmpty left = Just (Node x (Node a Empty Empty) right)
    | a < x && isEmpty right = Just (Node x left (Node a Empty Empty))
    | otherwise =
        case treeSmartAdd left a of
                Just newLeft -> Just (Node x newLeft right)
                Nothing ->
                    case treeSmartAdd right a of
                        Just newRight -> Just (Node x left newRight)
                        Nothing -> Nothing

isEmpty :: BinTree a -> Bool
isEmpty Empty = True
isEmpty _     = False

testTree :: BinTree Int
testTree = Node 1
             (Node 2
                 (Node 4 Empty Empty)
                 (Node 5 Empty Empty)
             )
             (Node 3
                 Empty
                 (Node 6 Empty Empty)
             )