main = print "Hello World!"

fak :: Int -> Int
fak 0 = 1
fak n = n * fak (n - 1)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)


oddEven :: [Int] -> [Either Int Int]
oddEven  = map (\x -> if even x then Left x else Right x)

maybeFilter :: [Int] -> [Maybe Int]
maybeFilter = map (\x -> if even x then Just x else Nothing)
