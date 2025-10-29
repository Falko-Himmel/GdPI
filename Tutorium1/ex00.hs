main :: IO ()
main = print "Hello World!!!"

ggt :: Int -> Int -> Int
ggt a b
    | a == b    = a
    | a > b     = ggt b (a - b)
    | otherwise = ggt a (b - a)