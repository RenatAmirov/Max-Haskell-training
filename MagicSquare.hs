

(//) :: Eq a => [a] -> a -> [a]
[] // y = []
(x:xs) // y = if (x == y)
                  then xs
                  else x:(xs // y)
 
(///) :: Eq a => [a] -> [a] -> [a]
s /// [] = s
s /// (x:xs) = (s // x) /// xs

{-
ms_3 :: [Integer]
ms_3 = [[x1, x2, x3, 
         y1, y2, y3, 
         z1, z2, z3] |
              x1 <- [1..9],
              x2 <- [1..9] /// [x1],
              x3 <- [1..9] /// [x1, x2],
              y1 <- [1..9] /// [x1, x2, x3],
              y2 <- [1..9] /// [x1, x2, x3, y1],
              y3 <- [1..9] /// [x1, x2, x3, y1, y2],
              z1 <- [1..9] /// [x1, x2, x3, y1, y2, y3],
              z2 <- [1..9] /// [x1, x2, x3, y1, y2, y3, z1],
              z3 <- [1..9] /// [x1, x2, x3, y1, y2, y3, z1, z2],
              x1 + x2 + x3 == y1 + y2 + y3,
              x1 + x2 + x3 == z1 + z2 + z3,
              x1 + x2 + x3 == x1 + y1 + z1,
              x1 + x2 + x3 == x2 + y2 + z2,
              x1 + x2 + x3 == x3 + y3 + z3,
              x1 + x2 + x3 == x1 + y2 + z3,
              x1 + x2 + x3 == x3 + y2 + z1]
-}

-------------В ЛОБ-----------------------------------------------------------

-- magicSum :: Fractional a => a -> a
-- magicSum n = n * (n^2 + 1) / 2

magicSum :: Integral a => a -> a
magicSum n = sum [1..(n^2)] `div` n

{-
ms_3' :: [[Integer]]
ms_3' = [[x1, x2, x3,
          y1, y2, y3,
          z1, z2, z3] |
                x1 <- [1..9],
                x2 <- [1..9] /// [x1],
                x3 <- [1..9] /// [x1, x2],
                x1 + x2 + x3 == ms,
                y1 <- [1..9] /// [x1, x2, x3],
                z1 <- [1..9] /// [x1, x2, x3, y1],
                x1 + y1 + z1 == ms,
                y2 <- [1..9] /// [x1, x2, x3, y1, z1],
                x3 + y2 + z1 == ms,
                y3 <- [1..9] /// [x1, x2, x3, y1, z1, y2],
                y1 + y2 + y3 == ms,
                z2 <- [1..9] /// [x1, x2, x3, y1, z1, y2, y3],
                x2 + y2 + z2 == ms,
                z3 <- [1..9] /// [x1, x2, x3, y1, z1, y2, y3, z2],
                z1 + z2 + z3 == ms,
                x3 + y3 + z3 == ms,
                x1 + y2 + z3 == ms]
              where ms = magicSum 3
-}
-----------ПЕРЕСТАНОВКИ-------------------------------------------------------------------

isMagic :: Int -> [Int] -> Bool
isMagic n ms = if (n^2 /= length ms)
                   then False
                   else (testH s n ms) &&
                        (testV s n ms) &&
                        (testD s n ms)
                   where s = magicSum n


permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations l = [x:ps |
    x <- l,
    ps <- (permutations (l // x))]


testH :: Int -> Int -> [Int] -> Bool
testH _ _ [] = True
testH s n ms = (sum (take n ms) == s) &&
               (testH s n (drop n ms))


testV :: Int -> Int -> [Int] -> Bool
testV s n ms = testV' s n n ms
    where testV' _ 0 _ _ = True
          testV' s a n ms =
            (sum [ms !! ((a - 1) + (j * n)) | j <- [0..(n - 1)]] == s) &&
            (testV' s (a - 1) n ms)

testD :: Int -> Int -> [Int] -> Bool
testD s n ms =
    (sum [ms !! (i * (n + 1)) |
             i <- [0..(n - 1)]] == s) &&
             (sum [ms !! ((i + 1) * (n - 1)) |
                 i <- [0..(n - 1)]] == s)


getMagicSquares :: Int -> [[Int]]
getMagicSquares n = [ms | ms <- permutations [1..(n^2)], isMagic n ms]

---------РАЗМЕЩЕНИЯ-----------------------------------------------------
{-
constructSquares :: Integral a => a -> [a] -> [[a]]
constructSquares _ [] = [[]]
constructSquares n l = [a ++ as | a <- (arrangements n l), sum a == s, as <- (constructSquares n (l /// a))]
    where s = magicSum n

getMagicSquares' :: Int -> [[Int]]
getMagicSquares' n = [ms | ms <- constructSquares n [1..(n^2)], isMagic n ms ]

-}





