module HaskellTyping where

slice :: Int -> Int -> [a] -> [a]
slice a b = take (b - a + 1) . drop (a - 1)

encode :: (Eq a) => [a] -> [(a, Int)]
encode [] = []
encode (z:zs) = encodeHelper z 1 zs
    where encodeHelper x n [] = [(x, n)]
          encodeHelper y n (x:xs)
              | y == x = encodeHelper x (n + 1) xs
              | otherwise = (y, n) : encodeHelper x 1 xs

encode2 :: (Eq a) => [a] -> [(a, Int)]
encode2 [] = []
encode2 lst@(x:xs) =
    let (same, rest) = span (== x) lst
    in (x, length same) : encode2 rest

decode :: [(a, Int)] -> [a]
decode [] = []
decode ((x, n):xs) = (replicate n x) ++ (decode xs)

decode2 :: [(a, Int)] -> [a]
decode2 lst = concat [replicate n x | (x, n) <- lst]

dropEvery :: [a] -> Int -> [a]
dropEvery lst n = helper lst n
    where helper [] _ = []
          helper (x:xs) 1 = helper xs n
          helper (x:xs) m = x : helper xs (m - 1)

dropEvery2 :: [a] -> Int -> [a]
dropEvery2 lst n = [x | (x, i) <- zip lst [1..], i `mod` n /= 0]

rpn :: (Num a, Read a) => String -> a
rpn = head . foldl foldingFunction [] . words
    where foldingFunction (x:y:ys) "*" = (x * y):ys
          foldingFunction (x:y:ys) "+" = (x + y):ys
          foldingFunction (x:y:ys) "-" = (y - x):ys
          foldingFunction xs numberString = read numberString:xs
