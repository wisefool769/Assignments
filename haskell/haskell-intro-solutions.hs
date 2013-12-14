module HaskellIntro where

myZip [] _ = []
myZip _ [] = []
myZip (x1:xs1) (x2:xs2) = (x1, x2):zip xs1 xs2

qsort [] = []
qsort (pivot:xs) =
    let smallerSorted = qsort [a | a <- xs, a <= pivot]
        biggerSorted = qsort [a | a <- xs, a > pivot]
    in  smallerSorted ++ [pivot] ++ biggerSorted

fib n = fibHelper (0, 1) n
    where fibHelper (a, b) 0 = a
          fibHelper (a, b) 1 = b
          fibHelper (a, b) n = fibHelper (b, a + b) (n - 1)

fibs = 0 : 1 : next fibs
    where next (a : b : xs) = (a + b) : next (b:xs)

euler6 =
    let squareOfSum = (sum [1 .. 100]) ^ 2
        sumOfSquare = sum [x ^ 2 | x <- [1 .. 100]]
    in squareOfSum - sumOfSquare

euler9 = head [a * b * c | a <- [1..1000], b <- [1..a], let c = 1000 - a - b,
               c > 0, a^2 + b^2 == c^2]
