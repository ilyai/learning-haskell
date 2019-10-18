
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myTake 0 _ = []
myTake _ [] = []
myTake n (x:xs) = x : myTake (n-1) xs

myCycle (first:rest) = first : myCycle (rest ++ [first])

ackermann 0 n = n+1
ackermann m 0 = ackermann (m-1) 1
ackermann m n = ackermann (m-1) (ackermann m (n-1))

collatz 1 = 1
collatz n
    | even n = 1 + collatz (n `div` 2)
    | otherwise = 1 + collatz (n * 3 + 1)

myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

add3ToAll [] = []
add3ToAll (x:xs) = (3+x) : add3ToAll xs

myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

myFilter test [] = []
myFilter test (x:xs) = if test x
                       then x : myFilter test xs
                       else myFilter test xs

remove test [] = []
remove test (x:xs) = if test x
                     then remove test xs
                     else x : remove test xs

myProduct xs = foldl (*) 1 xs

concatAll xs = foldl (++) "" xs

sumOfSquares xs = foldl (+) 0 (map (^2) xs)

myReverse2 xs = foldl (\x y -> y : x) [] xs

myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f (f init x) xs

myFoldr f init [] = init
myFoldr f init (x:xs) = f x (myFoldr f init xs)

-- isPalindrome s = normalizedString == reverse normalizedString
--   where normalizedString = map toLower (filter (\c -> c /= ' ') s)

harmonic 1 = 1
harmonic n = (1 / n) + harmonic (n-1)

harmonic2 n = foldl (\y x -> (1 / x) + y) 1 [2 .. n]
