
makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number,street,town)

makeAddressLambda = (\number ->
                        (\street ->
                            (\town -> (number,street,town))))

ifEven :: (Int -> Int) -> Int -> Int
ifEven f n = if even n
             then f n
             else n

simpleInt :: Int -> Int
simpleInt n = n

-- type variables
simple :: a -> a
simple x = x

myHead :: [[a]] -> [a]
-- myHead :: [a] -> a
myHead [] = []
myHead (x:_) = x

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f (f init x) xs

