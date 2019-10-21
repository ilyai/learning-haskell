-- import Data.Semigroup
import Data.Monoid

sort :: [a] -> [a]
sort a = a

myLast :: [a] -> a
myLast = head . reverse

myMin :: Ord a => [a] -> a
myMin = head . sort 

myMax :: Ord a => [a] -> a
myMax = head . reverse . sort

myAll :: (a -> Bool) -> [a] -> Bool
myAll test = (foldr (&&) True) . (map test)

myAny :: (a -> Bool) -> [a] -> Bool
myAny test = (foldr (||) False) . (map test)

-- instance Semigroup Integer where
--     (<>) x y = x + y

data Color = Transparent
           | Red
           | Yellow
           | Blue
           | Green
           | Purple
           | Orange
           | Brown deriving (Show,Eq)

-- instance Semigroup Color where
--     (<>) Red Blue = Purple
--     (<>) Blue Red = Purple
--     (<>) a b | a == b = a
--              | all (`elem` [Red,Blue,Purple]) [a,b] = Purple
--                otherwise = Brown

howMuch :: Int -> String
howMuch n | n > 10 = "a whole bunch"
          | n > 0 = "not much"
          | otherwise = "we're in debt!"

--
-- Monoid
--
-- Laws:
-- [] ++ [1] = [1]
-- [1] ++ [] = [1]
-- [1] ++ ([2] ++ [3]) = ([1] ++ [2]) ++ [3]
-- mconcat = foldr mappend mempty
--

type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
    where normalizedProbs = map (\x -> x/totalProbs) probs
          totalProbs = sum probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event,"|", show prob,"\n"]

instance Show PTable where
    show (PTable events probs) = mconcat pairs
        where pairs = zipWith showPair events probs

myPTable = createPTable ["heads","tails"] [0.5,0.5]

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
    where newL1 = mconcat repeatedL1
          repeatedL1 = map (take (length l2) . repeat) l1
          cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine combiner e1 e2
    where combiner = (\x y -> mconcat [x,"-",y])

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartCombine (*) p1 p2

-- instance Semigroup PTable where
--     (<>) ptable1 (PTable [] []) = ptable1
--     (<>) (PTable [] []) ptable2 = ptable2
--     (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
--         where newEvents = combineEvents e1 e2
--               newProbs = combineProbs p1 p2

instance Monoid PTable where
    mempty = PTable [] []
    mappend ptable1 (PTable [] []) = ptable1
    mappend (PTable [] []) ptable2 = ptable2
    mappend (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
        where newEvents = combineEvents e1 e2
              newProbs = combineProbs p1 p2

spinner :: PTable
spinner = createPTable ["red","blue","green"] [0.1,0.2,0.7]

instance Monoid Color where
    mempty = Transparent
    mappend Red Blue = Purple
    mappend Blue Red = Purple
    mappend Yellow Blue = Green
    mappend Blue Yellow = Green
    mappend Yellow Red = Orange
    mappend Red Yellow = Orange
    mappend Transparent b = b
    mappend a Transparent = a
    mappend a b | a == b = a
                | all (`elem` [Red,Blue,Purple]) [a,b] = Purple
                | all (`elem` [Blue,Yellow,Green]) [a,b] = Green
                | all (`elem` [Red,Yellow,Orange]) [a,b] = Orange
                | otherwise = Brown
