import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef

listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray vals = do
    let end = length vals - 1
    myArray <- newArray (0,end) 0
    forM_ [0 .. end] $ \i -> do
        let val = vals !! i
        writeArray myArray i val
    return myArray

-- listToUArray :: [Int] -> UArray Int Int
-- listToUArray vals = runSTUArray $ listToSTUArray vals

listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ do
    let end = length vals - 1
    myArray <- newArray (0,end) 0
    forM_ [0 .. end] $ \i -> do
        let val = vals !! i
        writeArray myArray i val
    return myArray

swapST :: (Int,Int) -> (Int,Int)
swapST (x,y) = runST $ do
    x' <- newSTRef x
    y' <- newSTRef y
    writeSTRef x' y
    writeSTRef y' x
    xfinal <- readSTRef x'
    yfinal <- readSTRef y'
    return (xfinal,yfinal)

bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray = runSTUArray $ do
    stArray <- thaw myArray
    let end = (snd . bounds) myArray
    forM_ [1 .. end] $ \i -> do
        forM_ [0 .. (end - i)] $ \j -> do
            val <- readArray stArray j
            nextVal <- readArray stArray (j+1)
            let outOfOrder = val > nextVal
            when outOfOrder $ do
                writeArray stArray j nextVal
                writeArray stArray (j+1) val
    return stArray

replaceZeros :: UArray Int Int -> UArray Int Int
replaceZeros myArray = runSTUArray $ do
    stArray <- thaw myArray
    let end = (snd . bounds) myArray
    forM_ [0 .. end] $ \i -> do
        val <- readArray stArray i
        writeArray stArray i (if val == 0 then -1 else val)
    return stArray
