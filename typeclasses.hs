class Descibable a where
    describe :: a -> String

data Icecream = Chocolate | Vanila
    deriving (Show, Ord, Eq)

-- cycleSucc :: (Bounded a, Enum a, ? a) => a -> a
-- cycleSucc n = 

data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Enum)

instance Show SixSidedDie where
    show S1 = "one"
    show S2 = "two"
    show S3 = "three"
    show S4 = "four"
    show S5 = "five"
    show S6 = "six"

instance Eq SixSidedDie where
    (==) S6 S6 = True
    (==) S5 S5 = True
    (==) S4 S4 = True
    (==) S3 S3 = True
    (==) S2 S2 = True
    (==) S1 S1 = True
    (==) _ _ = False

data Test1 = AA | ZZ deriving (Eq, Ord)
data Test2 = ZZZ | AAA deriving (Eq, Ord)

-- instance Enum SixSidedDie where
--     toEnum 0 = S1
--     toEnum 1 = S2
--     toEnum 2 = S3
--     toEnum 3 = S4
--     toEnum 4 = S5
--     toEnum 5 = S6
--     toEnum _ = error "No such value"
--     fromEnum S1 = 0
--     fromEnum S2 = 1
--     fromEnum S3 = 2
--     fromEnum S4 = 3
--     fromEnum S5 = 4
--     fromEnum S6 = 5

-- type Name = (String,String)
-- names :: [Name]
-- names = [ ("Emil","Cioran")
--         , ("Eugene", "Thacker")
--         , ("Friedrich","Nietzsche") ]

-- instance Ord Name where
--     compare (f1,l1) (f2,l2) = compare (l1,f1) (l2,f2)

-- data Name = Name (String, String) deriving (Show, Eq)
newtype Name = Name (String, String) deriving (Show, Eq)

instance Ord Name where
    compare (Name (f1,l1)) (Name (f2,l2)) = compare (l1,f1) (l2,f2)

names :: [Name]
names = [ Name ("Emil","Cioran")
        , Name ("Eugene","Thacker")
        , Name ("Friedrich","Nietzsche") ]

class Die a where
    roll :: a -> String

-- data FiveSidedDie = S1 | S2 | S3 | S4 | S5

data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show,Enum,Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
    where halfAlphabet = alphabetSize `div` 2
          offset = fromEnum c + halfAlphabet
          rotation = offset `mod` alphabetSize

rotChar :: Char -> Char
rotChar charToEncrypt = rotN sizeOfAlphabet charToEncrypt
    where sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

data ThreeLetterAphabet = Alpha
                        | Beta
                        | Kappa deriving (Show,Enum,Bounded)

threeLetterEncoder :: [ThreeLetterAphabet] -> [ThreeLetterAphabet]
threeLetterEncoder vals = map rot3l vals
    where rot3l = rotN alphaSize
          alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAphabet)

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
    where rotation = offset `mod` n
          offset = if even n
                   then fromEnum c + halfN
                   else 1 + fromEnum c + halfN
          halfN = n `div` 2

threeLetterDecoder :: [ThreeLetterAphabet] -> [ThreeLetterAphabet]
threeLetterDecoder vals = map rot3ldecoder vals
    where rot3ldecoder = rotNdecoder alphaSize
          alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAphabet)

rotEncoder :: String -> String
rotEncoder text = map rotChar text
    where rotChar = rotN alphaSize
          alphaSize = 1 + fromEnum (maxBound :: Char)

rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text
    where rotCharDecoder = rotNdecoder alphaSize
          alphaSize = 1 + fromEnum (maxBound :: Char)

xorBool :: Bool -> Bool -> Bool
xorBool x y = (x || y) && (not (x && y))

xorPair :: (Bool,Bool) -> Bool
xorPair (x,y) = xorBool x y

type Bits = [Bool]
xor :: Bits -> Bits -> Bits
xor xs ys = map xorPair (zip xs ys)

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if remainder == 0
               then False : intToBits' nextVal
               else True : intToBits' nextVal
    where nextVal = n `div` 2
          remainder = n `mod` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
    where leadingFalses = take missingBits (cycle [False])
          missingBits = maxBits - (length reversedBits)
          reversedBits = reverse (intToBits' n)

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2^(snd x)) trueLocations)
    where trueLocations = filter (\x -> fst x == True) (zip bits indices)
          indices = [size-1,size-2 .. 0]
          size = length bits

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits) :: Char

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plainText = map (\pair ->
                                (fst pair) `xor` (snd pair))
                                (zip padBits plainTextBits)
    where padBits = map charToBits pad
          plainTextBits = map charToBits plainText

applyOTP :: String -> String -> String
applyOTP pad plainText = map bitsToChar bitList
    where bitList = applyOTP' pad plainText

-- encoderDecoder :: String -> String
-- encoderDecoder = applyOTP myPad

class Cipher a where
    encode :: a -> String -> String
    decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
    encode Rot text = rotEncoder text
    decode Rot text = rotDecoder text

data OneTimePad = OTP String

instance Cipher OneTimePad where
    encode (OTP pad) text = applyOTP pad text
    decode (OTP pad) text = applyOTP pad text

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a * seed + b) `mod` maxNumber

examplePRNG :: Int -> Int
examplePRNG = prng 1337 7 100

data StreamCipher = StreamCipher String

instance Cipher StreamCipher where
    encode (StreamCipher pad) text = applyOTP pad text
    decode (StreamCipher pad) text = applyOTP pad text

myStreamChiper :: StreamCipher
myStreamChiper = StreamCipher (map
    (\x -> toEnum (examplePRNG x) :: Char)
    (cycle [minBound .. maxBound] :: [Int]))
