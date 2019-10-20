
-- type Document = String
-- Document -> [String]
-- [String] -> (String -> Bool) -> [String]
-- [String] -> [Integer]
-- [Integer] -> Integer

data BreakfastSide = Toast | Bescuit | Homefries | Fruit deriving Show
data BreakfastMeat = Sausage | Bacon | Ham deriving Show
data BreakfastMain = Egg | Pancake | Waffle deriving Show

-- 
-- Product types 
--

-- data AuthorName = AuthorName String String
data AuthorName = AuthorName {
    firstName :: String
  , lastName  :: String }

-- data Book Author String String Int
data Book = Book {
    author     :: Creator -- AuthorName
  , isbn       :: String
  , bookTitle  :: String
  , bookYear   :: Int
  , bookPrice  :: Double }

-- data SportsCar = SportsCar Car Spoiler

--
-- Sum types
--

type FirstName = String
type LastName = String
type MiddleName = String

data Name = Name FirstName LastName
    | NameWithMiddle FirstName MiddleName LastName
    | TwoInitialsWithLast Char Char LastName
    | FirstNameWithTwoInits FirstName Char Char
        deriving Show

data Creator = AuthorCreator Author | ArtistCreator Artist deriving Show
data Artist = Person Name | Band String deriving Show
data Author = Author Name deriving Show

hpLovecraft :: Creator
hpLovecraft = AuthorCreator
                (Author
                    (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

data VinylRecord = VinylRecord {
    artist      :: Creator
  , recordTitle :: String
  , recordYear  :: Int
  , recordPrice :: Double
  }

data StoreItem = BookItem Book
    | RecordItem VinylRecord
    | ToyItem CollectibleToy
    | PamphletItem Pamphlet

data CollectibleToy = CollectibleToy {
    name        :: String
  , description :: String
  , toyPrice    :: Double
  }

data Pamphlet = Pamphlet {
    pamphletTitle :: String
  , pamphletDescription :: String
  , pamphletContact :: String
  , pamphletPrice :: Double
  }

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem pamphlet) = pamphletPrice pamphlet

madeBy :: StoreItem -> String
madeBy (BookItem book) = show (author book)
madeBy (RecordItem record) = show (artist record)
madeBy _ = "unknown"

data Shape = Circle Double
    | Square Double
    | Rectangle Double Double

perimeter :: Shape -> Double
perimeter (Square x) = x * 4
perimeter (Rectangle x y) = (x+y) * 2

area :: Shape -> Double
area (Square x) = x * x
area (Rectangle x y) = x * y
