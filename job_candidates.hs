import qualified Data.Map as Map

data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)

data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate
    { candidateId :: Int
    , codeReview :: Grade
    , cultureFit :: Grade
    , education :: Degree } deriving Show

viable :: Candidate -> Bool
viable candidate = all (== True) tests
    where tests = [passedCoding,passedCultureFit,educationMin]
          passedCoding = codeReview candidate > B
          passedCultureFit = cultureFit candidate > C
          educationMin = education candidate >= MS

candidate1 = Candidate 1 A A PhD
candidate2 = Candidate 2 A A BA
candidate3 = Candidate 3 A B MS

candidates = [candidate1,candidate2,candidate3]
candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList (zip [1..3] candidates)

readInt :: IO Int
readInt = getLine >>= (return . read)

readGrade :: IO Grade
readGrade = getLine >>= (return . read)

readGrade2 :: IO Grade
readGrade2 = do
    val <- getLine
    return (read val)

readDegree :: IO Degree
readDegree = getLine >>= (return . read)

readCandidate :: IO Candidate
readCandidate = do
    putStrLn "enter id:"
    cId <- readInt
    putStrLn "enter code grade:"
    codeGrade <- readGrade2
    putStrLn "enter culture fit grade:"
    cultureGrade <- readGrade2
    putStrLn "enter education:"
    degree <- readDegree
    return (Candidate { candidateId = cId
                      , codeReview = codeGrade
                      , cultureFit = cultureGrade
                      , education = degree })

assessCandidateIO :: IO String
assessCandidateIO = do
    candidate <- readCandidate
    return (if viable candidate then "passed" else "failed")

assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cId = do
    candidate <- Map.lookup cId candidateDB
    return (if viable candidate then "passed" else "failed")

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
    candidate <- candidates
    return (if viable candidate then "passed" else "failed")

failPassOrElse :: Maybe String -> String
failPassOrElse Nothing = "error id not found"
failPassOrElse (Just result) = result

assessCandidates :: [Candidate] -> [String]
assessCandidates candidates = map (\x -> if x then "passed" else "failed") passed
    where passed = map viable candidates

assessCandidate :: Monad m => m Candidate -> m String
assessCandidate candidates = do
    candidate <- candidates
    return (if viable candidate then "passed" else "failed")
