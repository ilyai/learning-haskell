import Control.Applicative
import Control.Monad
import Data.Monoid

data Name = Name { firstName :: String
                 , lastName :: String }

data GradeLevel = Freshman
    | Sophmore
    | Junior
    | Senior deriving (Eq,Ord,Enum,Show)

instance Show Name where
    show (Name first last) = mconcat [first," ",last]

data Student = Student { studentId :: Int
                       , gradeLevel :: GradeLevel
                       , studentName :: Name } deriving Show    

data Teacher = Teacher { teacherId :: Int
                       , teacherName :: Name } deriving Show

data Course = Course { courseId :: Int
                      , courseTitle :: String
                      , teacher :: Int } deriving Show

students :: [Student]
students = [ (Student 1 Senior (Name "Audre" "Lorde"))
           , (Student 2 Junior (Name "Leslie" "Silko"))
           , (Student 3 Freshman (Name "Judith" "Butler")) ]

teachers :: [Teacher]
teachers = [ (Teacher 100 (Name "Simone" "De Beauvior"))
           , (Teacher 200 (Name "Susan" "Sontag")) ]

courses :: [Course]
courses = [ Course 101 "French" 100
          , Course 201 "English" 200 ]

-- _select :: (a -> b) -> [a] -> [b]
_select :: Monad m => (a -> b) -> m a -> m b
_select prop vals = do
    val <- vals
    return (prop val)

-- _where :: (a -> Bool) -> [a] -> [a]
_where :: (MonadPlus m, Alternative m) => (a -> Bool) -> m a -> m a
_where test vals = do
    val <- vals
    guard (test val)
    return val

-- _join :: Eq c => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a,b)]
_join :: (MonadPlus m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a,b)
_join data1 data2 prop1 prop2 = do
    d1 <- data1
    d2 <- data2
    let dpairs = (d1,d2)
    guard ((prop1 (fst dpairs)) == (prop2 (snd dpairs)))
    return dpairs

startsWith :: Char -> String -> Bool
startsWith char string = char == (head string)

joinData = (_join teachers courses teacherId teacher)
whereResult = _where ((== "English") . courseTitle . snd) joinData
-- selectResult = _select (teacherName . fst) whereResult

-- _hinq selectQuery joinQuery whereQuery = (\joinData ->
--                                            (\whereResult ->
--                                              selectQuery whereResult) (whereQuery joinData)
--                                          ) joinQuery

_hinq selectQuery joinQuery whereQuery = selectQuery whereResult
    where whereResult = whereQuery joinQuery
--          joinData = joinQuery

finalResult :: [Name]
finalResult = _hinq (_select (teacherName .fst))
                    (_join teachers courses teacherId teacher)
                    (_where ((== "English") . courseTitle . snd))

teacherFirstName :: [String]
teacherFirstName = _hinq (_select firstName)
                         finalResult
                         (_where (\_ -> True))

data HINQ m a b = HINQ (m a -> m b) (m a) (m a -> m a)
                | HINQ_ (m a -> m b) (m a)

-- instance Monoid HINQ where
    -- mempty = ???
    -- mappend (HINQ s1 j1 w1) (HINQ s2 j2 w2) = HINQ s1 (HINQ s2 j2 w2) w1 

runHINQ :: (MonadPlus m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ sClause jClause wClause) = _hinq sClause jClause wClause
runHINQ (HINQ_ sClause jClause) = _hinq sClause jClause
                                               (_where (\_ -> True))

query1 :: HINQ [] (Teacher, Course) Name
query1 = HINQ (_select (teacherName . fst))
              (_join teachers courses teacherId teacher)
              (_where ((== "English") . courseTitle . snd))

query2 :: HINQ [] Teacher Name
query2 = HINQ_ (_select teacherName)
              teachers

possibleTeacher :: Maybe Teacher
possibleTeacher = Just (head teachers)

possibleCourse :: Maybe Course
possibleCourse = Just (head courses)

maybeQuery :: HINQ Maybe (Teacher,Course) Name
maybeQuery = HINQ (_select (teacherName .fst))
                  (_join possibleTeacher possibleCourse
                                         teacherId teacher)
                  (_where ((== "French") . courseTitle . snd))

missingCourse :: Maybe Course
missingCourse = Nothing

maybeQuery2 :: HINQ Maybe (Teacher,Course) Name
maybeQuery2 = HINQ (_select (teacherName . fst))
                   (_join possibleTeacher missingCourse teacherId teacher)
                   (_where ((== "French") . courseTitle . snd))

data Enrollment = Enrollment { student :: Int
                             , course :: Int } deriving Show

enrollments :: [Enrollment]
enrollments = [ (Enrollment 1 101)
              , (Enrollment 2 101)
              , (Enrollment 2 201)
              , (Enrollment 3 101) ]

studentEnrollmentsQ = HINQ_ (_select (\(st,en) ->
                                       (studentName st, course en)))
                            (_join students enrollments studentId student)

studentEnrollments :: [(Name,Int)]
studentEnrollments = runHINQ studentEnrollmentsQ

frenchStudentsQ = HINQ (_select (fst . fst))
                        (_join studentEnrollments
                               courses
                               snd
                               courseId)
                        (_where ((== "French") . courseTitle . snd))

frenchStudents :: [Name]
frenchStudents = runHINQ frenchStudentsQ

getEnrollments :: String -> [Name]
getEnrollments courseName = runHINQ courseQuery
    where courseQuery = HINQ (_select (fst . fst))
                             (_join studentEnrollments
                                courses
                                snd
                                courseId)
                             (_where ((== courseName) . courseTitle . snd))
