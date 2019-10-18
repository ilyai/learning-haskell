
--
-- type synonyms
--
type FirstName = String
type LastName = String
type Age = Int
type Height = Int

type PatientName = (String,String)

patientInfo :: PatientName -> Age -> Height -> String
patientInfo patientName age height = name ++ " " ++ ageHeight
    where name = snd patientName ++ ", " ++ fst patientName
          ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

firstName :: PatientName -> String
firstName patient = fst patient


--
-- new types
--
data Sex = Male | Female

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

type MiddleName = String
data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

data Patient = Patient Name Sex Int Int Int BloodType

getAge :: Patient -> Int
getAge (Patient _ _ a _ _ _) = a

data PatientV2 = PatientV2 { name :: Name
                           , sex :: Sex
                           , age :: Int
                           , height :: Int
                           , weight :: Int
                           , bloodType :: BloodType }

canDonateToV2 :: PatientV2 -> PatientV2 -> Bool
canDonateToV2 patient1 patient2 = canDonateTo (bloodType patient1) (bloodType patient2)

patientSummary :: PatientV2 -> String
patientSummary p = "***************\n"
                ++ "Patient Name: " ++ showName (name p) ++ "\n"
                ++ "Sex: " ++ [sexInitial (sex p)] ++ "\n"
                ++ "Age: " ++ show (age p) ++ "\n"
                ++ "Height: " ++ show (height p) ++ "\n"
                ++ "Weight: " ++ show (weight p) ++ "\n"
                ++ "Blood Type: " ++ showBloodType (bloodType p) ++ "\n"
                ++ "***************\n"
