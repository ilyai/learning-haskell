import Control.Applicative

-- Functor 
r1 = fmap (+2) (Just 2)
r2 = (+2) <$> Just 2

-- Applicative
r3 = Just (+2) <*> Just 2
r4 = pure (+2) <*> Just 2

-- Monad
r5 = Just 2 >>= (\x -> Just (x + 2))
r6 = Just 2 >> Just (2+2)
r7 = return (2+2) :: Maybe Integer
r8 = fail (show (2+2)) :: Maybe ()
