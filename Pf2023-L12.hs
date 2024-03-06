--1

data List a = Nil | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x ls) = Cons (f x) (fmap f ls)

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    (Cons x ls) <*> orice = appendList (fmap x orice) (ls <*> orice)

appendList :: List a -> List a -> List a
appendList Nil orice = orice
appendList (Cons x xs) ys = Cons x (appendList xs ys) 

--2

data Cow = Cow {
    name :: String,
    age :: Int,
    weight :: Int
    } deriving (Eq, Show)

--a
noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative x
    |x<0        = Nothing
    |x>=0       = Just x

--b 
cowFromString :: String -> Int -> Int -> Maybe Cow
{-
cowFromString n a w 
    |((noEmpty n) /= Nothing) && ((noNegative a) /= Nothing) && ((noNegative w) /= Nothing)     = Just (Cow n a w)
    |otherwise                                                                                  = Nothing
-}
--c
cowFromString n a w = Cow <$> noEmpty n <*> noNegative a <*> noNegative w

--3

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person Name Address
    deriving (Eq, Show)

--a

validateLength :: Int -> String -> Maybe String
validateLength n str = if ((length str) < n) then (Just str) else Nothing

--b 

mkName :: String -> Maybe Name
mkName nume = fmap Name (validateLength 25 nume)

mkAddress :: String -> Maybe Address
mkAddress adresa = fmap Address (validateLength 100 adresa)

--c 

--mkPerson :: String -> String -> Maybe Person
{-
mkPerson nume adresa 
    | mkName nume == Nothing        = Nothing
    | mkAddress adresa == Nothing   = Nothing
    | otherwise                     = Just (Person (Name nume) (Address adresa) )
-}
--d

mkPerson :: String -> String -> Maybe Person
mkPerson nume adresa = pure Person <*> mkName nume <*> mkAddress adresa