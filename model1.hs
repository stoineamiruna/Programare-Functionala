import Data.Char
--1

data Prop = V String | T | F | Prop :&: Prop | Prop :|: Prop
    deriving (Show, Eq)

class Operations exp where
    simplify :: exp -> exp

instance Operations Prop where
    simplify T = T
    simplify F = F
    simplify (V x) = V x
    simplify (left :&: right) = case (simplify left, simplify right) of
        (T, r) -> r
        (l, T) -> l
        (F, _) -> F
        (_, F) -> F
        (l, r) -> l :&: r
    simplify (left :|: right) = case (simplify left, simplify right) of
        (T, _) -> T
        (_, T) -> T
        (F, r) -> r
        (l, F) -> l
        (l, r) -> l :|: r

prop1 = ((V "p") :|: (V "q")) :&: T
prop2 = prop1 :|: (V "r")
prop3 = ((F :&: V "p") :|: (V "q"))
prop4 = prop3 :&: (V "q")


--ex 2

f:: [Char] -> [Char]
f "" = ""
f (x:xs) 
    |isUpper x  = (toLower x) : (f xs)
    |isLower x  = (toUpper x) : (f xs)
    |isDigit x  = "*" ++ (f xs)
    |otherwise  = f xs
    
f' sir = do
    x <- sir
    if (isUpper x)
        then return (toLower x)
    else if (isLower x)
        then return (toUpper x)
    else if (isDigit x)
        then return '*'
    else ""

f'' sir = sir >>= \x -> 
            if (isUpper x)
                then return (toLower x)
            else if (isLower x)
                then return (toUpper x)
            else if (isDigit x)
                then return '*'
            else ""

--3

newtype ReaderM env a = ReaderM { runReaderM :: env -> Either String a }

instance Functor (ReaderM env) where
    fmap f (ReaderM g) = ReaderM $ \env ->
        case g env of
            Right a -> Right (f a)
            Left err -> Left err

instance Applicative (ReaderM env) where
    pure a = ReaderM $ \_ -> Right a
    (ReaderM f) <*> (ReaderM a) = ReaderM $ \env ->
        case f env of
            Right func -> case a env of
                Right val -> Right (func val)
                Left err -> Left err
            Left err -> Left err

instance Monad (ReaderM env) where
    return = pure
    (ReaderM a) >>= f = ReaderM $ \env ->
        case a env of
            Right val -> runReaderM (f val) env
            Left err -> Left err

testReaderM :: ReaderM String String
testReaderM = ma >>= k
    where
        ma =
            ReaderM (\str -> if length str > 10 then Right (length str) else Left "")
        k val =
            ReaderM (\str -> if mod val 2 == 0 then Right "par" else Left "")
