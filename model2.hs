import Data.Char
--e1

data Expr = Var String | Val Int | Plus Expr Expr | Mult Expr Expr
    deriving (Show, Eq)

class Operations exp where
    simplify :: exp -> exp

instance Operations(Expr) where
    simplify (Var s) = Var s
    simplify (Val n) =  Val n
    simplify (Plus e1 e2) = case (simplify e1, simplify e2) of
             (x, (Val 0)) -> x
             ((Val 0), x) -> x
             (l, r) -> Plus l r
    simplify (Mult e1 e2) = case (simplify e1,simplify e2) of
             (x, (Val 0)) -> (Val 0)
             ((Val 0), x) -> (Val 0)
             (x, (Val 1)) -> x
             ((Val 1), x) -> x
             (l, r) -> Mult l r
            
    
ex1 = Mult (Plus (Val 1) (Var "x")) (Val 1)
ex2 = Plus ex1 (Val 3)
ex3 = Plus (Mult (Val 0) (Val 2)) (Val 3)
ex4 = Mult ex3 (Val 5)

--2

f:: [Char] -> [Char]
f "" = ""
f (x:xs) 
    |(isAlpha x) && ((elem x "aeiouAEIOU") == False)   = x : 'P' : x : (f xs)
    |otherwise  = x : (f xs)
{-
f' sir = do
    x <- sir
    if ( (isAlpha x) && ((elem x "aeiouAEIOU") == False) )
        then [x : 'P' : x]
    else [x]
    -}

f'' :: String -> String
f'' sir = sir >>= \x -> 
    if isAlpha x && notElem x "aeiouAEIOU"
        then [x, 'P', x]
    else [x]


--3

newtype WriterM a =  MW {getMW :: (Maybe a, String)}
  deriving Show

instance Functor WriterM where
    fmap f (MW (mVal, sir)) = case mVal of
        Nothing -> MW (Nothing, sir)
        Just a -> MW (Just (f a), sir)


instance Applicative WriterM where
    pure a = MW (Just a, "")
    (MW (Nothing, sir)) <*> _ = MW (Nothing, sir)
    (MW (Just f, sir1)) <*> (MW (Nothing, sir2)) = MW (Nothing, sir1 ++ sir2)
    (MW (Just f, sir1)) <*> (MW (Just a, sir2)) = MW (Just (f a), sir1 ++ sir2)

instance Monad WriterM where
    return = pure
    (MW (Nothing, sir1)) >>= _ = MW (Nothing, sir1)
    (MW (Just a, sir1)) >>= k = let (MW (b, sir2)) = k a
                                in MW (b, sir1 ++ sir2)

testWriterM :: WriterM Int
testWriterM = ma >>= k where
    ma = 
        MW (Just 7, "ana are mere ")
    k x = 
        MW (if mod x 2 == 0 then Just x else Nothing, "si pere!")