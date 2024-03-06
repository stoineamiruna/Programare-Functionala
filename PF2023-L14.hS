data Point = Pt [Int]
    deriving (Show, Eq)

data Arb = Empty | Node Int Arb Arb
    deriving (Show, Eq)

class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a

--instance Functor (Point) where
--    fmap f (Pt ls) = Pt (map f ls)

insert :: Int -> Arb -> Arb
insert x Empty = Node x Empty Empty
insert x (Node nod st dr) 
    | x < nod    = Node nod (insert x st) dr
    | otherwise  = Node nod st (insert x dr)

f :: Arb -> [Int]
f Empty             = []
f (Node nod st dr)   = (f st) ++ [nod] ++ (f dr)

g :: [Int] -> Arb
g [] = Empty 
g ls = foldr insert Empty ls


instance ToFromArb (Point) where
    toArb (Pt []) = Empty
    toArb (Pt ls) = g ls

    fromArb Empty = Pt []
    fromArb arb = Pt (f arb)

--2

getFromInterval :: Int -> Int -> [Int] -> [Int]
getFromInterval a b ls = [x| x<-ls, x>=a && x<=b]

getFromInterval' :: Int -> Int -> [Int] -> [Int]
getFromInterval' a b xs = do
    x <- xs
    if x >= a && x <= b
        then return x
    else []

getFromInterval'' :: Int -> Int -> [Int] -> [Int]
getFromInterval'' a b xs = xs >>= \x -> if x >= a && x <= b then [x] else []


--3
newtype ReaderWriter env a = RW {getRW :: env-> (a,String)}

instance Functor (ReaderWriter env) where
  fmap f (RW g) = RW $ \env -> let (a, log) = g env in (f a, log)

instance Applicative (ReaderWriter env) where
  pure a = RW $ \_ -> (a, "")
  (RW mf) <*> (RW mx) = RW $ \env ->
    let (f, log1) = mf env
        (x, log2) = mx env
    in (f x, log1 ++ log2)

instance Monad (ReaderWriter env) where
  return = pure
  (RW g) >>= f = RW $ \env ->
    let (a, log1) = g env
        (RW h) = f a
        (b, log2) = h env
    in (b, log1 ++ log2)

