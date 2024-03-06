--1

class Collection c where
    empty :: c key value
    singleton :: key -> value -> c key value
    insert
        :: Ord key
        => key -> value -> c key value -> c key value
    lookup :: Ord key => key -> c key value -> Maybe value
    delete :: Ord key => key -> c key value -> c key value
    keys :: c key value -> [key]
    values :: c key value -> [value]
    toList :: c key value -> [(key, value)]
    fromList :: Ord key => [(key,value)] -> c key value

    keys = map fst . toList
    values = map snd . toList

--2
newtype PairList k v = PairList { getPairList :: [(k, v)] }

instance Collection PairList where
    empty = PairList []
    singleton k v = PairList [(k, v)]
    insert k v (PairList pairs) = PairList $ (k, v) : pairs
    lookup k (PairList pairs) = Prelude.lookup k pairs
    delete k (PairList pairs) = PairList $ filter (\(key, _) -> key /= k) pairs
    keys (PairList pairs) = map fst pairs
    values (PairList pairs) = map snd pairs
    toList (PairList pairs) = pairs
    fromList = PairList

--3
data SearchTree key value
    = Empty
    | BNode
        (SearchTree key value) -- elemente cu cheia mai mica
        key -- cheia elementului
        (Maybe value) -- valoarea elementului
        (SearchTree key value) -- elemente cu cheia mai mare
{-
instance Collection SearchTree where
    empty = Empty
    singleton k v = BNode Empty k (Just v) Empty
    insert k v Empty = singleton k v
    insert k v (BNode leftTree key value rightTree)
        | k < key   = BNode (insert k v leftTree) key value rightTree
        | k > key   = BNode leftTree key value (insert k v rightTree)
        | otherwise = BNode leftTree key (Just v) rightTree
    lookup k Empty = Nothing
    lookup k (BNode leftTree key value rightTree)
        | k < key   = lookup k leftTree
        | k > key   = lookup k rightTree
        | otherwise = value
    delete k Empty = Empty
    delete k (BNode leftTree key value rightTree)
        | k < key   = BNode (delete k leftTree) key value rightTree
        | k > key   = BNode leftTree key value (delete k rightTree)
        | otherwise = BNode leftTree key Nothing rightTree
    keys Empty = []
    keys (BNode leftTree key _ rightTree) = keys leftTree ++ [key] ++ keys rightTree
    values Empty = []
    values (BNode leftTree _ (Just value) rightTree) = values leftTree ++ [value] ++ values rightTree
    values (BNode leftTree _ Nothing rightTree) = values leftTree ++ values rightTree
    toList Empty = []
    toList (BNode leftTree key value rightTree) = toList leftTree ++ [(key, v) | Just v <- [value]] ++ toList rightTree
    fromList = foldr (\(k,v) acc -> insert k v acc) empty
-}
--4

data Punct = Pt [Int]
data Arb = Vid | F Int | N Arb Arb
    deriving Show
class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a

instance Show Punct where
    show (Pt lista) = "(" ++ flista lista ++ ")"
        where flista [] = ""
              flista [x] = show x
              flista (x:xs) = show x ++ ", " ++ flista xs

--5
instance ToFromArb Punct where
    toArb (Pt []) = Vid
    toArb (Pt [x]) = F x
    toArb (Pt (x:xs)) = N (F x) (toArb (Pt xs))

    fromArb Vid = Pt []
    fromArb (F x) = Pt [x]
    fromArb (N l r) = Pt (flatten l ++ flatten r)
        where flatten Vid = []
              flatten (F y) = [y]
              flatten (N left right) = flatten left ++ flatten right

--6
data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
    perimeter :: (Floating a) => g a -> a
    area :: (Floating a) => g a -> a

instance GeoOps Geo where
    perimeter (Square l) = 4*l
    perimeter (Rectangle l h) = 2*h+2*l
    perimeter (Circle r) = 2*pi*r 

    area (Square l) = l*l
    area (Rectangle l h) = l*h
    area (Circle r) = pi*r*r 

instance (Floating a, Eq a) => Eq (Geo a) where
    g1 == g2 = perimeter g1 == perimeter g2
