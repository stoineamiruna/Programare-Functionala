--1

f1:: [Int]->Int
f1 lista=foldr (+) 0 (map (\x->x*x) (filter odd lista))

--2

allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies prop lista=length(lista)==length(filter prop lista)

--3

anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies prop lista=(length(filter prop lista)>0)

--4
mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f lista= foldr (\x xs -> f x : xs) [] lista

--5 
filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr f lista=foldr (\x xs-> if f x then (x:xs) else xs) [] lista

--6
op6:: Integer->Integer->Integer
op6 x y=10*x+y

listToInt :: [Integer] -> Integer
listToInt lista
    |lista==[]  =0
    |otherwise  =foldl op6 0 lista

--7

--a
rmChar :: Char -> String -> String
rmChar chr sir=filter (\c->if c==chr then False else True) sir

--b 
rmCharsRec :: String -> String -> String
rmCharsRec sir1 sir2
    |sir1==[]   =sir2
    |otherwise  =rmCharsRec (tail sir1) (rmChar (head sir1) sir2)

--c

rmCharsFold :: String -> String -> String
rmCharsFold chars str = foldr rmChar str chars

--8

myReverse:: [Int]->[Int]
myReverse lista=foldl (\x xs-> xs:x) [] lista

--9

myElem:: Int->[Int]->Bool
myElem e lista
    |length(filter (\x->(x==e)) lista)>0  =True
    |otherwise                          =False

--10

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip lista=((map (\(x,y)->x) lista),(map (\(x,y)->y) lista))

--11

union :: [Int]->[Int]->[Int]
union lista1 lista2=lista1++(filter (\x-> if (myElem x lista1)==True then False else True) lista2)

--12

intersect :: [Int]->[Int]->[Int]
intersect lista1 lista2=filter (\x-> myElem x lista1) lista2

--13
permuta::[Int]->Int->[[Int]]
permuta lista n
    |lista==[]  =[[]]
    |n==1       =[lista]
    |otherwise  =(head lista : tail lista) : permuta (tail lista ++ [head lista]) (n - 1)


permutations::[Int]->[[Int]]
permutations lista
    |lista==[]  =[[]]
    |otherwise  =permuta lista (length(lista))