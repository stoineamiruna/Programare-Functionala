import Data.Char (isDigit, digitToInt)

--1

ePalindrom:: String->Bool
ePalindrom sir = sir==(reverse sir)

eVocala:: Char->Bool 
eVocala c   =elem c "aeiouAEIOU"

numarVocale :: String -> Int
numarVocale " " =0
numarVocale (c:sir)
    |eVocala c   =1+(numarVocale sir)
    |otherwise   =numarVocale sir

nrVocale:: [String]->Int
nrVocale [] =0
nrVocale(s:xs)
    |ePalindrom s =(numarVocale s) + (nrVocale xs)
    |otherwise      =nrVocale xs

--2

f:: Int->[Int]->[Int]
f nr lista 
    |lista==[]          =[]
    |even (head(lista)) = [head(lista)]++[nr]++(f nr (tail(lista)))
    |otherwise          = [head(lista)]++(f nr (tail(lista)))

--3
divizori::Int->[Int]
divizori n=[x|x<-[1..n],(mod n x )==0]

--4

listadiv:: [Int]->[[Int]]
listadiv lista=[[x|x<-[1..n],(mod n x )==0]|n<-lista]

--5
--a 
inIntervalRec::Int->Int->[Int]->[Int]
inIntervalRec inf sup lista
    |lista==[]                              =[]
    |head(lista)>=inf&&head(lista)<=sup     =[head(lista)]++(inIntervalRec inf sup (tail(lista)))
    |otherwise                              =inIntervalRec inf sup (tail(lista))

--b
inIntervalComp::Int->Int->[Int]->[Int]
inIntervalComp inf sup lista=[x|x<-lista, x<=sup&&x>=inf]

--6
--a 
pozitiveRec:: [Int]->Int
pozitiveRec lista
    |lista==[]      =0
    |head(lista)>0  =1+pozitiveRec (tail(lista))
    |otherwise      =pozitiveRec (tail(lista))

--b
pozitiveComp:: [Int]->Int
pozitiveComp lista=length([x|x<-lista,x>0])

--7
--a
pozitii:: [Int]->Int->[Int]
pozitii lista poz
    |lista==[]  =[]
    |odd (head(lista))  =poz:(pozitii (tail(lista)) (poz+1))
    |otherwise          =pozitii (tail(lista)) (poz+1)

pozitiiImpareRec::[Int]->[Int]
pozitiiImpareRec lista = pozitii lista 0

--b 

pozitiiImpareComp::[Int]->[Int]
pozitiiImpareComp lista=[b|(a,b)<-(zip lista [0..((length(lista))-1)]), odd a]

--8
--a 

multDigitsRec :: String -> Int
multDigitsRec sir
    | sir == ""          = 1
    | isDigit (head sir) = digitToInt (head sir) * multDigitsRec (tail sir)
    | otherwise          = multDigitsRec (tail sir)

--b

multDigitsComp :: String -> Int
multDigitsComp sir=product [(digitToInt(x))|x<-sir,isDigit(x)]


--9
permuta::[Int]->Int->[[Int]]
permuta lista n
    |lista==[]  =[[]]
    |n==1       =[lista]
    |otherwise  =(head lista : tail lista) : permuta (tail lista ++ [head lista]) (n - 1)


permutation::[Int]->[[Int]]
permutation lista
    |lista==[]  =[[]]
    |otherwise  =permuta lista (length(lista))

--10
combinari :: Int -> [a] -> [[a]]
combinari 0 _ = [[]]  
combinari _ [] = []   
combinari k (x:xs) = combinari k xs ++ [x : ys | ys <- combinari (k - 1) xs]

--11
aranjamente :: Int -> [a] -> [[a]]
aranjamente k lista
    | k < 0 || k > length lista = []  
    | k == 0 = [[]]  
    | otherwise = filter (\arrangement -> length arrangement == k) (permutation lista)

