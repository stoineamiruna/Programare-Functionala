import Data.Char

--1. Sa se scrie o functie care pentru o lista de numere intregi afiseaza suma dintre diferenta dintre doua elemente de pe pozitii consecutive daca elementele sunt 
--divizibile cu 3 sau produsul dintre ele, altfel. 

f11 :: Int -> Int -> Int
f11 a b = if (mod a 3 == 0) && (mod b 3 == 0) then (a-b) else a*b

f1 :: [Int] -> Int
f1 [] = 0
f1 lista 
    | length lista == 1     = 0
    | otherwise             = f11 (head lista) (head (tail lista)) + f1 (tail lista)

--2. Sa se scrie o functie care primeste o lista de siruri de caractere si un numar intreg si verifica daca sirurile care au lungimea mai mare strict decat numarul 
--dat contin acelasi numar de vocale si consoane.

nrVoc :: String -> Int
nrVoc "" = 0
nrVoc (x:xs) = if (isAlpha x == False)
                    then (nrVoc xs)
               else if (elem x "aeiouAEIOU")
                    then 1 + (nrVoc xs)
                else 
                    (nrVoc xs)
                

nrCons :: String -> Int
nrCons "" = 0
nrCons (x:xs) = if (isAlpha x == False)
                    then (nrCons xs)
               else if (elem x "aeiouAEIOU")==False
                    then 1 + (nrCons xs)
                else 
                    (nrCons xs)

f2 :: [String] -> Int -> Bool
f2 [] n = True
f2 (x:xs) n =   if (length x > n) && (nrVoc x == nrCons x)
                    then f2 xs n
                else if (length x > n)
                    then False
                else f2 xs n

--3. Sa se scrie o functie care primeste ca parametru o lista de liste de numere intregi si calculeaza produsul sumelor elementelor pare de pe fiecare linie. In 
--rezolvarea exercitiului NU se vor folosi functiile product sau sum.

f3 :: [[Int]] -> Int
f3 []   = 1
f3 (x:xs) = (foldr (+) 0 [y| y<-x,  mod y 2 == 0]) * (f3 xs)

--4. Sa se scrie o functie care primeste un sir de caractere si o lista de siruri de caractere si verifica daca toate sirurile care au ca prefix sirul dat ca parametru, 
--au lungime para.

f4 :: [String] -> String -> Bool
f4 [] _ = True
f4 (x:xs ) sir =    if( (head (words x)) == sir) && mod (length x) 2 == 0
                        then (f4 xs sir)
                    else if ( head (words x) == sir)
                        then False
                    else
                        f4 xs sir
--lines pt linii, words pt cuv

--5. Se dau urmatoarele tipuri de date reprezentand dictionare. Un dictionar poate fi format dintr-o intrare (cu titlu si definitie) sau o lista de dictionare 
--(continand un titlu si lista de dictionare).

type Name = String
type Def = String
data Dictionar = I Name Def
                | Ld Name [Dictionar]
        deriving Show

--a 

f5a :: Dictionar -> [String]
f5a (I n d) = [n]
f5a (Ld n d) = n : (g d)
            where 
                g [] = []
                g (x:xs) = (f5a x) ++ (g xs)

d1 = Ld "animal"[Ld "mamifer"[I "elefant" "acesta e un elefant", I "caine" "acesta este un caine", I "pisica" "aceasta este o pisica", I "animale domestice" "definitie"]]
d2 = Ld "Animal"[Ld "Mamifer"[I "Elefant" "acesta e un elefant",I "calne" "acesta este un caine",I "piSIca" "aceasta este o pisica"],I "animale domestice" "definitie"]
d3 = Ld "animal"[Ld "mamifer" [I "elefant" "Acesta e un Elefant", I "caine" "acesta este un caine", I "pisica" "aceasta este o pisica"], I "animale domestice" "definitie"]

--b 

instance Eq(Dictionar) where
    (I n1 d1) == (I n2 d2) = (d1 == d2) && ((map toUpper n1)==(map toUpper n2))
    (I n1 d1) == _ = False
    _ == (I n1 d1) = False
    (Ld n1 d1) == (Ld n2 d2) = ((map toUpper n1)==(map toUpper n2)) && (d1 == d2)

--6. Sa se scrie o functie care primeste o lista de liste de numere intregi si doua numere intregi n si m si verifica daca numarul sublistelor care au doar elemente 
--divizibile cu 3 este cuprins intre n si m (sau m si n, in functie de ordinea in care sunt date).

f62 :: [Int] -> Bool
f62 [] = True
f62 (x:xs) 
    |mod x 3 == 0   = f62 xs
    |otherwise      = False

f61 :: [[Int]] -> Int
f61 ls = length [1 | x<-ls, f62 x]

f6 :: [[Int]] -> Int -> Int -> Bool
f6 ls n m 
        |n<=m  = if (f61 ls)<=m && (f61 ls) >= n then True else False
        |n>=m  = if (f61 ls)<=n && (f61 ls) >= m then True else False


--7

data Tree = Empty 
    | Node Int Tree Tree Tree -- arbore vid cu valoare de tip Int in radacina si 3 fii

extree :: Tree
extree = Node 4 (Node 5 Empty Empty Empty) (Node 3 Empty Empty (Node 1 Empty Empty Empty)) Empty

class ArbInfo t where
    level :: t-> Int -- intoarce inaltimea arborelui; pt un arbore vid se considera ca are inaltimea 0
    sumval:: t -> Int -- intoarce suma valorilor din arbore
    nrFrunze :: t -> Int -- intoarce nr de frunze al arborelui

instance ArbInfo(Tree) where
    level Empty = 0
    level (Node nod t1 t2 t3) = 1 + max (level t1) (max (level t2) (level t3))

    sumval Empty = 0
    sumval (Node nod t1 t2 t3) = nod + (sumval t1) + (sumval t2) + (sumval t3)

    nrFrunze Empty = 0
    nrFrunze (Node _ Empty Empty Empty) = 1
    nrFrunze (Node nod t1 t2 t3) = (nrFrunze t1) + (nrFrunze t2) + (nrFrunze t3)
