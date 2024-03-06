myInt = 31415926535897932384626433832795028841971693993751058209749445923
--1
poly:: Integer->Integer->Integer->Integer->Integer
poly a b c x = a*x*x+b*x+c

--2
eeny:: Integer->[Char]
eeny x=
    if(even(x)==True)
        then "eeny"
        else "meeny"

--3
fizzbuzz:: Integer -> [Char]
fizzbuzz x=
    if(mod x 15==0)
        then "FizzBuzz"
    else if(mod x 3 == 0)
        then "Fizz"
    else if(mod x 5 == 0)
        then "Buzz"
    else ""

--4
tribonacci :: Integer->Integer
tribonacci 1=1
tribonacci 2=1
tribonacci 3=2
tribonacci n=
    tribonacci(n-1)+tribonacci(n-2)+tribonacci(n-3)

--5
binomial :: Integer->Integer->Integer
binomial 0 k = 0
binomial n 0 = 1
binomial n k = binomial (n-1) k + binomial (n-1) (k-1)

--6

--a
verifL:: [Integer]->Bool
verifL x=
    if(even(length(x))==True)
        then True
    else False

--b
takefinal:: [Integer]->Int->[Integer]
takefinal lista n
    |length(lista)==n    =lista
    |otherwise =takefinal (tail lista) n

--c 
remove:: [Int]->Int->[Int]
remove lista n
    |length(lista)<n    =lista
    |otherwise          =(take (n-1) lista)++(drop n lista)

--7
--a
myreplicate:: Int->Int->[Int]
myreplicate n v
    |n<=0       =[]
    |otherwise  =[v]++(myreplicate (n-1) v )

--b
sumImp:: [Int]->Int
sumImp lista
    |length(lista)==1   =
                        if((even (head(lista)))==False)
                            then head(lista)
                        else 0
    |otherwise          =
                        if((even (head(lista)))==False)
                            then head(lista)+sumImp(tail(lista))
                        else
                            sumImp(tail(lista))

--c 
totalLen:: [String]->Int
totalLen lista
    |length(lista)==1   =
                        if(head(head(lista))=='A')
                            then length(head(lista))
                        else 0
    |otherwise          =
                        if(head(head(lista))=='A')
                            then length(head(lista))+totalLen(tail(lista))
                        else 
                            totalLen(tail(lista))


    