--2
factori :: Int -> [Int]
factori n=[d|d<-[1..n], (mod n d)==0]

--3
prim :: Int->Bool
prim n=(length(factori n)==2)

--4
numerePrime:: Int->[Int]
numerePrime n=[x|x<-[2..n],prim x]


--5?
--myzip3:: [int]->[Int]->[Int]->[(Int,Int,Int)]
--myzip3 a b c=zip (zip a b) c

--6
firstEl :: [(a,b)] -> [a]
firstEl lista=map (\(x,__)->x) lista

--7
sumList::[[Int]]->[Int]
sumList lista=map sum lista

--8
pre12:: [Int]->[Int]
pre12 lista=map (\x->if odd x then 2*x else div x 2) lista

--9
f9:: Char->[String]->[String]
f9 c lista=filter (elem c) lista

--10
f10::[Int]->[Int]
f10 lista=map (\x->x^2) (filter odd lista)

--11
f11::[Int]->[Int]
f11 lista= map (\(a,b)->a*a) (filter (\(a,b)->odd b) (zip lista [1..length(lista)]))

--12
vocala:: Char->Bool
vocala c=elem c "aeiouAEIOU"     

numaiVocale::[String]->[String]
numaiVocale lista=map (\sir->(filter vocala sir)) lista 

--13

mymap::(a->b)->[a]->[b]
mymap f []=[] 
mymap f lista=[f (head lista)]++(mymap f (tail lista))

myfilter:: (a->Bool)->[a]->[a]
myfilter f []=[]
myfilter f lista
    |f (head lista) =[head lista]++(myfilter f (tail lista))
    |otherwise      =myfilter f (tail lista)
