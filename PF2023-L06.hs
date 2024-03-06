--1

data Fruct
    = Mar String Bool
    | Portocala String Int

cosFructe = [Mar "Ionatan" False,
            Portocala "Sanguinello" 10,
            Portocala "Valencia" 22,
            Mar "Golden Delicious" True,
            Portocala "Sanguinello" 15,
            Portocala "Moro" 12,
            Portocala "Tarocco" 3,
            Portocala "Moro" 12,
            Portocala "Valencia" 2,
            Mar "Golden Delicious" False,
            Mar "Golden" False,
            Mar "Golden" True]

--a

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala "Tarocco" _)     = True
ePortocalaDeSicilia (Portocala "Moro" _)         = True
ePortocalaDeSicilia (Portocala "Sanguinello" _)  = True
ePortocalaDeSicilia fruct                        = False

--b 
nrfelii :: Fruct ->Int
nrfelii (Portocala _ nr)=nr
nrfelii fruct=0

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia []           =0
nrFeliiSicilia (x:xs)
    |ePortocalaDeSicilia x  =(nrfelii x)+(nrFeliiSicilia xs)
    |otherwise              =nrFeliiSicilia xs

--c
nrMereViermi :: [Fruct] -> Int
nrMereViermi lista = length (filter marCuVierme lista)
  where 
    marCuVierme (Mar _ True) = True
    marCuVierme _ = False


--2

type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

--a
vorbeste :: Animal -> String
vorbeste (Pisica _)="Meow!"
vorbeste (Caine _ _)="Woof!"

--b 
rasa :: Animal -> Maybe String
rasa (Caine _ rs) = Just(rs)
rasa a            = Nothing

--3
data Linie = L [Int]
    deriving Show
data Matrice = M [Linie]
    deriving Show

--a
sumaLinie :: Linie -> Int
sumaLinie (L lin) = sum lin

verifica :: Matrice -> Int -> Bool
verifica (M mat) n
    |length mat ==1  && sumaLinie(head mat)==n      =True
    |length mat ==1                                 =False
    |sumaLinie(head mat)==n                         =verifica (M (tail mat)) n
    |otherwise                                      =False

--b 
liniepoz :: Linie -> Bool
liniepoz (L lin) = length lin==length (filter (>0) lin)

lengthL :: Linie -> Int
lengthL (L lin) = length lin

doarPozN :: Matrice -> Int -> Bool
doarPozN (M mat) n
    |length mat==1 && lengthL (head mat)==n && liniepoz (head mat)==True    =True
    |length mat==1 && lengthL (head mat)==n                                 =False
    |length mat==1                                                          =True
    |lengthL (head mat)==n && liniepoz (head mat)==True                     =doarPozN (M (tail mat)) n
    |lengthL (head mat)==n                                                  =False
    |otherwise                                                              =doarPozN (M (tail mat)) n


--c 
corectc :: [Linie] -> Int -> Bool
corectc mat n
    |length mat==1 && lengthL (head mat)==n =True
    |length mat==1                          =False
    |lengthL (head mat)==n                  =corectc (tail mat) n
    |otherwise                              =False

corect :: Matrice -> Bool
corect (M mat) = corectc mat (lengthL (head mat)) 