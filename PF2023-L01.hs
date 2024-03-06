import Data.List

myInt = 31415926535897932384626433832795028841971693993751058209749445923

double :: Integer -> Integer
double x = x+x

triple :: Integer -> Integer
triple x = x+x+x

maxim :: Integer -> Integer -> Integer
maxim x y = if (x > y) then x else y

maxim3 :: Integer -> Integer -> Integer -> Integer
maxim3 x y z = if (x > y) 
                    then if (x > z)
                              then x
                              else z
                    else if (y > z)
                         then y
                    else z


maxim32 x y z=
     let  
          u = maxim x y
     in
          maxim u z


--maxim :: Integer -> Integer -> Integer



max3 x y z = let
             u = maxim x y
             in (maxim  u z)

--a           
sp :: Integer->Integer->Integer
sp x y = x*x+y*y

--b ???
paritate :: Integer->Char
paritate x = 
     if (x && 1 == 0)
          then "par"
          else "impar"