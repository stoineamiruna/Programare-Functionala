--1

data Tree = Empty -- arbore vid
        | Node Int Tree Tree Tree -- arbore cu valoare de tip Int in radacina
        -- si 3 fii

extree :: Tree
extree = Node 4 (Node 5 Empty Empty Empty) (Node 3 Empty Empty (Node 1 Empty Empty Empty)) Empty

class ArbInfo t where
    level :: t -> Int -- intoarce inaltimea arborelui;
    -- consideram ca un arbore vid are inaltimea 0
    sumval :: t -> Int -- intoarce suma valorilor din arbore
    nrFrunze :: t -> Int -- intoarce nr de frunze al arborelui

instance ArbInfo Tree where
    level Empty = 0
    level (Node val tree1 tree2 tree3) = 1 + max ( max (level tree1)  (level tree2) )  (level tree3)

    sumval Empty = 0
    sumval (Node val tree1 tree2 tree3) = val + (sumval tree1) + (sumval tree2) + (sumval tree3)

    nrFrunze Empty = 0
    nrFrunze (Node val Empty Empty Empty) = 1
    nrFrunze (Node val tree1 tree2 tree3) = (nrFrunze tree1) + (nrFrunze tree2) + (nrFrunze tree3)

--2

class Scalar a where
    zero :: a
    one :: a
    adds :: a -> a -> a
    mult :: a -> a -> a
    negates :: a -> a
    recips :: a -> a

instance Scalar Int where
    zero = 0
    one = 1
    adds = (+)
    mult = (*)
    negates = negate
    recips x = 1 `div` x

instance Scalar Float where
    zero = 0.0
    one = 1.0
    adds = (+)
    mult = (*)
    negates = negate
    recips x = 1.0 / x

instance Scalar Double where
    zero = 0.0
    one = 1.0
    adds = (+)
    mult = (*)
    negates = negate
    recips x = 1.0 / x


class (Scalar a) => Vector v a where
    zerov :: v a
    onev :: v a
    addv :: v a -> v a -> v a -- adunare vector
    smult :: a -> v a -> v a -- inmultire cu scalare
    negatev :: v a -> v a -- negare vector

-- 3
data Vec2 a = Vec2 a a

instance (Scalar a) => Vector Vec2 a where
    zerov = Vec2 zero zero
    onev = Vec2 one one
    addv (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (adds x1 x2) (adds y1 y2)
    smult s (Vec2 x y) = Vec2 (mult s x) (mult s y)
    negatev (Vec2 x y) = Vec2 (negates x) (negates y)

instance Show a => Show (Vec2 a) where
    show (Vec2 x y) = "Vec2 " ++ show x ++ " " ++ show y

data Vec3 a = Vec3 a a a

instance (Scalar a) => Vector Vec3 a where
    zerov = Vec3 zero zero zero
    onev = Vec3 one one one
    addv (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (adds x1 x2) (adds y1 y2) (adds z1 z2)
    smult s (Vec3 x y z) = Vec3 (mult s x) (mult s y) (mult s z)
    negatev (Vec3 x y z) = Vec3 (negates x) (negates y) (negates z)

instance Show a => Show (Vec3 a) where
    show (Vec3 x y z) = "Vec3 " ++ show x ++ " " ++ show y ++ " " ++ show z

