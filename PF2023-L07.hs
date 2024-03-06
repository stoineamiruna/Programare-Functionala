data Expr = Const Int -- integer constant
    | Expr :+: Expr -- addition
    | Expr :*: Expr -- multiplication
    deriving Eq

data Operation = Add | Mult deriving (Eq, Show)
data Tree = Lf Int -- leaf
    | Node Operation Tree Tree -- branch
    deriving (Eq, Show)

instance Show Expr where
    show (Const x) = show x
    show (e1 :+: e2) = "(" ++ show e1 ++ " + "++ show e2 ++ ")"
    show (e1 :*: e2) = "(" ++ show e1 ++ " * "++ show e2 ++ ")"

--1

evalExp :: Expr -> Int
evalExp (Const e1)=e1
evalExp (e1 :+: e2)=(evalExp e1)+(evalExp e2)
evalExp (e1 :*: e2)=(evalExp e1)*(evalExp e2)

exp1 = ((Const 2 :*: Const 3) :+: (Const 0 :*: Const 5))
exp2 = (Const 2 :*: (Const 3 :+: Const 4))
exp3 = (Const 4 :+: (Const 3 :*: Const 3))
exp4 = (((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2)

--2
evalArb :: Tree -> Int
evalArb (Lf x)=x
evalArb (Node Add t1 t2)=(evalArb t1)+(evalArb t2)
evalArb (Node Mult t1 t2)=(evalArb t1)*(evalArb t2)

arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0)(Lf 5))
arb2 = Node Mult (Lf 2) (Node Add (Lf 3)(Lf 4))
arb3 = Node Add (Lf 4) (Node Mult (Lf 3)(Lf 3))
arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3)(Lf 1))) (Lf 2)

--3
expToArb :: Expr -> Tree
expToArb (Const e1)=Lf e1
expToArb (e1 :+: e2)=Node Add (expToArb e1) (expToArb e2)
expToArb (e1 :*: e2)=Node Mult (expToArb e1) (expToArb e2)


data IntSearchTree value
    = Empty
    | BNode
        (IntSearchTree value) -- elemente cu cheia mai mica
        Int -- cheia elementului
        (Maybe value) -- valoarea elementului
        (IntSearchTree value) -- elemente cu cheia mai mare

--4
lookup' :: Int -> IntSearchTree value -> Maybe value
lookup' x Empty = Nothing
lookup' x (BNode vmin cheie val vmax)
    | x == cheie = val
    | x < cheie = lookup' x vmin
    | otherwise = lookup' x vmax

ex4 :: IntSearchTree Int
ex4 =
    BNode
        (BNode Empty 3 (Just 3) Empty)
        5
        (Just 5)
        (BNode Empty 8 (Just 8) Empty)

r1=lookup' 5 ex4

--5
keys :: IntSearchTree value -> [Int]
keys Empty=[]
keys (BNode vmin cheie val vmax)=[cheie]++(keys vmin) ++ (keys vmax) 

--6
values :: IntSearchTree value -> [value]
values Empty=[]
values (BNode vmin cheie (Just val) vmax) = val : (values vmin ++ values vmax)
values (BNode vmin cheie Nothing vmax) = values vmin ++ values vmax

--7

insert :: Int -> value -> IntSearchTree value -> IntSearchTree value
insert cheiec valc Empty=BNode Empty cheiec (Just valc) Empty
insert cheiec valc (BNode vmin cheie val vmax)
    |cheiec==cheie  =BNode vmin cheiec (Just valc) vmax
    |cheiec<cheie   =BNode (insert cheiec valc vmin) cheie val vmax
    |otherwise      =BNode vmin cheie val (insert cheiec valc vmax)

--8

deleteMin :: IntSearchTree value -> (Int, Maybe value, IntSearchTree value)
deleteMin (BNode Empty k val vmax) = (k, val, vmax)
deleteMin (BNode vmin k val vmax) = let (minKey, minVal, restVmin) = deleteMin vmin
                                        in (minKey, minVal, BNode restVmin k val vmax)
deleteMin _ = error "deleteMin called on an empty tree"

delete :: Int -> IntSearchTree value -> IntSearchTree value
delete cheiec Empty=Empty
delete cheiec (BNode vmin cheie val vmax)
    |cheiec<cheie   =BNode (delete cheiec vmin) cheie val vmax
    |cheiec>cheie      =BNode vmin cheie val (delete cheiec vmax)
    |otherwise = case (vmin, vmax) of
        | otherwise = case (vmin, vmax) of
                    (Empty, _) -> vmax
                    (_, Empty) -> vmin
                    (_, _) -> let (minKey, minVal, restVmax) = deleteMin vmax
                              in BNode vmin minKey minVal restVmax

--9

toList :: IntSearchTree value -> [(Int, value)]
toList Empty = []
toList (BNode vmin cheie Nothing vmax)=toList vmin ++ [(cheie, error "Unexpected Nothing")] ++ toList vmax
toList (BNode vmin cheie (Just val) vmax)=toList vmin ++ [(cheie, val)] ++ toList vmax

--10

fromList :: [(Int, value)] -> IntSearchTree value
fromList [] = Empty
fromList ((key, val):coada) = insert key val (fromList coada)

--11

printTree :: IntSearchTree value -> String
printTree Empty=""
printTree (BNode Empty cheie val Empty)=show cheie
printTree (BNode vmin cheie val Empty)="("++ printTree vmin ++ ")" ++ show cheie
printTree (BNode Empty cheie val vmax)=show cheie ++ "("++ printTree vmax ++ ")"
printTree (BNode vmin cheie val vmax)="("++ printTree vmin ++ ") " ++ show cheie ++ " ("++ printTree vmax ++ ")"