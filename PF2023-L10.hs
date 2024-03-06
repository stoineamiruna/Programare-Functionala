import Data.List (nub)
import Data.Maybe (fromJust)

type Nume = String
data Prop
  = Var Nume
  | F
  | T
  | Not Prop
  | Prop :|: Prop
  | Prop :&: Prop
  | Prop :->: Prop
  | Prop :<->: Prop
  deriving (Eq, Read)
infixr 2 :|:
infixr 3 :&:
infixr 1 :->: --precedenta lor
infixr 5 :<->:

p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not(Var "Q"))

p3 :: Prop
p3 = (Var "P" :&: (Var "Q" :|: Var "R")) :&: ((Not (Var "P") :|: Not(Var "Q")) :&: (Not (Var "P") :|: Not(Var "R")))

instance Show Prop where
    show (Var n)=n
    show (F)="Fals"
    show(T)="Adevarat"
    show(Not p)="(~"++show p++")"
    show (p1 :|: p2)="("++show p1 ++ "|" ++ show p2 ++ ")"
    show (p1 :&: p2)="("++show p1 ++ "&" ++ show p2 ++ ")"
    show (p1 :<->: p2)="("++show p1 ++ "<->" ++ show p2 ++ ")"
    show (p1 :->: p2)="("++show p1 ++ "->" ++ show p2 ++ ")"

 
test_ShowProp :: Bool
test_ShowProp =
    show (Not (Var "P") :&: Var "Q") == "((~P)&Q)"


type Env = [(Nume, Bool)]

impureLookup :: Eq a => a -> [(a,b)] -> b
impureLookup a = fromJust . lookup a

--ex3

eval :: Prop -> Env -> Bool
eval (Var x) env=impureLookup x env
eval T _env=True
eval F _env=False
eval (Not p) env=not(eval p env)
eval (p1 :|: p2) env=(eval p1 env) || (eval p2 env)
eval (p1 :&: p2) env=(eval p1 env) && (eval p2 env)
eval (p1 :->: p2) env=(not (eval p1 env)) || (eval p2 env) --not P1 sau P2
eval (p1 :<->: p2) env=(eval p1 env) == (eval p2 env)
 

test_eval = eval  (Var "P" :|: Var "Q") [("P", True), ("Q", False)] == True
test_eval2=eval (T :->: T) [] ==True
test_eval3=eval (T :<->: T) [] ==True

--ex4

variabile :: Prop -> [Nume]
variabile (Var x)=[x]
variabile (Not p)=nub(variabile p)
variabile (p1 :|: p2)=nub((variabile p1)++(variabile p2))
variabile (p1 :&: p2)=nub((variabile p1)++(variabile p2))
variabile (p1 :->: p2)=nub((variabile p1)++(variabile p2))
variabile (p1 :<->: p2)=nub((variabile p1)++(variabile p2))
variabile _=[]

test_variabile =
  variabile (Not (Var "P") :&: Var "Q") == ["P", "Q"]

--ex5

envs :: [Nume] -> [Env]
envs []=[[]]
envs (nume:ns)=[(nume, val) : rest | val<-[False,True], rest <- envs ns]
 
test_envs = 
    envs ["P", "Q"]
    ==
    [ [ ("P",False)
      , ("Q",False)
      ]
    , [ ("P",False)
      , ("Q",True)
      ]
    , [ ("P",True)
      , ("Q",False)
      ]
    , [ ("P",True)
      , ("Q",True)
      ]
    ]

--ex6

satisfiabila :: Prop -> Bool
satisfiabila p=or $ map (\env->eval p env) (envs (variabile p))
 
test_satisfiabila1 = satisfiabila (Not (Var "P") :&: Var "Q") == True
test_satisfiabila2 = satisfiabila (Not (Var "P") :&: Var "P") == False
test_satisfiabila3 = satisfiabila (Var "P" :->: Var "Q") == True

--o prop e valida daca negatia ei e nesatisfiabila
valida :: Prop -> Bool
valida p=and $ map (\env->eval p env) (envs (variabile p))
--valida p=satisfiabila (Not p)==False


test_valida1 = valida (Not (Var "P") :&: Var "Q") == False
test_valida2 = valida (Not (Var "P") :|: Var "P") == True


echivalenta :: Prop -> Prop -> Bool
echivalenta p1 p2 = valida (p1 :<->: p2)
 
test_echivalenta1 =
  True
  ==
  (Var "P" :&: Var "Q") `echivalenta` (Not (Not (Var "P") :|: Not (Var "Q")))
test_echivalenta2 =
  False
  ==
  (Var "P") `echivalenta` (Var "Q")
test_echivalenta3 =
  True
  ==
  (Var "R" :|: Not (Var "R")) `echivalenta` (Var "Q" :|: Not (Var "Q"))
