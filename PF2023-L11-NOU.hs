newtype Identity a = Identity a
    deriving (Show, Eq)
instance Functor (Identity) where
    fmap f (Identity a) = Identity (f a)


data Pair a = Pair a a
    deriving (Show, Eq)
instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)


data Constant a b = Constant b
    deriving (Show, Eq)
instance Functor (Constant b) where
    fmap f (Constant x) = Constant (f x)


data Two a b = Two a b
    deriving (Show, Eq)

instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)


data Three a b c = Three a b c
    deriving Show

instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)


data Three' a b = Three' a b b
    deriving Show

instance Functor (Three' a) where
    fmap f (Three' x y z) = Three' x (f y) (f z)



data Four a b c d = Four a b c d
    deriving (Show)

instance Functor (Four a b c) where
    fmap f (Four x y z c) = Four x y z (f c)


data Four' a b = Four' a a a b
    deriving Show

instance Functor (Four' a) where
    fmap f (Four' x y z t) = Four' x y z (f t)


data Quant a b = Finance | Desk a | Bloor b
    deriving Show

instance Functor(Quant a) where
    fmap _ Finance = Finance
    fmap _ (Desk x) = Desk x
    fmap f (Bloor y) = Bloor (f y)


data LiftItOut f a = LiftItOut (f a)
    deriving Show

instance (Functor f) => Functor (LiftItOut f) where
    fmap f1 (LiftItOut f2) = LiftItOut (fmap f1 f2)


data Parappa f g a = DaWrappa (f a) (g a)
    deriving Show

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f1 (DaWrappa f2 f3) = DaWrappa (fmap f1 f2) (fmap f1 f3)


data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
    deriving Show

instance (Functor g) => Functor(IgnoreOne f g a) where
    fmap f1 (IgnoringSomething f2 f3) = IgnoringSomething f2 (fmap f1 f3)


data Notorious g o a t = Notorious (g o) (g a) (g t)
    deriving Show

instance (Functor g) => Functor (Notorious g o a) where
    fmap f (Notorious g1 g2 g3) = Notorious g1 g2 (fmap f g3)


data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
    deriving Show

instance Functor(GoatLord) where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat x) = OneGoat (f x)
    fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)
    

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print s a) = Print s (f a)
    fmap f (Read g) = Read (fmap f g)


