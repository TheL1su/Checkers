--Plik zawierajacy wszystkie typy wykorzystywane w programie
--AUTOR: Filip Rutka
module Types where


-- Klasa pionków czarnych i bialych
data Pawn = Black | White

--instancja Show dla pionkow
instance Show Pawn where
    show Black = "♙"
    show White = "♟"
    
--instacja Eq dla pionkow
instance Eq Pawn where
    Black == Black = True
    White == White = True
    Black == White = False
    White == Black = False




-- Klasa pustych pol na szachownicy, B - black, W - white 
data Square = B | W 

--instancja Show dla pustych pol
instance Show Square where
    show B = "□"
    show W = "■"
--instancja Eq dla pustych pol
instance Eq Square where
    B == B = True
    W == W = True
    B == W = False
    W == B = False




-- Klasa wszystkich elementow szachownicy
data Chequer = Occupied Pawn | Empty Square

-- instacja show dla wszystkich pol szachownicy
instance Show Chequer where
    show (Occupied Black) = show Black 
    show (Occupied White) = show White
    show (Empty B) = show B
    show (Empty W) = show W

-- instacja eq dla wszystkich pol szachownicy
instance Eq Chequer where
    Occupied x == Occupied y = x == y
    Empty x == Empty y = x == y
    Occupied x == Empty y = False
    Empty x == Occupied y = False

--Klasa sukcesow i bledow wykorzystywana do wykonywania ruchow
data MoveFail = Success [Chequer] Int | Fail [Chequer] String
--Klasa suckesow i bledow wykorzystywana do sprawdzania prawidlowej notacji
data NotationFail = Correct Int | Failure String 
--Klasa wszystkich ruchow
data Moves = Single | Double | Unknown