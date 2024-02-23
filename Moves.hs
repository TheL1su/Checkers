--Plik zawierajacy funckcje sprawdzajace czy mozna wykonac ruch wprowadzony przez uzytkownika
--AUTOR: Filip Rutka
module Moves where
import Types
import Table
import Data.Char
import Notation (singlenotation)

--Funkcja zwracajaca indeks w szachownicy zalezny od notacji
indexfromnotation :: Char -> Char -> Int
indexfromnotation letter digit = (8 - (ord(digit) - 48))*8 + ord(letter) - 65

--Funkcja sprawdzajaca ktory rodzaj ruchu zostal wykonany
whichtypeofmove :: Int -> Int -> Moves
whichtypeofmove prev curr 
    | abs(prev - curr) == 8 && (prev `mod` 8 == curr `mod` 8 ) = Single
    | abs(prev - curr) == 1 && (prev `div` 8 == curr `div` 8 ) = Single
    | abs(prev - curr) == 16 && (prev `mod` 8 == curr `mod` 8 ) = Double
    | abs(prev - curr) == 2 && (prev `div` 8 == curr `div` 8 ) = Double
    | otherwise = Unknown

--Funkcja zwracajaca element znajdujacy sie na okreslonym indeksie szachownicy
elementofboard :: [Chequer] -> Int -> Chequer
elementofboard [x] n = x
elementofboard (x:xs) 0 = x
elementofboard (x:xs) n = elementofboard xs (n-1)

--Funkcja sprawdzajaca czy na danym miejscu jest juz pionek czy nie
emptychequer :: [Chequer] -> Int -> Bool
emptychequer board index = case (elementofboard board index) of
    Empty x -> True
    Occupied x -> False

--Funkcja sprawdzajaca czy podczas skoku przeskakujemy jakis pionek
jumpoverpawn :: [Chequer] -> Int -> Int -> Bool
jumpoverpawn board prev curr = (not)(emptychequer board ((prev + curr) `div` 2))

--Funkcja sprawdzajaca czy ruch jest wykonywany prawidlowym kolorem pionka
iscolorcorrect :: Pawn -> [Chequer] -> Int -> MoveFail
iscolorcorrect pawn x index = if (Occupied pawn == element) then Success x index else Fail x "Cannot move pawn of different collor" where element = elementofboard x index

--Funkcja usuwajaca 3 pierwsze litery ze stringa
delete3 :: String -> String
delete3 (a:b:c:d) = d


--Funkcja zamieniajaca 3 literowy string na 3 chary
char3 :: String -> [Chequer] -> Pawn -> MoveFail
char3 (a:b:c:[]) board pawn = case singlenotation a b c of
    Correct int -> iscolorcorrect pawn board int
    Failure error -> Fail board "Zly kolor pionka ktorym chce sie wykonac ruch"

--Funkcja sprawdzajaca czy wykonywany ruch jest dozwolony
singlemove :: Int -> Char -> Char -> [Chequer] -> (NotationFail,Int)
singlemove prev c d board =
    let index_curr = indexfromnotation c d
        typeofmove = whichtypeofmove prev index_curr
    in case typeofmove of
        Unknown -> (Failure "Nie dozwolony ruch",0)
        Single -> do
            if emptychequer board index_curr then (Correct index_curr,1) else (Failure "Pionek juz tu jest!",1)
        Double -> do
            if emptychequer board index_curr && jumpoverpawn board prev index_curr then
                (Correct index_curr,2) else (Failure "Pionek juz tu jest lub probujesz przeskoczyc puste pole!",2)



--Funkcja mowiaca ktory kolor rusza sie nastepny
next :: Pawn -> Pawn
next Black = White
next White = Black


--Funkcja sprawdzajaca czy sekwencja ruchow jest dozwolona
aremovescorrect :: String -> Int -> Int -> [Chequer] -> MoveFail
aremovescorrect (lcurr:dcurr:sym:rest) prev bool board = 
    do 
        case (singlemove prev lcurr dcurr board,bool) of
            ((Correct curr,x),0) -> aremovescorrect rest curr x board
            ((Correct curr,2),2) -> aremovescorrect rest curr 2 board
            ((Correct curr,1),2) -> Fail board "Nie mozesz sie poruszyc o jedno pole po skoku!"
            ((Failure error,_),_) -> Fail board error
            ((Correct curr,_),1) -> Fail board "Nie mozesz w jednym ruchu przejsc dwa razy o jedno pole!"
aremovescorrect (lcurr:dcurr:[]) prev bool board = 
    do 
        case (singlemove prev lcurr dcurr board,bool) of
            ((Correct curr,x),0) -> Success board curr
            ((Correct curr,2),2) -> Success board curr
            ((Correct curr,1),2) -> Fail board "Nie mozesz sie poruszyc o jedno pole po skoku!"
            ((Failure error,_),_) -> Fail board error
            ((Correct curr,_),1) -> Fail board "Nie mozesz w jednym ruchu przejsc dwa razy o jedno pole!"
